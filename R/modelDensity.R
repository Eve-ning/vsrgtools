#' Static Model generator for Density
#'
#' @description Density is not meant to be a representation
#' of physical stress, instead it is for mental stress.
#'
#' In other words, the density of objects within a certain
#' time frame to be processed.
#' @param chart The chart generated from chartParse
#' @param window The window to check for objects
#' @param is.summarised A Logical to indicate if the chart
#' produced should be summarized by types.
#' @param summarised.fun The function to use to group
#' different types of objects together. This function must
#' contain (counts, types) as function arguments and output
#' a numeric for counts.
#'
#' Note that types are character types, please use the
#' names given through chartParse for conditionals
#'
#' If it's NA, it'll default to a normal sum of all objects
#' @export

model.density <- function(chart, window = 1000,
                          is.summarised = T,
                          summarised.fun = NA) {
  require(dplyr)

  unq_offsets <- unique(chart$offsets)
  chart <- split(chart, chart$types,drop = T)

  # We will use the .cpp function by types
  for (x in 1:length(chart)){
    counts <- .cppModelDensity(unq_offsets, chart[[x]]$offsets, window)
    chart[[x]] <- data.frame(
      counts = counts,
      offsets = unq_offsets,
      types = names(chart[x]),
      stringsAsFactors = F
    )
  }

  chart <- bind_rows(chart)

  if(is.summarised) {
    require(magrittr)
    require(dplyr)

    # Defaulted function if NA
    if(is.na(summarised.fun)) {
      summarised.fun <- function(counts, types) {
        return(sum(counts))
      }
    }

    chart %<>%
      group_by(offsets) %>%
      summarise(counts = summarised.fun(counts = counts,
                                        types = types))
  }

  return(chart)
}
