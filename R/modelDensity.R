#' Static Model generator for Density
#'
#' @description Density is not meant to be a representation
#' of physical stress, instead it is for mental stress.
#'
#' In other words, the density of objects within a certain
#' time frame to be processed.
#' @param chart The chart generated from chartParse
#' @param window The window to check for objects
#' @export

model.density <- function(chart, window = 1000) {

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

  chart <- dplyr::bind_rows(chart)
  return(chart)
}
