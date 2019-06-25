#' Static Model generator for Density
#'
#' @description Density is not meant to be a representation
#' of physical stress, instead it is for mental stress.
#'
#' In other words, the density of objects within a certain
#' time frame to be processed.
#' @param chart The chart generated from chartParse
#' @param window The window to check for objects
#' @param mini.ln.parse A logical indicating if miniLNs
#' should be parsed separately
#' @param mini.ln.threshold A numeric indicating the
#' threshold of LNs to be considered mini
#' @param mini.ln.tail.drop Logical indicating if the
#' miniLN Tail should be dropped.
#' @param types.mapping A data.frame to be merged with the
#' output types to generate weights.
#'
#' It must only hold the columns types and weights
#'
#' If NA, .dflt.model.density.mapping will be used
#'
#' @importFrom dplyr bind_rows mutate distinct filter
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#'
#' @export

model.density <- function(chart, window = 1000,
                          mini.ln.parse = T,
                          mini.ln.threshold = 150,
                          mini.ln.tail.drop = T,
                          types.mapping = NA) {

  if (mini.ln.parse){
    chart %<>%
      dplyr::mutate(
        types = ifelse((.data$types == 'lnoteh') & (.data$len <= mini.ln.threshold),
                       'm.lnote', .data$types),
        types = ifelse((.data$types == 'lnotel') & (.data$len <= mini.ln.threshold),
                       'm.lnotel', .data$types))
    if (mini.ln.tail.drop){
      chart %<>% dplyr::filter(.data$types != 'm.lnotel')
    }
  }

  chart %<>%
    dplyr::select(.data$keys, .data$offsets, .data$types, .data$len) %>%
    dplyr::distinct()

  unq.offsets <- unique(chart$offsets)
  chart <- split(chart, chart$types, drop = T)

  # We will use the .cpp function by types
  for (x in 1:length(chart)){
    counts <- .cppModelDensity(unq.offsets, chart[[x]]$offsets, window)
    chart[[x]] <- data.frame(
      counts = counts,
      offsets = unq.offsets,
      types = names(chart[x]),
      stringsAsFactors = F
    )
  }

  chart <- dplyr::bind_rows(chart)

  # Summarize here
  suppressWarnings({
    if (is.na(types.mapping)){
      types.mapping <- .dflt.dns.mapping()
    }
  })

  chart %<>%
    merge(types.mapping, by = 'types') %>%
    dplyr::mutate(values = .data$counts * .data$weights) %>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(values = sum(.data$values))

  return(chart)
}

