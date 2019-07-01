#' Static Model generator for Density
#'
#' @description Density is not meant to be a representation of physical stress,
#' instead it is for mental stress.
#'
#' In other words, the density of objects within a certain time frame to be
#' processed.
#'
#' @param chart The chart generated from chartParse
#' @param window The window to check for objects
#' @param mini.ln.parse A logical indicating if miniLNs should be parsed
#' separately
#' @param mini.ln.threshold A numeric indicating the threshold of LNs to be
#' considered mini
#' @param mini.ln.tail.drop Logical indicating if the miniLN Tail should be
#' dropped.
#' @param types.mapping A data.frame to be merged with the output types to
#' generate weights.
#'
#' It must only hold the columns types and weights
#'
#' If NA, .dflt.model.density.mapping will be used
#'
#' @return Returns a data.frame compatible with model.sim
#'
#' @importFrom dplyr bind_rows mutate distinct filter
#' @importFrom magrittr %<>% %>% set_colnames
#' @importFrom rlang .data
#' @importFrom reshape2 melt
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
        types = ifelse(
          (.data$types == 'lnoteh') & (.data$len <= mini.ln.threshold),
          'm.lnote', .data$types),
        types = ifelse(
          (.data$types == 'lnotel') & (.data$len <= mini.ln.threshold),
          'm.lnotel', .data$types))
    if (mini.ln.tail.drop){
      chart %<>% dplyr::filter(.data$types != 'm.lnotel')
    }
  }

  unq.offsets <- unique(chart$offsets)
  # Lists are sorted alphabetically, this is required for colnames
  types.names <- sort(unique(chart$types))

  chart %<>%
    split(chart$types, drop = T)
  chart <- .cppModelDensity(unq.offsets, chart, window, F) %>%
    dplyr::bind_rows() %>%
    # Need to do a roundabout way to name the columns
    magrittr::set_colnames(c("offsets", types.names)) %>%
    reshape2::melt(id.vars = 1, variable.name = 'types', value.name = 'counts')

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

