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
                          mini.ln.len.min = 100,
                          mini.ln.len.max = 400,
                          mini.ln.weight.min = 0.65,
                          mini.ln.weight.max = 1,
                          mini.ln.tail.drop = T,
                          types.mapping = NA) {

  # chart <- chartParse("../osutools_test/src/r/osu/4/Betwixt & Between - out of Blue (Shoegazer) [Abyss].osu")
  #
  # Rcpp::sourceCpp("src/modelDensity.cpp")
  unq.offsets <- unique(chart$offsets)
  # Lists are sorted alphabetically, this is required for colnames
  types.names <- sort(unique(chart$types))

  mini.ln.len.width <- mini.ln.len.max - mini.ln.len.min
  mini.ln.weight.width <- mini.ln.weight.max - mini.ln.weight.min

  chart %<>%
    dplyr::mutate(
      weights = (.data$len - mini.ln.len.min) / mini.ln.len.width,
      weights = (.data$weights * mini.ln.weight.width) + mini.ln.weight.min,
      # Crop min
      weights = dplyr::if_else(.data$weights < mini.ln.weight.min,
        mini.ln.weight.min, .data$weights),
      # Crop max
      weights = dplyr::if_else(weights > mini.ln.weight.max,
        mini.ln.weight.max, .data$weights),
      # Weight notes correctly
      weights = dplyr::if_else(.data$types == 'note',
        1, .data$weights))

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

