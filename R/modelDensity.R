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
#' @param mini.ln.len.min Defines the minimum length of a Mini Long Note
#' @param mini.ln.len.max Defines the maximum length of a Mini Long Note
#' @param mini.ln.weight.min Defines the minimum weight of a Mini Long Note
#'
#' Note that the weight counts separately for the head and tail. So 0.5 will
#' treat a Long Note as 1 weight
#' @param mini.ln.weight.max Defines the maximum weight of a Mini Long Note
#'
#' Note that the weight counts separately for the head and tail. So 0.5 will
#' treat a Long Note as 1 weight
#' @param weight.note Defines the weight of a note
#' @return Returns a data.frame compatible with model.sim
#'
#' @importFrom dplyr bind_rows mutate if_else summarise group_by
#' @importFrom magrittr %<>% %>% set_colnames
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#'
#' @export

model.density <- function(chart, window = 1000,
                          mini.ln.len.min = 100,
                          mini.ln.len.max = 400,
                          mini.ln.weight.min = 0.65,
                          mini.ln.weight.max = 1,
                          weight.note = 1) {

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
      weights = dplyr::if_else(.data$weights > mini.ln.weight.max,
        mini.ln.weight.max, .data$weights),
      # Weight notes correctly
      weights = dplyr::if_else(.data$types == 'note',
                               weight.note, .data$weights))

  chart %<>%
    split(chart$types, drop = T)
  chart <- .cppModelDensity(unq.offsets, chart, window, F) %>%
    dplyr::bind_rows() %>%
    # Need to do a roundabout way to name the columns
    magrittr::set_colnames(c("offsets", types.names)) %>%
    reshape2::melt(id.vars = 1, variable.name = 'types', value.name = 'counts')

  # Summarize here
  chart %<>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(values = sum(.data$counts))

  return(chart)
}

