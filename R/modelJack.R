#' Static Model generator for Jack
#'
#' @description This uses chartBroadcast and just inverses the difference between
#' equal keys.froms and keys.tos
#' @param chart.ext The chart generated from chartBroadcast
#'
#' @return Returns a data.frame compatible with model.sim
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate filter rename group_by summarise
#' @importFrom rlang .data
#' @export

model.jack <- function(chart.ext){

  chart.ext %<>%
    dplyr::filter(.data$directions == 'jack') %>%
    dplyr::rename(keys = .data$keys.froms) %>%
    dplyr::mutate(values = 1/.data$diffs) %>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(values = max(.data$values) + mean(.data$values))

  return(chart.ext)
}
