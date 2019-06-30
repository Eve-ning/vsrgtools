#' Static Model generator for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses the difference between
#' equal keys.froms and keys.tos
#' @param chart.ext The chart generated from diffBroadcast
#'
#' @return Returns a data.frame compatible with model.sim
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate filter select rename
#' @importFrom rlang .data
#' @export

model.jackInv <- function(chart.ext){

  chart.ext %<>%
    dplyr::filter(.data$directions == 'jack') %>%
    dplyr::rename(keys = .data$keys.froms) %>%
    dplyr::mutate(values = 1/.data$diffs)

  # Summarise here

  chart.ext %<>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(values = max(.data$values))

  return(chart.ext)
}
