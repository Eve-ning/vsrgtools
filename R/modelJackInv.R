#' Static Model generator for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses
#' the difference between equal keys.froms and keys.tos
#' @param chart.bcst The chart generated from diffBroadcast
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate filter select rename
#' @importFrom rlang .data
#' @export

model.jackInv <- function(chart.ext){

  chart.ext %<>%
    dplyr::filter(.data$directions == 'jack') %>%
    dplyr::rename(keys = .data$keys.froms) %>%
    dplyr::mutate(jack.invs = 1/.data$diffs) %>%
    dplyr::select(c(.data$keys, .data$offsets, .data$jack.invs))

  return(chart.ext)
}
