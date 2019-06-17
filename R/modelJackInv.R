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

model.jackInv <- function(chart.bcst){

  chart.bcst %<>%
    dplyr::filter(.data$keys.froms == .data$keys.tos) %>%
    dplyr::rename(keys = .data$keys.froms) %>%
    dplyr::mutate(jack.invs = 1/.data$diffs) %>%
    dplyr::select(-c(.data$types, .data$diffs))

  return(chart.bcst)
}
