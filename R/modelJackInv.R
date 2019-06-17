#' Static Model generator for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses
#' the difference between equal keys.froms and keys.tos
#' @param chart.bcst The chart generated from diffBroadcast
#' @export

model.jackInv <- function(chart.bcst){

  chart.bcst %<>%
    dplyr::filter(keys.froms == keys.tos) %>%
    dplyr::rename(keys = keys.froms) %>%
    dplyr::mutate(jack.invs = 1/diffs) %>%
    dplyr::select(-c(types, diffs))

  return(chart.bcst)
}
