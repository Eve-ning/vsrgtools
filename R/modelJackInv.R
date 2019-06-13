#' Static Model generator for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses
#' the difference between equal keys.froms and keys.tos
#' @param chart.bcst The chart generated from diffBroadcast
#' @param ignore.types The types of notes to be ignored
#' when calculating jacks

model.jackInv <- function(chart.bcst){
  require(magrittr)
  require(dplyr)

  chart.bcst %<>%
    filter(keys.froms == keys.tos) %>%
    rename(keys = keys.froms) %>%
    mutate(jack.invs = 1/diffs) %>%
    select(-c(keys.froms, diffs))

  return(bcst)
}
