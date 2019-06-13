#' Static Model generater for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses
#' the difference between equal keys.froms and keys.tos
#' @param chart The chart generated from chartParse
#' @param ignore.types The types of notes to be ignored
#' when calculating jacks

model.jackInv <- function(chart, ignore.types = c('lnotel')){
  require(magrittr)
  require(dplyr)
  bcst <- diffBroadcast(chart = chart,
                        ignore.types = ignore.types)

  bcst %<>%
    filter(keys.froms == keys.tos) %>%
    mutate(jack.invs = 1/diffs)

  return(bcst)
}
