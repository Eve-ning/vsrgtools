#' Static Model generator for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses
#' the difference between equal keys.froms and keys.tos
#' @param chart.bcst The chart generated from diffBroadcast
#' @export

model.jackInv <- function(chart.bcst){
  
  chart.bcst %<>%
    filter(keys.froms == keys.tos) %>%
    rename(keys = keys.froms) %>%
    mutate(jack.invs = 1/diffs) %>%
    select(-c(types, diffs))

  return(chart.bcst)
}
