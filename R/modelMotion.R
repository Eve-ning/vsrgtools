#' Static Model generator for Motion
#'
#' @description This uses diffBroadcast and createMoveMapping
#' to
#' @param chart.bcst The chart generated from diffBroadcast
#' @param move.mapping The data.frame generated from
#' createMoveMapping, if NA, keyset.select must be defined
#' to generate a default move.mapping
#' @param keyset.select If move.mapping is NA, this must be
#' specified to generate a default move.mapping. For valid
#' values refer to ?createMoveMapping
#'
#' @export

model.motion <- function(chart.bcst, move.mapping = NA,
                         keyset.select = NA,
                         suppress.low.diff = T,
                         suppress.threshold = 50,
                         suppress.power = 2){

  # Preemptive check on correct arguments
  if (is.na(move.mapping) & is.na(keyset.select)) {
    stop("Either move.mapping or keyset.select must be
         defined. Read the help for more information.")
  } else if (is.na(move.mapping)) {
    move.mapping <-
      createMoveMapping(keyset.select = keyset.select)
  }

  require(magrittr)
  require(dplyr)

  chart.bcst <- chart %>% diffBroadcast()
  move.mapping <- createMoveMapping(keyset.select = '4')

  chart.motion <- merge(chart.bcst, move.mapping,
                        by = c('keys.froms', 'keys.tos'))
  return(chart.motion)
}
