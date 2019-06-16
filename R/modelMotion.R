#' Static Model generator for Motion
#'
#' @description This uses diffBroadcast and createMoveMapping
#' to summarize the occurences of different patterns.
#'
#' Suppression
#'
#' Suppression works by reflecting the inverse function used
#' by diffs.inv on specified suppress.threshold value.
#'
#' Suppression for smaller values can be further controled
#' via suppress.scale. Where smaller values are suppressed
#' further with higher suppress.scale.
#'
#' @param chart.bcst The chart generated from diffBroadcast
#' @param move.mapping The data.frame generated from
#' createMoveMapping, if NA, keyset.select must be defined
#' to generate a default move.mapping
#' @param keyset.select If move.mapping is NA, this must be
#' specified to generate a default move.mapping. For valid
#' values refer to ?createMoveMapping
#' @param suppress A Logical determining if suppresion should
#' occur
#' @param suppress.threshold If suppress is True, this will
#' reflect the inverse function at this point. Creating a
#' piecewise function where the suppress function is
#' @param suppress.scale If suppress is True, this will
#' expand the x range. In other words, smaller diffs will
#' have a lower diffs.inv when this increases.
#' @param is.summarised A Logical to indicate if the chart
#' produced should be summarized by motions.
#' @param summarised.char.fun The function to use to group
#' different diffs.invs together. This function must
#' contain (diffs.invs, froms, tos) as function argument and
#' output a numeric for diffs.invs.
#'
#' Only one of the summarised.fun is needed to summarise.
#'
#' froms and tos are character types, the initials of each
#' finger then the hand number indicates the character
#'
#' E.g. M1 is Middle Finger on Hand 1 (usually left hand).
#'
#' P1, R1, M1, I1, T1, T2, I2, M2, R2, P2 for all fingers.
#'
#' If it's NA, it'll default to a quantile(diffs.invs, 0.85)
#' @param summarised.num.fun The function to use to group
#' different diffs.invs together. This function must
#' contain (diffs.invs, keys.froms, keys.tos) as function
#' argument and output a numeric for diffs.invs.
#'
#' Only one of the summarised.fun is needed to summarise.
#'
#' keys.froms and keys.tos are numeric types, they indicate
#' the column indexes
#'
#' If it's NA, it'll default to a quantile(diffs.invs, 0.85)
#' @export

model.motion <- function(chart.bcst, move.mapping = NA,
                         keyset.select = NA,
                         suppress = T,
                         suppress.threshold = 50,
                         suppress.scale = 2.0,
                         is.summarised = T,
                         summarised.char.fun = NA,
                         summarised.num.fun = NA){

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

  chart.motion <- merge(chart.bcst, move.mapping,
                        by = c('keys.froms', 'keys.tos'))

  chart.motion %<>%
    filter(keys.froms != keys.tos) %>%
    mutate(diffs.invs =
             ifelse(diffs >= suppress.threshold,
                    # Inverse Function
                    1/diffs,
                    # Suppress Function
                    1/abs(((diffs - suppress.threshold)
                          * suppress.scale)
                         - suppress.threshold)
                    ))

  if(is.summarised) {

    # Defaulted function if NA
    if(!is.na(summarised.char.fun)) {
      chart.motion %<>%
        group_by(offsets) %>%
        summarise(diffs.invs = summarised.char.fun(diffs.invs = diffs.invs,
                                                   froms = froms,
                                                   tos = tos))
    } else if (!is.na(summarised.num.fun)) {
      chart.motion %<>%
        group_by(offsets) %>%
        summarise(diffs.invs = summarised.num.fun(diffs.invs = diffs.invs,
                                              keys.froms = keys.froms,
                                              keys.tos = keys.tos))
    } else {
      chart.motion %<>%
        group_by(offsets) %>%
        summarise(diffs.invs = quantile(diffs.invs, 0.85))
    }

  }

  return(chart.motion)
}
