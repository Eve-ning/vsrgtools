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
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr filter mutate
#' @importFrom rlang .data
#' @export

model.motion <- function(chart.bcst, move.mapping = NA,
                         keyset.select = NA,
                         suppress = T,
                         suppress.threshold = 50,
                         suppress.scale = 2.0){

  # Preemptive check on correct arguments
  if (is.na(move.mapping) & is.na(keyset.select)) {
    stop("Either move.mapping or keyset.select must be
         defined. Read the help for more information.")
  } else if (is.na(move.mapping)) {
    move.mapping <-
      createMoveMapping(keyset.select = keyset.select)
  }

  chart.motion <- merge(chart.bcst, move.mapping,
                        by = c('keys.froms', 'keys.tos'))

  chart.motion %<>%
    dplyr::filter(.data$keys.froms != .data$keys.tos) %>%
    dplyr::mutate(.data$diffs.invs =
                  ifelse(.data$diffs >= suppress.threshold,
                         # Inverse Function
                         1/.data$diffs,
                         # Suppress Function
                         1/abs(((.data$diffs - suppress.threshold)
                               * suppress.scale)
                              - suppress.threshold)
                         ))

  return(chart.motion)
}
