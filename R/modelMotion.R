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
#' @param chart.ext The chart generated from chartExtract
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

model.motion <- function(chart.ext,
                         suppress = T,
                         suppress.threshold = 50,
                         suppress.scale = 2.0){

  chart.ext %<>%
    dplyr::filter(.data$directions != 'jack') %>%
    dplyr::mutate(diffs.invs =
                  ifelse(.data$diffs >= suppress.threshold,
                         # Inverse Function
                         1/.data$diffs,
                         # Suppress Function
                         1/abs(((.data$diffs - suppress.threshold)
                               * suppress.scale)
                              - suppress.threshold)
                         ))

  return(chart.ext)
}
