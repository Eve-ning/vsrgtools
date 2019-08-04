#' Static Model generator for Motion
#'
#' @description This uses chartBroadcast and createMoveMapping to summarize the
#' occurences of different patterns. This excludes jacks.
#'
#' Suppression
#'
#' Suppression works by reflecting the inverse function used by diffs.inv on
#' specified suppress.threshold value.
#'
#' Suppression for smaller values can be further controlled via suppress.scale.
#' Where smaller values are suppressed further with higher suppress.scale.
#'
#' @param chart.ext The chart generated from chartExtract
#' @param suppress A Logical determining if suppresion should occur
#'
#' Suppression does not happen to direction 'jack'
#' @param suppress.threshold If suppress is True, this will reflect the inverse
#' function at this point. Creating a piecewise function where the suppress
#' function is
#' @param suppress.scale If suppress is True, this will expand the x range. In
#' other words, smaller diffs will have a lower diffs.inv when this increases.
#' @param directions.mapping A data.frame to be merged with the output
#' directions to generate weights.
#'
#' It must hold the columns directions and weights
#'
#' If NA, .dflt.model.motion.mapping will be used
#' @return Returns a data.frame compatible with model.sim
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr filter mutate if_else
#' @importFrom rlang .data
#' @export

model.motion <- function(chart.ext,
                         suppress = T,
                         suppress.threshold = 50,
                         suppress.scale = 2.0,
                         directions.mapping = NA){

  chart.ext %<>%
    # Drop jacks, it will be handled by a different model
    dplyr::filter(.data$directions != "jack") %>%
    # Suppress graces
    dplyr::mutate(
      diffs.invs =
        dplyr::if_else(
          # Condition
          .data$diffs >= suppress.threshold,
          # Default Inv
          1/.data$diffs,
          # Suppress Function
          1/abs((.data$diffs - suppress.threshold) * suppress.scale
                - suppress.threshold)))

  # Summarize here
  suppressWarnings({
    if (is.na(directions.mapping)) directions.mapping <- .dflt.mtn.mapping()})

  chart.ext %<>%
    merge(directions.mapping, by = 'directions') %>%
    dplyr::mutate(
      # This is a custom made variable, a combo of
      # reflections and distances
      # rfls.dist roughly represent how hard is a pair to execute
      rfls.dist = .data$rfls * 8 + .data$distances,
      values = (.data$rfls.dist * .data$weights * .data$diffs.invs) ** 2
    ) %>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(
      values = sum(.data$values)
    ) %>%
    dplyr::mutate(
      values = sqrt(values)
    )

  return(chart.ext)
}
