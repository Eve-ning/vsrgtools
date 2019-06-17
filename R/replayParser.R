#' Parses the replay generated through the Python API downloader.
#'
#' @description Using proximity matching, data of the replay is
#' joined with the chart, so we can tell what notes are the replay
#' related to.
#'
#' @param chart A **data.frame** that needs to have types, offsets,
#' and keys
#' @param replay.path **Path** of the replay generated.
#' @param ignore.threshold **Numeric** to indicate what deviations should
#' be ignored.
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate filter bind_rows
#' @importFrom feather read_feather
#' @importFrom rlang .data
#' @export

replayParse <- function(chart, replay.path, ignore.threshold = 100){

  similarityMatch <- function(chart, replay){
    "chart should only come in keys, we will need to
    transform it into actions, which will help in
    pairing"
    chart %<>%
      dplyr::mutate(
        actions = ifelse
        (.data$types == 'lnotel', -.data$keys, .data$keys),
        replay.offsets = NA)

    actions.unq <- unique(chart$actions)

    chart.ac.split <- split(chart, f=chart$actions)
    replay.ac.split <- split(replay, f=replay$actions)

    chart.ac.list <- c()
    for (i in actions.unq) {
      chart.ac <- chart.ac.split[[as.character(i)]]
      replay.ac <- replay.ac.split[[as.character(i)]]

      for (row in 1:nrow(chart.ac)) {
        # From all offsets of the replay df, we will deduct a looped
        # chart df offset
        # We will get the minimum (which is the closest match)
        # Then we throw it into df.joined
        replay.match <- replay.ac[which.min(abs(replay.ac$offsets -
                                                chart.ac$offsets[row]))[1],]

        chart.ac[row, 'replay.offsets'] <- replay.match$offsets
      }
      chart.ac.split[[as.character(i)]] <- chart.ac
    }
    return(dplyr::bind_rows(chart.ac.split))
  }

  replay <- feather::read_feather(replay.path)
  chart %<>%
    similarityMatch(replay) %>%
    dplyr::mutate(devs = abs
                  (.data$offsets - .data$replay.offsets)
                  ) %>%
    dplyr::filter(.data$devs < ignore.threshold)

  return(chart)
}
