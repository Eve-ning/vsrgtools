#' Static Model generator for Long Notes
#'
#' @description
#' Note that we define difficulty by the Long Note to Note Movement, so the
#' froms are Long Notes, tos are Notes.
#'
#' In other words, if you're holding X, we define difficulty for a Y as
#' X -> Y akin to model.motion.
#'
#' @param chart The chart generated from chartParse
#' @param chart.keyset.select Read ?createMoveMapping for more details
#' @param chart.keyset Read ?createMoveMapping for more details
#' @param directions.mapping A data.frame to be merged with the output
#' directions to generate weights.
#'
#' It must hold the columns directions and weights
#'
#' If NA, .dflt.model.motion.mapping will be used
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr filter select mutate group_by summarise first
#' @importFrom reshape2 melt
#' @importFrom stats complete.cases
#' @importFrom rlang .data
#'
#' @export

model.longNote <- function(chart,
                           chart.keyset.select = NA,
                           chart.keyset = NA,
                           directions.mapping = NA,
                           scale = 1){

  # Load in parameters
  keyset <- chartFngMapping(chart.keyset.select = chart.keyset.select,
                            chart.keyset = chart.keyset)
  suppressWarnings({
    if (is.na(directions.mapping)) directions.mapping <- .dflt.mtn.mapping()})

  # Define what constitutes a "note"
  chart.note <- chart %>%
    dplyr::select(.data$keys, .data$offsets)

  # Define what constitutes a long note body
  chart.longNote <- chart %>%
    reshape2::dcast(offsets ~ keys, value.var = 'types', fun.aggregate = dplyr::first) %>%
    .cppModelLongNote('lnoteh', 'lnotel', 'lnote', F, F) %>%
    dplyr::bind_rows()

  chart.longNote %<>% # Required to split since ncol(.) doesn't support piping
    magrittr::set_colnames(c('offsets', 1:(ncol(chart.longNote) - 1))) %>%
    reshape2::melt(id.vars = 1, value.name = 'types', variable.name = 'keys') %>%
    dplyr::filter(.data$types == 'lnote') %>%
    dplyr::select(.data$offsets, .data$keys)

  # Merge by crossjoin
  chart.merge <- merge(
    chart.note, chart.longNote,
    by = 'offsets', all = T, suffixes = c('.tos', '.froms'))

  keys <- max(chart$keys)

  # Set the values of all of the moves
  chart.merge %<>%
    merge(keyset) %>%
    merge(directions.mapping, by = 'directions') %>%
    dplyr::mutate(
      # Same as modelMotion it has a specific calcultion algo
      # TODO We need to tweak this
      rfls.dist = .data$rfls * 8 + .data$distances,
      values = .data$rfls.dist * .data$weights
    ) %>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(values = max(.data$values) + mean(.data$values)) %>%
    # dplyr::group_by(.data$offsets) %>%
    # dplyr::summarise(values = sum(.data$values)) %>%
    dplyr::mutate(values = (.data$values /(scale * keys)))
  return(chart.merge)
}
