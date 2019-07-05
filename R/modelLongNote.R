#' Static Model generator for Long Notes
#'
#'
model.longNote <- function(chart,
                           chart.keyset.select = NA,
                           chart.keyset = NA,
                           directions.mapping = NA){

  # Load in parameters
  keyset <- chartFngMapping(chart.keyset.select = chart.keyset.select,
                            chart.keyset = chart.keyset)
  suppressWarnings({
    if (is.na(directions.mapping)) directions.mapping <- .dflt.mtn.mapping()})

  # Define what constitutes a "note"
  chart.note <- chart %>%
    dplyr::filter(.data$types %in% c('note', 'lnoteh')) %>%
    dplyr::select(.data$keys, .data$offsets)

  # Define what constitutes a long note body
  chart.longNote <- chart %>%
    reshape2::dcast(offsets ~ keys,value.var = 'types', fill = NA) %>%
    .cppModelLongNote('lnoteh', 'lnotel', 'lnote', T, T) %>%
    magrittr::set_colnames(c('offsets', 1:(ncol(.) - 1))) %>%
    reshape2::melt(id.vars = 1, value.name = 'types', variable.name = 'keys') %>%
    dplyr::filter(.data$types == 'lnote') %>%
    dplyr::select(.data$offsets, .data$keys)

  chart.merge <- merge(
    chart.note, chart.longNote,
    by = 'offsets', all = T, suffixes = c('.tos', '.froms'))

  chart.merge %<>%
    merge(keyset) %>%
    merge(directions.mapping, by = 'directions') %>%
    dplyr::filter(stats::complete.cases(.)) %>%
    merge(keyset) %>%
    dplyr::mutate(
      # Same as modelMotion it has a specific calcultion algo
      rfls.dist = .data$rfls * 8 + .data$distances,
      values = .data$rfls.dist * .data$weights
    ) %>%
    dplyr::group_by(offsets) %>%
    dplyr::summarise(values = sum(.data$values))

  return(chart)
}
