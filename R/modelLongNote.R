#' Static Model generator for Long Notes
#'
#'
require(osutools)
require(dplyr)
require(magrittr)

chart <- chartParse("../osutools_test/src/r/osu/7/Ayane - Endless Tears... (richardfeder) [CrossOver].osu")

Rcpp::sourceCpp("src/modelLongNote.cpp")
model.longNote <- function(chart){
  chart.note <- chart %>%
    dplyr::filter(.data$types == 'note') %>%
    dplyr::select(.data$keys, .data$offsets)

  chart.longNote <- chart %>%
    reshape2::dcast(offsets ~ keys,value.var = 'types', fill = NA) %>%
    .cppModelLongNote('lnoteh', 'lnotel', 'lnote', T, T) %>%
    magrittr::set_colnames(c('offsets', 1:(ncol(.) - 1))) %>%
    reshape2::melt(id.vars = 1, value.name = 'types', variable.name = 'keys') %>%
    dplyr::filter(.data$types == 'lnote') %>%
    dplyr::select(.data$offsets, .data$keys)

  chart.merge <- merge(
    chart.note, chart.longNote,
    by = 'offsets', all = T, suffixes = c('.note', '.lnote'))

  f <- chart.merge %>%
    dplyr::filter(stats::complete.cases(.))

  moves <- .dflt.move.keysets()
  moves$`7R`
    return(chart)
}
