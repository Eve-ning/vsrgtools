#' Default Model Mapping for model.density
#' @export
.dflt.dns.mapping <- function() {
  return(
    data.frame(
      types = c('lnoteh', 'lnotel', 'm.lnote', 'm.lnotel', 'note'),
      weights = rep(1,5),
      stringsAsFactors = F
    )
  )
}
