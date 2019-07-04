#' Default Model Mapping for model.density
#' May be deprecated after smoothing
#' @export
.dflt.dns.mapping <- function() {
  return(
    data.frame(
      types = c('lnoteh', 'lnotel', 'note'),
      weights = rep(1,3),
      stringsAsFactors = F
    )
  )
}
