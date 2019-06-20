#' Default Model Mapping for model.density
#' @name .dflt.model.density.mapping
#' @export
.dflt.model.density.mapping <- function() {
  return(
    data.frame(
      types = c('lnoteh', 'lnotel', 'm.lnoteh', 'm.lnotel', 'note'),
      weights = rep(1,5)
    )
  )
}
