#' Default Model Mapping for model.motion
#' @export
.dflt.mtn.mapping <- function() {
  return(
    data.frame(
      directions = c('across', 'in', 'out'),
      weights = c(1.1, 1, 1.5)
    )
  )
}
