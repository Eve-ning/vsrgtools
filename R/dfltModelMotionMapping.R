#' Default Model Mapping for model.motion
#' @export
.dflt.mtn.mapping <- function() {
  return(
    data.frame(
      directions = c('across', 'in', 'out', 'jack'),
      weights = c(0.7, 1, 1.3, 3.0),
      stringsAsFactors = F
    )
  )
}
