#' Default Motion Mapping for model.motion
#' @name .dflt.model.motion.mapping
#' @export
.dflt.model.motion.mapping <- function() {
  return(
    data.frame(
      directions = c('across', 'in', 'out'),
      weights = c(1.1, 1, 1.5)
    )
  )
}
