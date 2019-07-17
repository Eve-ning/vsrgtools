#' Default Move Keyset for chartFngMapping
#' @export
.dflt.move.keysets <- function() {
  return(list(
    '4' = data.frame(keys = 1:4,
                     fingers = c(3,4,7,8), stringsAsFactors = F),
    '5R' = data.frame(keys = 1:5,
                      fingers = c(3,4,6,7,8), stringsAsFactors = F),
    '5L' = data.frame(keys = 1:5,
                      fingers = c(3,4,5,7,8), stringsAsFactors = F),
    '6' = data.frame(keys = 1:6,
                     fingers = c(2,3,4,7,8,9), stringsAsFactors = F),
    '7R' = data.frame(keys = 1:7,
                      fingers = c(2,3,4,6,7,8,9), stringsAsFactors = F),
    '7L' = data.frame(keys = 1:7,
                      fingers = c(2,3,4,5,7,8,9), stringsAsFactors = F),
    '8SPL' = data.frame(keys = 1:8,
                        fingers = c(1,2,3,4,6,7,8,9), stringsAsFactors = F),
    '8SPR' = data.frame(keys = 1:8,
                        fingers = c(2,3,4,5,7,8,9,10), stringsAsFactors = F),
    '8SYM' = data.frame(keys = 1:8,
                        fingers = c(2,3,4,5,6,7,8,9), stringsAsFactors = F),
    '9R' = data.frame(keys = 1:9,
                      fingers = c(1,2,3,4,6,7,8,9,10), stringsAsFactors = F),
    '9L' = data.frame(keys = 1:9,
                      fingers = c(1,2,3,4,5,7,8,9,10), stringsAsFactors = F)
  ))
}
