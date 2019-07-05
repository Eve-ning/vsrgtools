#' Static Model generator for Long Notes
#'
#'
require(osutools)
require(dplyr)
require(magrittr)

chart <- chartParse("../osutools_test/src/r/osu/7/Ayane - Endless Tears... (richardfeder) [CrossOver].osu")

model.longNote <- function(chart){
  d <- chart %>%
    dplyr::arrange(desc(.data$offsets))
  e <- split(d, d$keys)


}
