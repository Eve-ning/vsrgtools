#' A model that detects manipulatability in charts
#'
#' @description Manipulation is where certain notes
#' can be played in an incorrect order while still
#' maintaining a good accuracy and judgement.
#'
#' This model aims to look at surrounding notes, if
#' their placement don't vary much, this will give
#' it a higher rating.
#'
# <export here>

library(osutools)
library(ggplot2)
require(dplyr)
require(magrittr)
require(reshape2)
require(Rcpp)

model.manip <- function(chart,
                        window = 1000){
  Rcpp::sourceCpp("src/modelManipulation.cpp")
  chart <- chartParse("../osutools_test/src/r/osu/4/DJ Myosuke & Noizenecio - Architecture (Mat) [Mat's 4k DEATH].osu")

  unq.offsets <- unique(chart$offsets)
  chart %<>%
    dplyr::filter(!.data$types %in% c("lnotel")) %>%
    dplyr::mutate(types = 1) %>%
    split(x = .,f = .$keys)

  # The idea of a window is so that manipulatible notes are grouped together
  # This is also the reason why we cap the counts at 1
  chart.count <- as.data.frame(.cppModelManipulation(unq.offsets,
                                                     chart,
                                                     window = window,
                                                     is_sorted = F))
  chart.count %<>%
    magrittr::set_colnames(c("offsets", 1:(ncol(.) - 1))) %>%
    reshape2::melt(id.vars = 1, variable.name = "keys", value.name = "counts") %>%
    dplyr::group_by(.data$offsets) %>%
    dplyr::summarise(variance =  1 / (var(.data$counts) ** 2 + 1))

  require(ggplot2)
  ggplot(f) +
    aes(offsets, variance) +
    geom_smooth(span = 0.1, method = 'loess') +
    geom_line(alpha = 0.3)

}


