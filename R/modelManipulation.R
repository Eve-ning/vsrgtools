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

model.manip <- function(){
  chart <- chartParse("../osutools_test/src/r/osu/4/Camellia - Bassline Yatteru w (Lude) [w].osu")

  Rcpp::sourceCpp("src/modelManipulation.cpp")
  unq.offsets <- unique(chart$offsets)
  d <- chart %>%
    filter(!types %in% c("lnotel")) %>%
    mutate(types = 1) %>%
    split(x = .,f = .$keys)


  e <- cppModelManipulation(unq.offsets, d)
  colnames(e) <- 's'
    # mutate(bins = (offsets %/% 250) * 250) %>%
    # select(-offsets) %>%
    # group_by(bins) %>%
    # summarize_all(sum) %>%
    # melt(id.vars = 1, variable.name = "keys") %>%
    # group_by(bins) %>%
    # summarize(manips = var(value))

  osutools::.cppModelDensity
  ggplot(d) +
    aes(bins, manips) +
    geom_smooth(span = 0.15, se = F) +
    geom_smooth(span = 0.15, method = 'loess', data = j$model, aes(bins, values/50), se=F,
                color = 'red')

}


