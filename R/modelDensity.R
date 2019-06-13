#' Static Model generator for Density
#'
#' @description Density is not meant to be a representation
#' of physical stress, instead it is for mental stress.
#'
#' In other words, the density of objects within a certain
#' time frame to be processed.
#' @param chart The chart generated from chartParse
#' @param window The window to check for objects

modelDensity <- function(chart, window = 1000) {

  require(magrittr)
  require(dplyr)
  require(reshape2)

  chart %<>%
    dcast(offsets + keys ~ types,
          value.var = 'offsets',
          fun.aggregate = length) %>%
    melt(id.vars = 1:2,
         variable.name = "types",
         value.name = "value") %>%
    group_by(types) %>%
    mutate(no.objects = sapply(offsets, function(x){
                          sum((offsets <= x & offsets >= x - window) * value)
                        })) %>%
    dcast(offsets + keys ~ types,
          value.var = 'no.objects',
          fun.aggregate = first)


  return(chart)
}
