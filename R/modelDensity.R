#' Static Model generator for Density
#'
#' @description Density is not meant to be a representation
#' of physical stress, instead it is for mental stress.
#'
#' In other words, the density of objects within a certain
#' time frame to be processed.
#' @param chart The chart generated from chartParse
#' @param window The window to check for objects

model.density <- function(chart, window = 1000) {

  require(magrittr)
  require(dplyr)
  require(reshape2)

  chart %<>%

    # The cast and melt is to add offsets to types that dont
    # have offsets
    dcast(offsets + keys ~ types,
          value.var = 'offsets',
          fun.aggregate = length) %>%
    melt(id.vars = 1:2,
         variable.name = "types",
         value.name = "value") %>%

    # Separate by types to do moving average
    group_by(types) %>%

    # This calculates the moving average, with a bool, but excludes
    # extrapolated values by multiplying with the value
    mutate(no.objects = sapply(offsets, function(x){
                          sum((offsets <= x & offsets >= x - window) * value)
                        })) %>%

    # Cast back to wide table
    dcast(offsets + keys ~ types,
          value.var = 'no.objects',
          fun.aggregate = first)


  return(chart)
}
