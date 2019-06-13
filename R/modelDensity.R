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

  chart <- chartParse("../requests/src/reqs/fletch_04062019/Hyper Potions - Jungle Cruise (Theresa May) [Stage 2 - Excitement].osu")
  chart %>%
    arrange(offsets) %>%
    mutate(no.objects = sapply(.$offsets, function(x){
      sum(.$offsets <= x & .$offsets >= x - window)
    })) %>%
    group_by(offsets) %>%
    summarise(no.objects = first(no.objects))

  return(chart)
}
