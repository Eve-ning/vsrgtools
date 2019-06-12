#' Calculates the difficulty
#'
#' @param osu.stress Get this data.frame from stressSim
#' @param osu.bcst Get this data.frame from diffBroadcast
#' @param bin.size Size of the bin
#' @param quant.threshold For Stress and BCst, at what
#' quantile should the value be grabbed from
#' @param alpha.threshold For the difficulty, what quantile
#' should the value be grabbed from
#'
#' @export

diffCalc <- function(osu.stress,
                     osu.bcst,
                     bin.size = 5000,
                     quant.threshold = 0.95,
                     alpha.threshold = 0.95) {

  require(dplyr)
  require(magrittr)
  require(reshape2)

  osu.stress %<>%
    group_by(keys, offsets) %>%
    summarise(stress = quantile(stress, quant.threshold)) %>%
    mutate(bins = (offsets %/% bin.size) * bin.size) %>%
    group_by(bins) %>%
    summarise(stress = mean(stress))

  osu.bcst %<>%
    filter(keys.froms == keys.tos) %>%
    mutate(jack.inv = 1 / diffs) %>%
    group_by(keys.froms, offsets) %>%
    summarise(jack.inv = quantile(jack.inv, quant.threshold)) %>%
    mutate(bins = (offsets %/% bin.size) * bin.size) %>%
    group_by(bins) %>%
    summarise(jack.inv = mean(jack.inv))

  osu.j <- merge(osu.stress, osu.bcst, by = 'bins')

  # Attempt on diff calc
  alpha <- quantile(osu.j$stress, alpha.threshold) *
    quantile(osu.j$jack.inv, alpha.threshold)

  return(list("graph" = osu.j, "alpha" = alpha))
}


