#' Calculates the difficulty
#'
#' @param osu.stress Get this data.frame from stressSim
#' @param osu.bcst Get this data.frame from diffBroadcast
#' @param bin.size Size of the bin
#' @param f.merge.key Specify the function used to merge
#' key values together
#' @param f.merge.bin Specify the function used to merge
#' bins values together
#' @export

diffCalc <- function(osu.stress,
                     osu.bcst,
                     f.merge.key,
                     f.merge.bin,
                     bin.size = 5000) {
  require(dplyr)
  require(magrittr)
  require(reshape2)

  osu.stress %<>%
    group_by(keys, offsets) %>%
    summarise(stress = f.merge.key(stress)) %>%
    mutate(bins = (offsets %/% bin.size) * bin.size) %>%
    group_by(bins) %>%
    summarise(stress = f.merge.bin(stress))

  osu.bcst %<>%
    filter(keys.froms == keys.tos) %>%
    mutate(jack.inv = 1 / diffs) %>%
    group_by(keys.froms, offsets) %>%
    summarise(jack.inv = f.merge.key(jack.inv)) %>%
    mutate(bins = (offsets %/% bin.size) * bin.size) %>%
    group_by(bins) %>%
    summarise(jack.inv = f.merge.bin(jack.inv))

  osu.j <- merge(osu.stress, osu.bcst, by = 'bins')

  return(osu.j)
}


