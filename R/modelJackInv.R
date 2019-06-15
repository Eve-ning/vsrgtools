#' Static Model generator for Jack Inverse
#'
#' @description This uses diffBroadcast and just inverses
#' the difference between equal keys.froms and keys.tos
#' @param chart.bcst The chart generated from diffBroadcast
#' @param is.summarised A Logical to indicate if the chart
#' produced should be summarized by keys.
#' @param summarised.fun The function to use to group
#' different jack.invs together. This function must
#' contain (jack.invs) as function argument and output
#' a numeric for jack.invs.
#'
#' If it's NA, it'll default to a quantile(jack.invs, 0.85)
#'
#' @export

model.jackInv <- function(chart.bcst,
                          is.summarised = T,
                          summarised.fun = NA){
  require(magrittr)
  require(dplyr)

  chart.bcst %<>%
    filter(keys.froms == keys.tos) %>%
    rename(keys = keys.froms) %>%
    mutate(jack.invs = 1/diffs) %>%
    select(-c(types, diffs))

  # We summarize the values here
  if (is.summarised){

    # Defaulted function if NA
    if(is.na(summarised.fun)) {
      summarised.fun <- function(jack.invs) {
        return(quantile(jack.invs, 0.85))
      }
    }

    chart.bcst %<>%
      group_by(offsets) %>%
      summarise(jack.invs = summarised.fun(jack.invs = jack.invs))
  }

  return(chart.bcst)
}
