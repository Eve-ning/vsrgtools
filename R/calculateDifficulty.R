#' Processes the file into difficulty models.
#'
#' @description
#'
#' calculateDifficulty(chart.path = "../7Kdifficulty.osu",
#' chart.keyset.select = '7R')
#'
#' calculateDifficulty(chart.path = "../4Kdifficulty.osu",
#' chart.keyset.select = '4')
#'
#' @param chart.path Path to chart
#' @param chart.lines ReadLines on the path
#' @param chart.rate The rate to judge the chart on
#' @param chart.keyset.select A character vector indicating the default
#' chart.keyset to map to the keys.
#'
#' Refer to ?chartFngMapping for more details
#' @param chart.keyset A custom data.frame indicating the default chart.keyset
#' to map to the keys.
#'
#' Refer to ?chartFngMapping for more details
#' @param mtn.suppress A logical indicating if small motions should be
#' suppressed
#'
#' Refer to ?model.motion
#' @param mtn.suppress.threshold A numeric indicating the threshold of what
#' determines a "small motion".
#'
#' Refer to ?model.motion
#' @param mtn.suppress.scale A numeric indicating the power of suppression on
#' small motions
#'
#' Refer to ?model.motion
#' @param mtn.ignore.jacks A logical indicating if jacks should be ignored by
#' mtn and handled separately by JackInv
#'
#' Refer to ?model.motion
#' @param mtn.across.weight A numeric indicating the weight of the 'across'
#' motion weight
#'
#' Refer to ?model.motion
#' @param mtn.in.weight A numeric indicating the weight of the 'in' motion
#' weight
#'
#' Refer to ?model.motion
#' @param mtn.out.weight A numeric indicating the weight of the 'out' motion
#' weight
#'
#' Refer to ?model.motion
#' @param mtn.jack.weight A numeric indicating the weight of the 'jack' motion
#' weight
#'
#' Refer to ?model.motion
#' @param dns.window A numeric indicating the window to check for density
#'
#' Refer to ?model.density
#' @param dns.mini.ln.len.min Defines the minimum length of a Mini Long Note
#'
#' Refer to ?model.density
#' @param dns.mini.ln.len.max Defines the maximum length of a Mini Long Note
#'
#' Refer to ?model.density
#' @param dns.mini.ln.weight.min Defines the minimum weight of a Mini Long Note
#'
#' Note that the weight counts separately for the head and tail. So 0.5 will
#' treat a Long Note as 1 weight
#'
#' Refer to ?model.density
#' @param dns.mini.ln.weight.max Defines the maximum weight of a Mini Long Note
#'
#' Note that the weight counts separately for the head and tail. So 0.5 will
#' treat a Long Note as 1 weight
#'
#' Refer to ?model.density
#' @param mnp.window A numeric indicating the window to check for bias.
#'
#' Refer to ?model.manipulation
#' @param mnp.bias.scale A numeric indicating the intensity to scale the
#' correction
#'
#' Refer to ?model.manipulation
#' @param mnp.bias.power A numeric indicating the intensity of the correction.
#'
#' Refer to ?model.manipulation
#' @param sim.decay.ms A numeric indicating the decay of stress per ms.
#'
#' Refer to ?model.sim
#' @param sim.decay.perc.s A numeric indicating the perc decay of stress per
#' second.
#'
#' Refer to ?model.sim
#' @param sim.stress.init A numeric indicating the starting stress.
#'
#' Refer to ?model.sim
#' @param sim.mtn.pow A numeric indicating the power weight of the Motion model.
#'
#' Refer to ?model.sim
#' @param sim.dns.pow A numeric indicating the power weight of the Density
#' model.
#'
#' Refer to ?model.sim
#' @param sim.bin.size A numeric indicating the size of model.sim bin size.
#'
#' Refer to ?model.sim
#' @param sim.disable A logical indicating if stress sim should be disabled.
#' Will improve performance of calculation.
#'
#' @return Returns a list of calculated models
#'
#' sim: Simulated, model: merged model, ...
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
calculateDifficulty <- function(chart.path             = NA,
                                chart.lines            = NA,
                                chart.rate             = 1.0,
                                chart.keyset.select    = '4',
                                chart.keyset           = NA,
                                mtn.suppress           = T,
                                mtn.suppress.threshold = 50,
                                mtn.suppress.scale     = 2.0,
                                mtn.ignore.jacks       = T,
                                mtn.across.weight      = 0.9,
                                mtn.in.weight          = 1.0,
                                mtn.out.weight         = 1.4,
                                mtn.jack.weight        = 2.5,
                                dns.window             = 1000,
                                dns.mini.ln.len.min    = 100,
                                dns.mini.ln.len.max    = 400,
                                dns.mini.ln.weight.min = 0.65,
                                dns.mini.ln.weight.max = 1,
                                mnp.window             = 1000,
                                mnp.bias.scale         = 2,
                                mnp.bias.power         = 0.1,
                                sim.decay.ms           = 0,
                                sim.decay.perc.s       = 0.25,
                                sim.stress.init        = 0,
                                sim.mtn.pow            = 0.5,
                                sim.dns.pow            = 1.0,
                                sim.bin.size           = 2500,
                                sim.disable            = F) {

  chart <- chartParse(chart.path, chart.lines)

  if (chart.rate > 0){
    chart %<>%
      dplyr::mutate(offsets = .data$offsets / chart.rate)
  } else {
    stop("chart.rate must be positive")
  }
  chart.ext <- chartExtract(chart,
                            chart.keyset.select,
                            chart.keyset)

  m.mtn <- model.motion(chart.ext = chart.ext,
                        suppress = mtn.suppress,
                        suppress.threshold = mtn.suppress.threshold,
                        suppress.scale = mtn.suppress.scale,
                        directions.mapping =
                          data.frame(
                            directions = c('across', 'in', 'out', 'jack'),
                            weights = c(mtn.across.weight,
                                        mtn.in.weight,
                                        mtn.out.weight,
                                        mtn.jack.weight))
                        )
  m.dns <- model.density(chart = chart,
                         window = dns.window,
                         mini.ln.len.min = dns.mini.ln.len.min,
                         mini.ln.len.max = dns.mini.ln.len.max,
                         mini.ln.weight.min = dns.mini.ln.weight.min,
                         mini.ln.weight.max = dns.mini.ln.weight.max)

  m.mnp <- model.manipulation(chart = chart,
                              window = mnp.window,
                              bias.scale = mnp.bias.scale,
                              bias.power = mnp.bias.power)

  sim <- model.sim(m.mtn = m.mtn,
                   m.dns = m.dns,
                   m.mnp = m.mnp,
                   mtn.pow = sim.mtn.pow,
                   dns.pow = sim.dns.pow,
                   decay.ms = sim.decay.ms,
                   decay.perc.s = sim.decay.perc.s,
                   stress.init = sim.stress.init,
                   bin.size = sim.bin.size,
                   sim.disable = sim.disable)


  return(list("sim" = sim$sim,
              "model" = sim$model,
              "mtn" = m.mtn,
              "dns" = m.dns,
              "mnp" = m.mnp))
}
# require(osutools)
# e <- calculateDifficulty("../osutools_test/src/r/osu/4/DJ Myosuke & Noizenecio - Architecture (Mat) [Mat's 4k DEATH].osu",
#                          sim.disable = F,sim.decay.perc.s = 0.3,sim.decay.ms = 0)
#
# require(ggplot2)
#
# ggplot(e$sim) +
#   aes(offsets, stress) +
#   geom_smooth() +
#   geom_line(alpha = 0.2)
