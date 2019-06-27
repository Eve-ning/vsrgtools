#' Calculates difficulty using stressSim
#'
#' @param chart.path Path to chart
#' @param chart.lines ReadLines on the path
#' @param chart.rate The rate to judge the chart on
#' @param keyset.select A character vector indicating
#' the default keyset to map to the keys
#' Refer to ?chartFngMapping for more details
#' @param keyset A custom data.frame indicating the
#' default keyset to map to the keys
#' Refer to ?chartFngMapping for more details
#' @param mtn.suppress A logical indicating if small
#' motions should be suppressed
#' Refer to ?model.motion
#' @param mtn.suppress.threshold A numeric indicating
#' the threshold of what determines a "small motion"
#' Refer to ?model.motion
#' @param mtn.suppress.scale A numeric indicating the
#' power of suppression on small motions
#' Refer to ?model.motion
#' @param mtn.ignore.jacks A logical indicating if jacks
#' should be ignored by mtn and handled separately by
#' JackInv
#' Refer to ?model.motion
#' @param mtn.across.weight A numeric indicating the
#' weight of the 'across' motion weight
#' Refer to ?model.motion
#' @param mtn.in.weight A numeric indicating the
#' weight of the 'in' motion weight
#' Refer to ?model.motion
#' @param mtn.out.weight A numeric indicating the
#' weight of the 'out' motion weight
#' Refer to ?model.motion
#' @param mtn.jack.weight A numeric indicating the
#' weight of the 'jack' motion weight
#' Refer to ?model.motion
#' @param dns.window A numeric indicating the window to
#' check for density
#' Refer to ?model.density
#' @param dns.mini.ln.parse A logical indicating if mini
#' Long Note should be parsed
#' Refer to ?model.density
#' @param dns.mini.ln.threshold A numeric indicating the
#' threshold of what determines a "mini LN"
#' Refer to ?model.density
#' @param dns.mini.ln.tail.drop A logical indicating if
#' mini LNs should be dropped.
#'
#' It is recommended to drop mini LNs for density
#' calculation
#'
#' Refer to ?model.density
#' @param dns.note.weight A numeric indicating the
#' weight of the 'note' density weight
#' Refer to ?model.density
#' @param dns.lnoteh.weight A numeric indicating the
#' weight of the 'lnoteh' density weight
#' Refer to ?model.density
#' @param dns.lnotel.weight A numeric indicating the
#' weight of the 'lnotel' density weight
#' Refer to ?model.density
#' @param dns.m.lnoteh.weight A numeric indicating the
#' weight of the 'm.lnoteh' density weight
#' Refer to ?model.density
#' @param dns.m.lnotel.weight A numeric indicating the
#' weight of the 'm.lnotel' density weight
#' Refer to ?model.density
#' @param decay.ms A numeric indicating the decay of
#' stress per ms
#' Refer to ?model.sim
#' @param stress.init A numeric indicating the starting
#' stress
#' Refer to ?model.sim
#' @param jck.pow A numeric indicating the power weight of the
#' Jack model
#' Refer to ?model.sim
#' @param mtn.pow A numeric indicating the power weight of the
#' Motion model
#' Refer to ?model.sim
#' @param dns.pow A numeric indicating the power weight of the
#' Density model
#' Refer to ?model.sim
#'
#' @param sim.bin.size A numeric indicating the size of
#' model.sim bin.size
#' Refer to ?model.sim
#'
#' @return Returns a list of calculated models
#'
#' sim: Simulated, model: merged model, ...
#' @examples
#' calculateDifficulty(chart.path = "../7Kdifficulty.osu",
#' keyset.select = '7R')
#'
#' calculateDifficulty(chart.path = "../4Kdifficulty.osu",
#' keyset.select = '4')
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
calculateDifficulty <- function(chart.path = NA,
                                chart.lines = NA,
                                chart.rate = 1.0,
                                keyset.select = '4',
                                keyset = NA,
                                mtn.suppress = T,
                                mtn.suppress.threshold = 50,
                                mtn.suppress.scale = 2.0,
                                mtn.ignore.jacks = T,
                                mtn.across.weight = 0.7,
                                mtn.in.weight = 1.0,
                                mtn.out.weight = 1.3,
                                mtn.jack.weight = 3.0,
                                dns.window = 1000,
                                dns.mini.ln.parse = T,
                                dns.mini.ln.threshold = 150,
                                dns.mini.ln.tail.drop = T,
                                dns.note.weight = 1,
                                dns.lnoteh.weight = 1,
                                dns.lnotel.weight = 1,
                                dns.m.lnote.weight = 1,
                                dns.m.lnotel.weight = 1,
                                decay.ms = 0.5,
                                stress.init = 0,
                                jck.pow = 1.0,
                                mtn.pow = 1.0,
                                dns.pow = 1.0,
                                sim.bin.size = 5000) {
  chart <- chartParse(chart.path = chart.path,
                      chart.lines = chart.lines)

  if (chart.rate > 0){
    chart %<>%
      dplyr::mutate(offsets = .data$offsets / chart.rate)
  } else {
    stop("chart.rate must be positive")
  }
  chart.ext <- chartExtract(chart,
                            keyset.select = keyset.select,
                            keyset = keyset)

  m.jck <- model.jackInv(chart.ext)

  m.mtn <- model.motion(chart.ext,
                        suppress = mtn.suppress,
                        suppress.threshold = mtn.suppress.threshold,
                        suppress.scale = mtn.suppress.scale,
                        ignore.jacks = mtn.ignore.jacks,
                        directions.mapping =
                          data.frame(
                            directions = c('across', 'in', 'out', 'jack'),
                            weights = c(mtn.across.weight,
                                        mtn.in.weight,
                                        mtn.out.weight,
                                        mtn.jack.weight))
                        )
  m.dns <- model.density(chart,
                         window = dns.window,
                         mini.ln.parse = dns.mini.ln.parse,
                         mini.ln.threshold = dns.mini.ln.threshold,
                         mini.ln.tail.drop = dns.mini.ln.tail.drop,
                         types.mapping =
                           data.frame(
                             types = c('note', 'lnoteh', 'lnotel', 'm.lnote', 'm.lnotel'),
                             weights = c(dns.note.weight,
                                         dns.lnoteh.weight,
                                         dns.lnotel.weight,
                                         dns.m.lnote.weight,
                                         dns.m.lnotel.weight)
                           )
                         )

  sim <- model.sim(m.jck,
                   m.mtn,
                   m.dns,
                   decay.ms = decay.ms,
                   stress.init = stress.init,
                   jck.pow = jck.pow,
                   mtn.pow = mtn.pow,
                   dns.pow = dns.pow,
                   bin.size = sim.bin.size)

  return(list("sim" = sim$sim,
              "model" = sim$model,
              "jck" = m.jck,
              "mtn" = m.mtn,
              "dns" = m.dns))
}
