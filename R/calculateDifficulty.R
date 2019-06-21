#' Calculates difficulty using stressSim
#'
#' @export
calculateDifficulty <- function(chart.path = NA,
                                chart.lines = NA,
                                keyset.select = '4',
                                keyset = NA,
                                mtn.suppress = T,
                                mtn.suppress.scale = 2.0,
                                mtn.suppress.threshold = 50,
                                mtn.across.weight = 0.7,
                                mtn.in.weight = 1.0,
                                mtn.out.weight = 1.3,
                                mtn.jack.weight = 3.0,
                                dns.window = 1000,
                                dns.mini.ln.parse = T,
                                dns.mini.ln.threshold = 150,
                                dns.mini.ln.tail.drop = T,
                                dns.note.weight = 1,
                                dns.lnotel.weight = 1,
                                dns.lnoteh.weight = 1,
                                dns.m.lnoteh.weight = 1,
                                dns.m.lnotel.weight = 1,
                                decay.ms = 0.5,
                                stress.init = 0) {
  chart <- chartParse(chart.path = chart.path,
                      chart.lines = chart.lines)

  chart.ext <- chartExtract(chart,
                            keyset.select = keyset.select,
                            keyset = keyset)

  m.jck <- model.jackInv(chart.ext)

  m.mtn <- model.motion(chart.ext,
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
  m.dns <- model.density(chart.ext,
                         window = dns.window,
                         mini.ln.parse = dns.mini.ln.parse,
                         mini.ln.threshold = dns.mini.ln.threshold,
                         mini.ln.tail.drop = dns.mini.ln.tail.drop,
                         types.mapping =
                           data.frame(
                             types = c('note', 'lnoteh', 'lnotel', 'm.lnoteh', 'm.lnotel'),
                             weights = c(dns.note.weight,
                                         dns.lnoteh.weight,
                                         dns.lnotel.weight,
                                         dns.m.lnoteh.weight,
                                         dns.m.lnotel.weight)
                           )
                         )

  sim <- model.sim(m.jck,
                   m.mtn,
                   m.dns,
                   decay.ms = decay.ms,
                   stress.init = stress.init)


}
