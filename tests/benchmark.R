benchmark <- function(chart.path){
  require(osutools)
  require(microbenchmark)
  keyset.select <- '4'
  chart <- chartParse(chart.path)
  chart.ext <- chartExtract(chart, keyset.select)
  chart <- chartParse(chart.path)

  # Initialize
  dns <- model.density(chart)
  lng <- model.longNote(chart, keyset.select)
  mtn <- model.motion(chart.ext)
  mnp <- model.manipulation(chart)
  jck <- model.jack(chart.ext)
  joined <- model.join(mtn,dns,mnp,lng,jck)

  bench <- microbenchmark(times = 1, unit = 'ms',
    "Parsing" = {
      chart <- chartParse(chart.path)
    },
    "Model Density" = {
      dns <- model.density(chart)
    },
    "Model Long Note" = {
      lng <- model.longNote(chart, keyset.select)
    },
    "Model Motion" = {
      mtn <- model.motion(chart.ext)
    },
    "Model Manipulation" = {
      mnp <- model.manipulation(chart)
    },
    "Model Jack" = {
      jck <- model.jack(chart.ext)
    },
    "Model Join" = {
      joined <- model.join(mtn,dns,mnp,lng,jck)
    }
  )
  print(bench)
  return(joined)
}
# TR <- benchmark("tests/test_charts/typeMARS - Triumph & Regret ([ A v a l o n ]) [Regret].osu")
# OOB <- benchmark("tests/test_charts/Betwixt & Between - out of Blue (Shoegazer) [Abyss].osu")
#
# TR$label <- 'Triumph & Regret'
# OOB$label <- 'Out of Blue'
#
# merged <- rbind(TR, OOB)
#
# require(ggplot2)
# require(dplyr)
# require(magrittr)
# require(reshape2)
# require(ggdark)
#
# t <- merged %>%
#   mutate(
#     lng.vals = scale(lng.vals),
#     dns.vals = scale(dns.vals),
#     mtn.vals = scale(mtn.vals),
#     mnp.vals = scale(mnp.vals),
#     jck.vals = scale(jck.vals)
#   ) %>%
#   select(-lng.vals) %>%
#   melt(measure.vars = 2:5)
#
# ggplot(t) +
#   aes(offsets, value,
#       group = variable,
#       color = variable) +
#   facet_wrap(. ~ label, scales = 'free_x') +
#   geom_smooth(se = F, method = 'loess', span = 0.2) +
#   dark_theme_gray()
#
# # ---
#
# AA <- benchmark("tests/test_charts/Yuyoyuppe - AiAe (Fullerene-) [Wafles' SHD].osu")
# EMP <- benchmark("tests/test_charts/UNDEAD CORPORATION - The Empress scream off ver (TheZiemniax) [Zenx's SHD].osu")
#
# AA$label <- 'AiAe'
# EMP$label <- 'The Empress'
#
# merged <- rbind(AA, EMP)
#
# require(ggplot2)
# require(dplyr)
# require(magrittr)
# require(reshape2)
# require(ggdark)
#
# t <- merged %>%
#   mutate(
#     lng.vals = scale(lng.vals),
#     dns.vals = scale(dns.vals),
#     mtn.vals = scale(mtn.vals),
#     mnp.vals = scale(mnp.vals),
#     jck.vals = scale(jck.vals)
#   ) %>%
#   select(-lng.vals) %>%
#   melt(measure.vars = 2:5)
#
# ggplot(t) +
#   aes(offsets, value,
#       group = variable,
#       color = variable) +
#   facet_wrap(. ~ label, scales = 'free_x') +
#   geom_smooth(se = F, method = 'loess', span = 0.2) +
#   dark_theme_gray()
