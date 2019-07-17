require(osutools)
require(microbenchmark)

init <- function(){
  chart.path <<- "tests/test_charts/Igorrr & Ruby My Dear - Figue Folle (Parachor) [Grotesque [Lv.18]].osu"
  keyset.select <<- '4'
  chart <<- chartParse(chart.path)
  chart.ext <<- chartExtract(chart, keyset.select)
}
init()

microbenchmark(times = 5, unit = 'ms',
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
  }
)
