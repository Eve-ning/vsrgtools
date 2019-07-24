benchmark <- function(){
  require(osutools)
  require(microbenchmark)
  chart.path <- "tests/test_charts/Igorrr & Ruby My Dear - Figue Folle (Parachor) [Grotesque [Lv.18]].osu"
  keyset.select <- '4'
  chart <- chartParse(chart.path)
  chart.ext <- chartExtract(chart, keyset.select)
  microbenchmark(times = 1, unit = 'ms',
    "Parsing" = {
      chart <<- chartParse(chart.path)
    },
    "Model Density" = {
      dns <<- model.density(chart)
    },
    "Model Long Note" = {
      lng <<- model.longNote(chart, keyset.select)
    },
    "Model Motion" = {
      mtn <<- model.motion(chart.ext)
    },
    "Model Manipulation" = {
      mnp <<- model.manipulation(chart)
    },
    "Model Jack" = {
      jck <<- model.jack(chart.ext)
    },
    "Model Join" = {
      joined <<- model.join(mtn,dns,mnp,lng,jck)
    }
  )
  return(joined)
}
benchmark()
