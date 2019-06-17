#' Parses the chart into a data.frame
#'
#' Only .osu formats are supported for now.
#'
#' @param chart.path Path of the chart to be parsed
#' @param chart.lines Lines of the chart to be parsed, if the chart
#' is not in a file format
#' @return A data.frame consisting of the note's data only.
#' Columns: keys, len, types, offsets
#'
#' @export

chartParse <- function(chart.path = NA,
                       chart.lines = NA){

  require(magrittr)
  loadInput <- function(){
    if (and(is.na(chart.path), is.na(chart.lines))) {
      stop("Both Arguments cannot be NA")
    } else if (is.na(chart.path)) {
      chart <- chart.lines
    } else {
      chart.f <- file(chart.path, open='r')
      chart <- readLines(chart.f)
      close(chart.f)
    }
    return(chart)
  }

  chartParseOsu <- function(chart) {
    #' Parses the osu chart into a data.frame
    #'
    #' @param chart The chart to be parsed, in a vector of characters.
    #' This can be provided via readLines function.
    #'
    require(dplyr)
    require(tidyr)
    require(stringr)
    require(reshape2)

    f.extract <- function(chart) {
      cs.i <- pmatch('CircleSize:', chart)
      keys <- as.integer(substr(chart[cs.i],
                                start = 12,
                                stop = nchar(chart[cs.i])))
      ho.i <- pmatch('[HitObjects]', chart)
      chart <- chart[ho.i+1:length(chart)]
      return(list("chart" = chart, "keys" = keys))
    }

    extract <- f.extract(chart)
    chart <- as.data.frame(nm = "raw", extract$chart, stringsAsFactors = F)
    chart.keys <- extract$keys

    chart %<>%
      mutate(is.ln = str_count(string = raw, pattern = ":") == 5) %>%
      separate(col=raw, sep=":", into=c("txt",".0"), extra="drop") %>%
      separate(col=txt, sep=",", into=c("axis",".0","note",".1",".2","lnotel")) %>%
      select(axis, note, lnotel, is.ln) %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(len = ifelse(is.ln, lnotel - note, -1),
             keys = round((axis * chart.keys - 256) / 512) + 1) %>%
      melt(id.vars = c('keys', 'len'),
           measure.vars = c('note', 'lnotel'),
           na.rm = T, variable.name = 'types',
           value.name = 'offsets')  %>%
      filter(!(len == -1 & types == 'lnotel')) %>%
      mutate(types = ifelse(len != -1 & types == 'note',
                            'lnoteh', as.character(types)))


    return(chart)
  }
  # To add a switch/ifelse statement if more formats are done

  chart <- suppressWarnings(loadInput())
  return(chartParseOsu(chart))
}
