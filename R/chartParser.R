#' Parses the chart into a data.frame
#'
#' Only .osu formats are supported for now.
#'
#' @param chart.path Path of the chart to be parsed
#' @param chart.lines Lines of the chart to be parsed, if the chart
#' is not in a file format
#' @return A data.frame consisting of the note's data only.
#' Columns: keys, types, offsets
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

  chart <- suppressWarnings(loadInput())

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
    chart <- extract$chart
    keys <- extract$keys

    chart <- data.frame(chart, stringsAsFactors = F)
    colnames(chart) <- "txt"

    chart$is.ln <- str_count(string = chart$txt,
                             pattern = ":") == 5

    chart %<>% separate(col=txt,
                        sep=":",
                        into=c("txt","_"),
                        extra="drop")

    chart %<>% separate(col=txt,
                        sep=",",
                        into=c("axis",".0","note",".1",".2","lnotel"))

    chart$keys = round((as.integer(chart$axis) * keys - 256) / 512) + 1
    chart %<>% na.omit()
    chart$lnotel[chart$is.ln == F] <- NA
    chart %<>% mutate_if(is.character, as.numeric)

    chart <- chart[c('note', 'lnotel', 'keys', 'is.ln')]
    chart$lnoteh[chart$is.ln] <- chart$note[chart$is.ln]
    chart$note[chart$is.ln] <- NA
    chart <- melt(chart, id.vars = 'keys',
                  na.rm = T, variable.name = 'types',
                  value.name = 'offsets')
    chart <- subset(chart, types != 'is.ln')
    return(chart)
  }

  # To add a switch/ifelse statement if more formats are done
  return(chartParseOsu(chart))
}

