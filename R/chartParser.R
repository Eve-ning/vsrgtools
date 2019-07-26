#' Parses the chart into a data.frame
#'
#' @description
#' Only .osu formats are supported for now.
#'
#' @param chart.path Path of the chart to be parsed
#' @param chart.lines Lines of the chart to be parsed, if the chart is not in a
#' file format
#' @param return.keys To return keys or not.
#'
#' If true, a list will be returned.
#' Chart can be accessed via "chart" and Keys can be accessed via "keys"
#'
#' If false, just the chart as DataFrame will be returned.
#'
#' @importFrom magrittr %<>% %>%
#' @importFrom dplyr mutate filter mutate_if select
#' @importFrom stringr str_count
#' @importFrom tidyr separate
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#'
#' @export

chartParse <- function(chart.path = NA,
                       chart.lines = NA,
                       return.keys = F){

  loadInput <- function(){
    if (!is.na(chart.lines)) { chart <- chart.lines }
    else if (!is.na(chart.path)) {
      chart.f <- file(chart.path, open='r')
      chart <- readLines(chart.f)
      close(chart.f)
    }
    else { stop("Both Arguments cannot be empty") }
    return(chart)
  }

  # TODO: More formats in the chartParse<filename> convention
  # All formats must return keys as keys = <keys> in a list
  chartParseOsu <- function(chart) {
    f.extract <- function(chart) {
      cs.i <- pmatch('CircleSize:', chart)
      keys <- as.integer(substr(chart[cs.i],
                                start = 12, stop = nchar(chart[cs.i])))
      ho.i <- pmatch('[HitObjects]', chart)
      chart <- chart[ho.i+1:length(chart)]
      return(list("chart" = chart, "keys" = keys))
    }

    extract <- f.extract(chart)
    chart <- as.data.frame(nm = "raw", extract$chart, stringsAsFactors = F)
    chart.keys <- extract$keys

    chart %<>%
      dplyr::mutate(is.ln = stringr::str_count(string = raw, pattern = ":") == 5) %>%

      tidyr::separate(col=.data$raw, sep=":",
                      into=c("txt",".0"), extra="drop") %>%

      tidyr::separate(col=.data$txt, sep=",",
                      into=c("axis",".0","note",".1",".2","lnotel")) %>%

      dplyr::select(.data$axis, .data$note, .data$lnotel, .data$is.ln) %>%

      dplyr::mutate_if(is.character, as.numeric) %>%

      dplyr::mutate(len = ifelse(.data$is.ln, .data$lnotel - .data$note, -1),
             keys = round((.data$axis * chart.keys - 256) / 512) + 1) %>%

      reshape2::melt(id.vars = c('keys', 'len'),
                     measure.vars = c('note', 'lnotel'),
                     na.rm = T, variable.name = 'types',
                     value.name = 'offsets')  %>%

      dplyr::filter(!(.data$len == -1 & .data$types == 'lnotel')) %>%

      dplyr::mutate(types = ifelse(
                      .data$len != -1 & .data$types == 'note',
                      'lnoteh', as.character(.data$types)))


    return(list("chart" = chart, "keys" = chart.keys))
  }
  # To add a switch/ifelse statement if more formats are done

  chart <- suppressWarnings(loadInput())
  chart <- chartParseOsu(chart)

  if (return.keys){
    return(list("chart" = chart$chart, "keys" = chart$keys))
  } else {
    return(chart$chart)
  }
}
