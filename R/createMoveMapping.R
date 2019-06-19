#' Creates the mapping to be used alongside with
#' the broadcasted chart.
#'
#' @description
#' Using the broadcasted format, this function maps
#' the values provided by `mapping`
#'
#' This can also be skipped if you're doing a custom
#' mapping
#'
#' @param keyset.select A character vector based on
#' type of keyset.
#'
#' Either keyset or keyset.select must be defined
#'
#' Valid Keyset.select values are
#'
#' '4', '5L', '5R', '6', '7L', '7R',
#' '8SPR', '8SPL','8SYM', '9L', '9R'
#'
#' Only 8K has special keysets
#'
#' 8SYM: 8 Key Symmetry
#'
#' 8SPL: 8 Key Special (Left)
#'
#' 8SPR: 8 Key Special (Right)
#'
#' @param keyset A data.frame based on type of keyset.
#'
#' Either keyset or keyset.select must be defined
#'
#' Fingers are denoted as
#' P|inky
#' R|ing
#' M|iddle
#' I|ndex
#' T|humb
#'
#' Column keys and fingers is required.
#'
#' An example of keyset would be
#'
#' 7R = data.frame(keys = 1:7,
#'                 fingers = c(2,3,4,6,7,8,9))
#'
#' @param include.details A Logical to indicate if
#' details should be calculated
#'
#' Direction
#'
#' Direction is a character vector indicating what
#' type of motion it is.
#'
#' "across" means moves from hand to hand
#'
#' "jack" is self-explanatory
#'
#' "in" is where the motion is leading towards the
#' thumb
#'
#' "out" is where the motion is leading to the pinkies
#'
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate select
#' @importFrom stringr str_replace
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#'
#' @export

createMoveMapping <- function(keyset.select=NA,
                              keyset=NA,
                              include.details=T){

  loadMapping <- function(mapping){
    fngr <- merge(1:10, 1:10)
    colnames(fngr) <- c("fngs.tos", "fngs.froms")
    return(fngr)
  }

  # Chooses between keyset or keyset.select, if both NA, stop()
  loadKeyset <- function(keyset, keyset.select){
    if (!is.na(keyset)){
      return(keyset)
    } else if (!is.na(keyset.select)){
    move.keysets = list(
      '4' = data.frame(keys = 1:4,
                       fingers = c(3,4,7,8)),
      '5R' = data.frame(keys = 1:5,
                        fingers = c(3,4,6,7,8)),
      '5L' = data.frame(keys = 1:5,
                        fingers = c(3,4,5,7,8)),
      '6' = data.frame(keys = 1:6,
                       fingers = c(2,3,4,7,8,9)),
      '7R' = data.frame(keys = 1:7,
                        fingers = c(2,3,4,6,7,8,9)),
      '7L' = data.frame(keys = 1:7,
                        fingers = c(2,3,4,5,7,8,9)),
      '8SPL' = data.frame(keys = 1:8,
                          fingers = c(1,2,3,4,6,7,8,9)),
      '8SPR' = data.frame(keys = 1:8,
                          fingers = c(2,3,4,5,7,8,9,10)),
      '8SYM' = data.frame(keys = 1:8,
                          fingers = c(2,3,4,5,6,7,8,9)),
      '9R' = data.frame(keys = 1:9,
                        fingers = c(1,2,3,4,6,7,8,9,10)),
      '9L' = data.frame(keys = 1:9,
                        fingers = c(1,2,3,4,5,7,8,9,10))
    )
    return(move.keysets[[keyset.select]])
    } else {
    stop("Either keyset or keyset.select must be defined.")
    }
  }

  # Merges both data.frames together
  mergeMapping <- function(mapping, keyset) {
    mapping %<>%
      merge(keyset,
            by.x = 'fngs.froms',
            by.y = 'fingers')
    colnames(mapping)[ncol(mapping)] <- "keys.froms"
    mapping %<>%
      merge(keyset,
            by.x = 'fngs.tos',
            by.y = 'fingers')
    colnames(mapping)[ncol(mapping)] <- "keys.tos"

    # We will return all due to move naming, which is easier
    # to work with with finger chars
    # mapping <- mapping[3:5] # Return only the required columns.
    return(mapping)
  }

  getDetails <- function(mapping) {
    mapping %<>%
      dplyr::mutate(
        direction = 'in',
        disparity = abs(keys.tos - keys.froms),

        fngs.tos.rfl = abs(.data$fngs.tos - 5.5),
        fngs.froms.rfl = abs(.data$fngs.froms - 5.5),
        direction = ifelse(.data$fngs.tos.rfl > .data$fngs.froms.rfl,
                           'out', .data$direction),

        direction = ifelse(.data$fngs.tos == .data$fngs.froms,
                           'jack', .data$direction),

        fngs.tos.left = .data$fngs.tos < 5.5,
        fngs.froms.left = .data$fngs.froms < 5.5,
        direction = ifelse(xor(.data$fngs.tos.left, .data$fngs.froms.left),
                           'across', .data$direction)
      )  %>%
      dplyr::select(1:6)

    return(mapping)
  }

  move.mapping <- loadMapping()
  move.keyset <- loadKeyset(keyset, as.character(keyset.select))

  move.mapping %<>%
    mergeMapping(move.keyset)

  if (include.details) {
      move.mapping %<>% getDetails()
  }

  return(move.mapping)
}
