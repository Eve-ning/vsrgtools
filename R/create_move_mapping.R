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
#' @param keyset.select A **character vector** based on
#' type of keyset.
#'
#' Valid Keyset.select values are '4','5', ..., '8SP','8SY','9'
#' Only 8K has special keysets
#' 8SYM: 8 Key Symmetry
#' 8SPL: 8 Key Special (Left)
#' 8SPR: 8 Key Special (Right)
#'
#' @param keyset A **data.frame** based on type of keyset.
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
#' '7' = data.frame(keys = 1:7,
#'                  fingers = c('R','M','I','T','I','M','R'))
#'
#' @param mapping A custom **5 by 5 matrix** to be used
#' to map onto the finger. If NA, a default would be used
#'
#' An example of a mapping. Values can change, but not
#' the format.
#'
#' pr means pinky to ring, rm means ring to middle, ...
#'
#' # [P] [R] [M] [I] [T]
#' c(7.0,5.0,3.0,2.7,2.7), # [P] pp,pr,pm,pi,pt
#' c(6.0,6.0,2.5,2.0,2.0), # [R] rp,rr,rm,ri,rt
#' c(3.5,3.0,5.0,1.0,1.3), # [M] mp,mr,mm,mi,mt
#' c(2.7,1.2,1.2,5.0,2.4), # [I] ip,ir,im,ii,it
#' c(2.7,1.7,1.5,1.9,5.0)  # [T] tp,tr,tm,ti,tt
#'
#' @param mapping.opp A custom **5 by 5 matrix** to be used
#' to map onto actions that involve alternating hands.
#' If NA, a default would be used
#'
#' Same format as `mapping`. However, pr would mean
#' FROM Hand 1 Pinky to Hand 2 Ring. Rest is the same idea.
#'
#' @export

createMoveMapping <- function(keyset.select=NA,
                              keyset=NA,
                              mapping=NA,
                              mapping.opp=NA){

  # Loads Mapping, if NA, it's defaulted
  loadMapping <- function(mapping){
    #' Loads the fngr mapping
    #'
    #' @description
    #' If there is no specified mapping, a default
    #' will be loaded
    #'
    #' Details of the format is located in the
    #' parent help

    fngr <- data.frame(P1 = c(rep(0,5)),
                       R1 = c(rep(0,5)),
                       M1 = c(rep(0,5)),
                       I1 = c(rep(0,5)),
                       T1 = c(rep(0,5)))

    fngr.opp <- data.frame(P2 = c(rep(0,5)),
                           R2 = c(rep(0,5)),
                           M2 = c(rep(0,5)),
                           I2 = c(rep(0,5)),
                           T2 = c(rep(0,5)))

    # If not defined, we will load default
    if (is.na(mapping)){
      fngr[] = c(
        read.table("src/r/defaults/move_mapping",
                   header = F, sep = ",")
      )
    } else {
      fngr[] <- mapping
    }

    if (is.na(mapping.opp)){
      fngr.opp[] <- c(
        read.table("src/r/defaults/move_mapping_opp",
                   header = F, sep = ",")
      )
    } else {
      fngr.opp[] = mapping.opp
    }

    fngr$froms = c('P1','R1','M1','I1','T1')
    fngr.opp$froms = c('P1','R1','M1','I1','T1')

    fngr %<>%
      melt(variable.name = 'tos',
           value.name = 'moves.values')
    fngr.opp %<>%
      melt(variable.name = 'tos',
           value.name = 'moves.values')

    # FROM 1 TO 2 to 2 TO 1
    fngr.opp.s <- fngr.opp
    colnames(fngr.opp.s) <- c('tos', 'froms', 'moves.values')

    # FROM 1 TO 1 to 2 TO 2
    fngr.s <- fngr
    fngr.s$froms <- sub("[[:digit:]]", "2", fngr.s$froms)
    fngr.s$tos <- sub("[[:digit:]]", "2", fngr.s$tos)

    fngr <- rbind(fngr, fngr.s, fngr.opp, fngr.opp.s)

    return(fngr)
  }

  # Chooses between keyset or keyset.select, if both NA, stop()
  loadKeyset <- function(keyset, keyset.select){
    if (!is.na(keyset)){
      return(keyset)
    } else if (!is.na(keyset.select)){
    move.keysets = list(
      '4' = data.frame(keys = 1:4,
                       fingers = c('M1','I1','I2','M2')),
      '5R' = data.frame(keys = 1:5,
                        fingers = c('M1','I1','T2','I2','M2')),
      '5L' = data.frame(keys = 1:5,
                        fingers = c('M1','I1','T1','I2','M2')),
      '6' = data.frame(keys = 1:6,
                       fingers = c('R1','M1','I1','I2','M2','R2')),
      '7R' = data.frame(keys = 1:7,
                        fingers = c('R1','M1','I1','T2','I2','M2','R2')),
      '7L' = data.frame(keys = 1:7,
                        fingers = c('R1','M1','I1','T1','I2','M2','R2')),
      '8SPL' = data.frame(keys = 1:8,
                          fingers = c('P1','R1','M1','I1','T2','I2','M2','R2')),
      '8SPR' = data.frame(keys = 1:8,
                          fingers = c('R1','M1','I1','T1','I2','M2','R2','P2')),
      '8SYM' = data.frame(keys = 1:8,
                          fingers = c('R1','M1','I1','T1','T2','I2','M2','R2')),
      '9R' = data.frame(keys = 1:9,
                        fingers = c('P1','R1','M1','I1','T2','I2','M2','R2','P2')),
      '9L' = data.frame(keys = 1:9,
                        fingers = c('P1','R1','M1','I1','T1','I2','M2','R2','P2'))
    )
    return(move.keysets[[keyset.select]])
    } else {
    stop("Either keyset or keyset.select must be defined.")
    }
  }

  move.mapping <- loadMapping(mapping)
  move.keyset <- loadKeyset(keyset,
                               as.character(keyset.select))

  # Merges both data.frames together
  mergeMapping <- function(mapping, keyset) {
    mapping %<>%
      merge(keyset,
            by.x = 'froms',
            by.y = 'fingers')
    colnames(mapping)[ncol(mapping)] <- "keys.froms"
    mapping %<>%
      merge(keyset,
            by.x = 'tos',
            by.y = 'fingers')
    colnames(mapping)[ncol(mapping)] <- "keys.tos"

    # We will return all due to move naming, which is easier
    # to work with with finger chars
    # mapping <- mapping[3:5] # Return only the required columns.
    return(mapping)
  }

  move.mapping %<>%
    mergeMapping(move.keyset)

  return(move.mapping)
}
