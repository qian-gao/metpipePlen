#' @title filter_peaks_summary
#'
#' @description Filter the peaks based on different criteria and generate summary
#'              statistics.
#'
#' @param XCMSnExp A XCMSnExp object. If null, peaktable should be mandatory.
#' @param peaktable A dataframe of peaktable, feature x sample. If null, XCMSnExp
#'                  should be mandatory.
#' @param sample.type A vector indicating sample types.
#' @param rsd.filter A list of threshold for keeping features based on their rsd in QCs.
#'                   e.g. c( 1, "QC", "<", 0.3).
#' @param mean.filter A list of threshold for keeping features based on their mean in QCs
#'                    or other sample types. e.g. list( c( 1, "PO", ">", "1000"),
#'                                                      c( 1, "PO100", ">", "PO50") ).
#' @param rt.range Retention time range for keeping features. e.g. c(0.5, 30).
#'
#' @return A data frame object that contains filtered peaktable and summary statistics.
#' @examples filter_peaks_summary( peaktable = peaks,
#'                                 sample.type = sample.info.pos$Sample.type,
#'                                 rsd.filter = c( 1, "QC", "<", 0.3),
#'                                 mean.filter = c(1, "QC", ">", 500),
#'                                 rt.range = c(0.5, 30))
#'
#' @export

filter_peaks_summary <-
  function( XCMSnExp = NULL,
            peaktable = NULL,
            sample.type = NULL,
            rsd.filter = NULL,
            rt.range = NULL,
            mean.filter = NULL
  ){

    if ( !is.null(XCMSnExp) ){

      mzrt <-
        XCMSnExp_mzrt( XCMSnExp = XCMSnExp,
                       mzdigit = 4,
                       rtdigit = 1,
                       method = "medret",
                       value = "into")

      data <- t(mzrt$data) %>%
        as.data.frame()

      rt.range.s <- rt.range*60

    } else if ( !is.null(peaktable) ){

      mzrt <-
        list( peaktable = peaktable,
              rt = peaktable$rt )

      data <-
        t( peaktable[, !colnames(peaktable) %in% c("mz", "rt", "Identity")] ) %>%
        as.data.frame()

      colnames(data) <- mzrt$peaktable$Identity
      rt.range.s <- rt.range

    }

    summary <-
      calculate_summary( data = data,
                     type = sample.type,
                     impute = TRUE)

    summary$type.mean <-
      summary$type.mean %>%
      rename_all(~stringr::str_replace(.,"^mean.",""))

    summary$type.sd <-
      summary$type.sd %>%
      rename_all(~stringr::str_replace(.,"^sd.",""))

    summary$type.rsd <-
      summary$type.rsd %>%
      rename_all(~stringr::str_replace(.,"^rsd.",""))

    mzrt <- c(mzrt, summary)
    index.list <- list()

    if (!is.null(mean.filter)){

      for (i in 1:ifelse(is.list(mean.filter), length(mean.filter), 1)){

        if (i == 1 & !is.list(mean.filter)){
          m.thres <- mean.filter
        } else {
          m.thres <- mean.filter[[i]]
        }

        index <-
          apply( mzrt$type.mean,
                 MAR = 1,
                 function(x){
                   if (!is.na(as.numeric(m.thres[4]))){
                     eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], m.thres[4]) ))
                   } else {
                     eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], x[m.thres[4]]) ))
                   }
                 })

        index[is.na(index)] <- FALSE
        index.list$mean.filter[[i]] <- c(index)

        mzrt <-
          mzrt_filter( mzrt = mzrt,
                       index = index)

        print( paste0( "Only keep features having mean intensity ",
                       m.thres[1], "*", m.thres[2], " ", m.thres[3], " ", m.thres[4],
                       ": ", sum(!index), " features have been removed" ),
               quote = FALSE)

      }

    }

    if (!is.null(rsd.filter)){

      for (i in 1:ifelse(is.list(rsd.filter), length(rsd.filter), 1)){

        if (i == 1 & !is.list(rsd.filter)){
          m.thres <- rsd.filter
        } else {
          m.thres <- rsd.filter[[i]]
        }

        index <-
          apply( mzrt$type.rsd,
                 MAR = 1,
                 function(x){
                   if (!is.na(as.numeric(m.thres[4]))){
                     eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], m.thres[4]) ))
                   } else {
                     eval( parse( text = paste0( m.thres[1], "*", x[m.thres[2]], m.thres[3], x[m.thres[4]]) ))
                   }
                 })

        index[is.na(index)] <- FALSE
        index.list$rsd.filter[[i]] <- c(index)

        mzrt <-
          mzrt_filter( mzrt = mzrt,
                       index = index)

        print( paste0( "Only keep features having RSD in ",
                       m.thres[1], "*", m.thres[2], " ", m.thres[3], " ", m.thres[4],
                       ": ", sum(!index), " features have been removed" ),
               quote = FALSE)

      }

    }

    if (!is.null(rt.range)){

      index <-
        mzrt$rt > rt.range.s[1] & mzrt$rt < rt.range.s[2]

      index.list$index.rt.range <- index

      mzrt <-
        mzrt_filter( mzrt = mzrt,
                     index = index)

      print( paste0( "Only keep features having retention time ",
                     rt.range[1], " - ", rt.range[2], " min",
                     ": ", sum(!index), " features have been removed" ),
             quote = FALSE)

    }

    mzrt$index.list <- index.list

    tm <- mzrt$type.mean
    colnames(tm) <- paste0( "mean.", colnames(tm))
    tsd <- mzrt$type.sd
    colnames(tsd) <- paste0( "sd.", colnames(tsd))
    trsd <- mzrt$type.rsd
    colnames(trsd) <- paste0( "rsd.", colnames(trsd))

    summary <-
      data.frame(tm, trsd[, -1], tsd[, -1])

    mzrt$summary <- summary

    return(mzrt)

  }

#' @title mzrt_filter
#'
#' @description Apply the filter to all the elements in a list.
#'
#' @param mzrt A list of dataframe or vectors containing peaktable, feature.info.
#' @param index Index of features to remove.
#'
#' @return A list of filtered dataframe or vectors.
#' @examples mzrt_filter( mzrt = list,
#'                        index = index)
#'
mzrt_filter <-
  function( mzrt = NULL,
            index = NULL){

    obj.names <- names(mzrt)
    obj.names <- obj.names[ !grepl( "sample.info", obj.names ) ]

    for ( i in 1:length(obj.names) ){

      name <- obj.names[i]

      if ( !is.null(dim(mzrt[[name]])) ) mzrt[[name]] <- mzrt[[name]][index, ]
      else mzrt[[name]] <- mzrt[[name]][index]


    }

    return(mzrt)

  }
