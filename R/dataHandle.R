#' Calculate log returns on vector or list
#' @param x Numeric vector or list
#' @param na_omit TRUE to remove NA values
#' @return Vector or List of log returns on x
#' @export
logReturn <- function(x, na_omit = TRUE) {
  if (na_omit) {
    if (typeof(x) =="list") { lapply(x, function(ts) {
      logTS <- data.frame(na.omit(diff(log(ts))))
      names(logTS) <- "Log Return"
      return(logTS)
    }) }
    else {  na.omit(diff(log(x))) }
  }
  else {
    if (typeof(x) =="list") { lapply(x, function(ts) {
      logTS <- data.frame(diff(log(ts)))
      names(logTS) <- "Log Return"
      return(logTS)
    }) }
    else {  diff(log(x)) }
  }
}

#' Fetch and store equity data from Quandl
#' @param tv Character vector of stock tickers
#' @param start Start date with format: "YYYY-MM-DD"
#' @param end End date with format: "YYYY-MM-DD"
#' @param src Source of Database (default "WIKI")
#' @return List of equity data in xts format
#' @export
quandlList <- function(tv, start, end, src="WIKI") {
  if(!require(Quandl)){
    install.packages("Quandl")
    library(Quandl)
  }
  if(!require(xts)){
    install.packages("xts")
    library(xts)
  }
  data <- lapply(tv, function(ts) {
    tryCatch(
      Quandl(paste0(src,"/",ts), start_date=start, end_date=end, type="xts"),
      error = function(e) print("Error fetching data.")
    )
  })
  names(data) <- tv
  return(data)
}

#' Index column(s) in quandlList object
#' @param quandlList quandlList object
#' @param idx Character vector to index columns in quandlList
#' @return List of indexed column(s) in quandList object
#' @export
idxList <- function(quandlList, idx = "Adj. Close") {
  lapply(quandlList, function(quandlList) {
    quandlList[,idx]
  })
}

#' Apply technical indicators function from TTR to
#' list of time series data
#' @param tsList List of timeseries data with 1 column
#' @param FUN Vector of functions from TTR package; must only take one argument
#' @return List of xts dataframes with technical indicators applied to tsList
#' @export
techList  <- function(tsList, FUN) {
  if(!require(TTR)){
    install.packages("TTR")
    library(TTR)
  }
  if(!require(xts)){
    install.packages("xts")
    library(xts)
  }
  lapply(tsList, function(ts) {
    tryCatch(
      as.xts(data.frame(lapply(FUN, function(x) {
        x(na.omit(ts))
      }))),
      error = function(e) print("Error.")
    )
  })
}

#' Remove NA values from each dataframe in a list and name columns
#' @param dfList List of dataframes
#' @param cnames Character vector of columns names for dataframe; must match ncol for dataframe
#' @return List of dataframed without NA values
#' @export
cleanDFList <- function(dfList, cnames) {
  lapply(dfList, function(df) {
    cleaned <- na.omit(df)
    tryCatch(
      names(cleaned) <- cnames,
      error = function(e) print("Error.")
    )
    return(cleaned)
  })
}

#' Write dataframe or list of dataframes to csv file(s)
#' @param x Dataframe or list of dataframes
#' @param path Path directory to save csv file(s)
#' @param replaceExisting TRUE to replace existing folder if it exists
#' @export
writeCSV <- function(x, path, replaceExisting = TRUE) {
  path <- file.path(path)
  if(!replaceExisting && file.exists(path)) {
    stop("Folder already exists and replaceExisting is FALSE.")
  }
  else {
    if (!file.exists(path)) { dir.create(path) }
    i <- 1
    names <- names(x)
    lapply(x, function(df) {
      i <<- i + 1
      write.zoo(df, paste0(path, "/", names[i], ".csv"), row.names=FALSE, col.names=TRUE, sep=",")
    })
    invisible(0)
  }
}

#' Calculate Portfolio Asset Mean and Standard Deviation
#' @param logL List of time series returns for each asset
#' @return Mean and SD for each asset in logL
#' @export
assetMuSd <- function(logL){
  data <- sapply(logL, function(ts) {
    tmp <- c(mean(ts), sd(ts))
    names(tmp) <- c("Mean", "Sd")
    return(tmp)
  })
  return(t(data))
}
