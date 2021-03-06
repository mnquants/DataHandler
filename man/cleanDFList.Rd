% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataHandle.R
\name{cleanDFList}
\alias{cleanDFList}
\title{Remove NA values from each dataframe in a list and name columns}
\usage{
cleanDFList(dfList, cnames)
}
\arguments{
\item{dfList}{List of dataframes}

\item{cnames}{Character vector of columns names for dataframe; must match ncol for dataframe}
}
\value{
List of dataframed without NA values
}
\description{
Remove NA values from each dataframe in a list and name columns
}
