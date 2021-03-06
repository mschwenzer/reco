% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reco.R
\name{str_replace_by_df}
\alias{str_replace_by_df}
\title{str_replace_by_df}
\usage{
str_replace_by_df(q, df)
}
\arguments{
\item{q}{a vector that can be processed by stringr::str_replace_all}

\item{df}{An object of cl`ass data.frame or which can be converted to data.frame by as.data.frame.}
}
\value{
a vector of the same length as q
}
\description{
replace every part of a string based on a data.frame given by df. str_replace_by_df
}
\author{
Marc Schwenzer
}
