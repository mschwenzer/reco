% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reco.R
\name{generate_recode_code}
\alias{generate_recode_code}
\title{generate_recode_code}
\usage{
generate_recode_code(a.df, df_from_class, df_to_class, not.matching)
}
\arguments{
\item{a.df}{a data.frame}

\item{df_from_class}{class of from column in data.frame}

\item{df_to_class}{class of to column in data.frame}

\item{not.matching}{elements in replacements that was not in the orig vector}
}
\value{

}
\description{
generate_recode_code
}
\details{
generate code for dplyr::recode.
}
\author{
Marc Schwenzer
}
