% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_choice_descvs_params}
\alias{make_choice_descvs_params}
\title{Make choice descriptives parameters}
\usage{
make_choice_descvs_params(
  dce_design_ls,
  records_ls,
  opt_out_nm_1L_chr = "Opt out"
)
}
\arguments{
\item{dce_design_ls}{Discrete choice experiment design (a list)}

\item{records_ls}{Records (a list)}

\item{opt_out_nm_1L_chr}{Opt out name (a character vector of length one), Default: 'Opt out'}
}
\value{
Choice descriptives parameters (a list)
}
\description{
make_choice_descvs_params() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make choice descriptives parameters. The function returns Choice descriptives parameters (a list).
}
\keyword{internal}
