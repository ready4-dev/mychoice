% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_attempt_dur_red_flag}
\alias{add_attempt_dur_red_flag}
\title{Add attempt duration red flag}
\usage{
add_attempt_dur_red_flag(
  data_tb,
  attempt_dur_min_1L_dbl,
  attempt_dur_var_nm_1L_chr
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{attempt_dur_min_1L_dbl}{Attempt duration minimum (a double vector of length one)}

\item{attempt_dur_var_nm_1L_chr}{Attempt duration variable name (a character vector of length one)}
}
\value{
Data (a tibble)
}
\description{
add_attempt_dur_red_flag() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add attempt duration red flag. The function returns Data (a tibble).
}
\keyword{internal}
