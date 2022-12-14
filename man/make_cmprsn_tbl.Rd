% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_cmprsn_tbl}
\alias{make_cmprsn_tbl}
\title{Make comparison table}
\usage{
make_cmprsn_tbl(
  ds_tb,
  grouping_var_nms_chr,
  popl_var_nm_1L_chr,
  sample_var_nm_1L_chr,
  expected_dbl = numeric(0),
  is_pc_1L_lgl = T
)
}
\arguments{
\item{ds_tb}{Dataset (a tibble)}

\item{grouping_var_nms_chr}{Grouping variable names (a character vector)}

\item{popl_var_nm_1L_chr}{Population variable name (a character vector of length one)}

\item{sample_var_nm_1L_chr}{Sample variable name (a character vector of length one)}

\item{expected_dbl}{Expected (a double vector), Default: numeric(0)}

\item{is_pc_1L_lgl}{Is pc (a logical vector of length one), Default: T}
}
\value{
Comparison (a tibble)
}
\description{
make_cmprsn_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make comparison table. The function returns Comparison (a tibble).
}
\keyword{internal}
