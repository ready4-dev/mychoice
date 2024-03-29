% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_case_choices_mat}
\alias{make_case_choices_mat}
\title{Make case choices matrix}
\usage{
make_case_choices_mat(
  ds_tb,
  block_indcs_ls,
  choice_sets_ls,
  design_mat,
  choice_vars_pfx_1L_chr = "DCE_B"
)
}
\arguments{
\item{ds_tb}{Dataset (a tibble)}

\item{block_indcs_ls}{Block indices (a list)}

\item{choice_sets_ls}{Choice sets (a list)}

\item{design_mat}{Design (a matrix)}

\item{choice_vars_pfx_1L_chr}{Choice variables prefix (a character vector of length one), Default: 'DCE_B'}
}
\value{
Case choices (a matrix)
}
\description{
make_case_choices_mat() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make case choices matrix. The function returns Case choices (a matrix).
}
\keyword{internal}
