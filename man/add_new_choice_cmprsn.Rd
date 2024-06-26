% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_new_choice_cmprsn}
\alias{add_new_choice_cmprsn}
\title{Add new choice comparison}
\usage{
add_new_choice_cmprsn(
  analysis_ls,
  dce_design_ls,
  mdl_params_ls,
  new_choices_ls,
  records_ls,
  altv_nms_chr = character(0),
  set_idx_1L_int = integer(0),
  with_chr = "mnl_mlogit_mdl"
)
}
\arguments{
\item{analysis_ls}{Analysis (a list)}

\item{dce_design_ls}{Discrete choice experiment design (a list)}

\item{mdl_params_ls}{Model parameters (a list)}

\item{new_choices_ls}{New choices (a list)}

\item{records_ls}{Records (a list)}

\item{altv_nms_chr}{Alternative names (a character vector), Default: character(0)}

\item{set_idx_1L_int}{Set index (an integer vector of length one), Default: integer(0)}

\item{with_chr}{With (a character vector), Default: 'mnl_mlogit_mdl'}
}
\value{
Analysis (a list)
}
\description{
add_new_choice_cmprsn() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add new choice comparison. The function returns Analysis (a list).
}
\keyword{internal}
