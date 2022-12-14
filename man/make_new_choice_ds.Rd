% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_new_choice_ds}
\alias{make_new_choice_ds}
\title{Make new choice dataset}
\usage{
make_new_choice_ds(
  choices_ls,
  dce_design_ls,
  mdl_params_ls,
  records_ls,
  replace_cards_int = integer(0),
  return_1L_chr = "dfidx"
)
}
\arguments{
\item{choices_ls}{Choices (a list)}

\item{dce_design_ls}{Discrete choice experiment design (a list)}

\item{mdl_params_ls}{Model parameters (a list)}

\item{records_ls}{Records (a list)}

\item{replace_cards_int}{Replace cards (an integer vector), Default: integer(0)}

\item{return_1L_chr}{Return (a character vector of length one), Default: 'dfidx'}
}
\value{
New choices (an output object of multiple potential types)
}
\description{
make_new_choice_ds() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make new choice dataset. The function returns New choices (an output object of multiple potential types).
}
\keyword{internal}
