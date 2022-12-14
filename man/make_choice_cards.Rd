% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_choice_cards}
\alias{make_choice_cards}
\title{Make choice cards}
\usage{
make_choice_cards(
  dce_design_ls,
  block_indcs_ls = list(),
  seed_1L_int = 1987,
  set_idx_1L_int = integer(0),
  transform_att_nms_1L_lgl = T
)
}
\arguments{
\item{dce_design_ls}{Discrete choice experiment design (a list)}

\item{block_indcs_ls}{Block indices (a list), Default: list()}

\item{seed_1L_int}{Seed (an integer vector of length one), Default: 1987}

\item{set_idx_1L_int}{Set index (an integer vector of length one), Default: integer(0)}

\item{transform_att_nms_1L_lgl}{Transform attribute names (a logical vector of length one), Default: T}
}
\value{
Choice cards (a list)
}
\description{
make_choice_cards() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make choice cards. The function returns Choice cards (a list).
}
\keyword{internal}
