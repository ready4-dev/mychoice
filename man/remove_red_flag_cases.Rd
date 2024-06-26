% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_remove.R
\name{remove_red_flag_cases}
\alias{remove_red_flag_cases}
\title{Remove red flag cases}
\usage{
remove_red_flag_cases(
  data_tb,
  flags_max_1L_int = 0,
  flags_tot_var_nm_1L_chr = "red_flag_count_int",
  qltv_ax_green_flag_1L_chr = "Green",
  qltv_ax_var_nm_1L_chr = character(0)
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{flags_max_1L_int}{Flags maximum (an integer vector of length one), Default: 0}

\item{flags_tot_var_nm_1L_chr}{Flags total variable name (a character vector of length one), Default: 'red_flag_count_int'}

\item{qltv_ax_green_flag_1L_chr}{Qualitative assessment green flag (a character vector of length one), Default: 'Green'}

\item{qltv_ax_var_nm_1L_chr}{Qualitative assessment variable name (a character vector of length one), Default: character(0)}
}
\value{
Data (a tibble)
}
\description{
remove_red_flag_cases() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove red flag cases. The function returns Data (a tibble).
}
\keyword{internal}
