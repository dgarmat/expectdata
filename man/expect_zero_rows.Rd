% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_expectations.R
\name{expect_zero_rows}
\alias{expect_zero_rows}
\title{Check if a dataframe has no rows. If a vector is given the length of the vectors is checked.}
\usage{
expect_zero_rows(df1, stop_if_fail = TRUE, report_rowcount = FALSE,
  return_df = TRUE, show_fails = TRUE)
}
\arguments{
\item{df1}{dataframe or vector to check}

\item{stop_if_fail}{T/F for whether to consider failure an error}

\item{report_rowcount}{T/F for whether to return the number of rows}

\item{return_df}{T/F whether to end function with dataframe 1 input (as in if a check in part of a pipe)}

\item{show_fails}{T/F for whether to show head(df1) if it is non-zero. This can help with the next step of debugging why these records exist.}
}
\value{
several options depending on whether it fails or succeeeds
}
\description{
Check if a dataframe has no rows. If a vector is given the length of the vectors is checked.
}
\examples{
expect_zero_rows(mtcars[mtcars$cyl == 0, ], return_df = F)
# [1] "No rows found as expected...OK"

expect_zero_rows(mtcars$cyl[mtcars$cyl == 0])
# [1] "No rows found as expected...OK"
# numeric(0)

expect_zero_rows(mtcars)
                   mpg cyl disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
Error: Different number of rows: 32 vs: 0 
}
