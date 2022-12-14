% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_data.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Count the total number of observations for each month and year combination.}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{a vector of years.}
}
\value{
a table for counting based on each combination of months and years.
}
\description{
This function is used to summarize documents in a count table.
External packages are loaded including dplyr (for counting) and tidyr (for generating a table).
}
\examples{
fars_summarize_years(c(2013,2014))

}
