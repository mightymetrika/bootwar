% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_funcs.R
\name{process_seed}
\alias{process_seed}
\title{Process Seed Input for the Bootwar Game}
\usage{
process_seed(seed_input)
}
\arguments{
\item{seed_input}{Numeric or NA. The input seed value from the Shiny app.}
}
\value{
Integer or NULL. If the input is not NA, it returns the integer value
of the seed. Otherwise, it returns NULL.
}
\description{
This function processes the seed input for the Bootwar game.
It converts the seed to an integer if it's not NA. If the seed is NA, it
returns NULL.
}
\keyword{internal}
