% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run-ed.R
\name{tail_ed_output}
\alias{tail_ed_output}
\title{View the end of the ED2 output log file}
\usage{
tail_ed_output(casename, out_root = getOption("fortebaseline.ed_root"), ...)
}
\arguments{
\item{casename}{Name}

\item{out_root}{Path to ED2 output root directory. Default is the option
`fortebaseline.ed_root`, and if unset, "ed2-output" in the project root
directory.}

\item{...}{Additional arguments to [utils::tail()]}
}
\description{
View the end of the ED2 output log file
}
