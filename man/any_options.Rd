% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{any_options}
\alias{any_options}
\title{Set or get options for my package}
\usage{
any_options(...)
}
\arguments{
\item{...}{Option names to retrieve option values or \code{[key]=[value]} pairs to set options.}
}
\value{
the default and modified options.
}
\description{
Set or get options for my package
}
\section{Supported options}{

The following options are supported
 host: Host of anything llm
 apikey: apikey of anything llm
}

\examples{
any_options(host = "http://127.0.0.1:3001/")
}
