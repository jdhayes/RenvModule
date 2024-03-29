% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module.R
\name{module}
\alias{module}
\title{Define what happens based on action}
\usage{
module(action_type, module_name = "")
}
\arguments{
\item{action_type}{Name of the action to be executed as character vector. The following switches are accepted: \dQuote{avail}, \dQuote{list}, \dQuote{init}, \dQuote{load}, \dQuote{unload}, and \dQuote{clear}.}

\item{module_name}{Name of software to load as character vector.}
}
\description{
Define what happens based on action
}
\examples{
\dontrun{
module("load","tophat")
module("load","tophat/2.1.1")
module("list")
module("avail")
module("init")
module("unload", "tophat")
module("unload", "tophat/2.1.1")
module("clear")
}
}
