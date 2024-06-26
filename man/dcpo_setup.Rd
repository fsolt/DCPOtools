% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcpo_setup.R, R/dcpo_setup2.R
\name{dcpo_setup}
\alias{dcpo_setup}
\title{Prepare Data for DCPO}
\usage{
dcpo_setup(
  vars,
  datapath = "../data/dcpo_surveys",
  file = "",
  chime = TRUE,
  survey_additions = NULL,
  ...
)

dcpo_setup(
  vars,
  datapath = "../data/dcpo_surveys",
  file = "",
  chime = TRUE,
  survey_additions = NULL,
  ...
)
}
\arguments{
\item{vars}{a data frame (or, optionally, a .csv file) of survey items}

\item{datapath}{path to the directory that houses raw survey datasets}

\item{file}{a file path to save output to (usually a .csv or .rds, but any format supported by \code{rio::export})}

\item{chime}{play chime when complete?}

\item{survey_additions}{a data frame (or, optionally, a .csv file) of information on surveys not currently included in \code{surveys_data}}

\item{...}{additional arguments}
}
\value{
a data frame or list of data frames

a data frame or list of data frames
}
\description{
\code{dcpo_setup} prepares survey data for use with the \code{dcpo} function.

\code{dcpo_setup} prepares survey data for use with the \code{dcpo} function.
}
\details{
\code{dcpo_setup}, when passed a data frame of survey items, collects the
responses and formats them for use with the \code{dcpo} function.

\code{dcpo_setup}, when passed a data frame of survey items, collects the
responses and formats them for use with the \code{dcpo} function.
}
