% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow_functions.R
\name{put_forecast}
\alias{put_forecast}
\title{Upload forecast file and metadata to s3 bucket}
\usage{
put_forecast(saved_file, eml_file_name = NULL, config)
}
\arguments{
\item{saved_file}{full path of saved FLARE netcdf}

\item{eml_file_name}{full path of saved FLARE metadata}

\item{config}{flare configuration object}
}
\description{
Upload forecast file and metadata to s3 bucket
}
