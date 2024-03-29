% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dx.R
\name{find_fields}
\alias{find_fields}
\title{List fields associated with dataset ID}
\usage{
find_fields(dataset_id = NULL)
}
\arguments{
\item{dataset_id}{\itemize{
\item ID of the dataset, in \code{project-XXXX:record-YYYY} format
}}
}
\value{
data.frame of all field IDs associated with the dataset
}
\description{
Given a dataset ID generated from get_dataset_id(), runs \verb{dx extract_dataset}
with the \code{--list-fields} option.
}
\examples{

\dontrun{
ds_id <- get_dataset_id()
field_frame <- list_fields(ds_id)
}
}
