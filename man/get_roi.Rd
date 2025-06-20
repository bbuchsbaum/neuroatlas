% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_generic.R, R/atlas.R
\name{get_roi}
\alias{get_roi}
\alias{get_roi.atlas}
\title{Extract a region of interest (ROI) from an atlas}
\usage{
get_roi(x, label = NULL, id = NULL, hemi = NULL)

\method{get_roi}{atlas}(x, label = NULL, id = NULL, hemi = NULL)
}
\arguments{
\item{x}{An atlas object}

\item{label}{Character string specifying the ROI label/name}

\item{id}{Numeric ID of the ROI in the atlas}

\item{hemi}{Character string specifying hemisphere ('left' or 'right')}
}
\value{
Returns a subset of the atlas containing only the specified ROI
}
\description{
Extracts a specific region of interest from an atlas object based on label, ID,
and hemisphere information.
}
\examples{
\dontrun{
# Load the aseg atlas
atlas <- get_aseg_atlas()

# Extract the hippocampus ROI
roi <- get_roi(atlas, label = "Hippocampus")
}
}
