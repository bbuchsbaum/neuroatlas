% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schaefer.R
\name{get_schaefer_atlas}
\alias{get_schaefer_atlas}
\title{Load Schaefer Brain Parcellation Atlas}
\source{
\url{https://github.com/ThomasYeoLab/CBIG/}
}
\usage{
get_schaefer_atlas(
  parcels = c("100", "200", "300", "400", "500", "600", "800", "1000"),
  networks = c("7", "17"),
  resolution = c("1", "2"),
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE
)
}
\arguments{
\item{parcels}{Character string specifying number of parcels.
Options: "100", "200", "300", "400", "500", "600", "800", "1000"}

\item{networks}{Character string specifying network count.
Options: "7", "17"}

\item{resolution}{Character string specifying MNI space resolution in mm.
Options: "1", "2"}

\item{outspace}{Optional \code{NeuroSpace} object for resampling the atlas}

\item{smooth}{Logical. Whether to smooth parcel boundaries after resampling.
Default: FALSE}

\item{use_cache}{Logical. Whether to cache downloaded files. Default: TRUE}
}
\value{
A list with classes c("schaefer", "volatlas", "atlas") containing:
\describe{
  \item{name}{Character string identifying atlas version}
  \item{atlas}{\code{ClusteredNeuroVol} object containing the parcellation}
  \item{cmap}{Data frame with RGB colors for visualization}
  \item{ids}{Integer vector of region IDs}
  \item{labels}{Character vector of region names}
  \item{orig_labels}{Original region labels from source data}
  \item{network}{Network assignment for each region}
  \item{hemi}{Hemisphere designation for each region}
}
}
\description{
Retrieves and loads the Schaefer brain parcellation atlas, which provides a
data-driven parcellation of the cerebral cortex based on both local gradient
and global similarity approaches.
}
\details{
The Schaefer atlas offers multiple resolutions of cortical parcellation
(100-1000 parcels) and two network versions (7 or 17 networks). The atlas
is based on resting-state functional connectivity from 1489 subjects.
Features include:
\itemize{
  \item Multiple granularity levels (100-1000 parcels)
  \item Network assignments (7 or 17 networks)
  \item Bilateral parcellation
  \item Available in different resolutions (1mm or 2mm)
}
}
\examples{
\dontrun{
# Load 300-parcel atlas with 7 networks
atlas <- get_schaefer_atlas(parcels = "300", networks = "7")

# Load high-resolution version
atlas_hires <- get_schaefer_atlas(parcels = "400",
                                 networks = "17",
                                 resolution = "1")

# Resample to a different space
new_space <- neuroim2::NeuroSpace(dim = c(91,109,91),
                                 spacing = c(2,2,2))
atlas_resampled <- get_schaefer_atlas(parcels = "300",
                                     outspace = new_space)
}

}
\references{
Schaefer, A., et al. (2018). Local-Global Parcellation of the Human Cerebral
Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9),
3095-3114.
}
\seealso{
\code{\link{get_schaefer_surfatlas}} for surface-based version
}
