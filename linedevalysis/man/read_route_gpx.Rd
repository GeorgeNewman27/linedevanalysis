\name{read_route_gpx}
\alias{read_route_gpx}
\title{Read Route GPX Files}
\description{
Reads one or more GPX files into a single data frame, combining the track points from all provided files.
This function is intended to read route data from your expedition gathered using a tracking device.
}
\usage{
read_route_gpx(filenames)
}
\arguments{
\item{filenames}{
A non-empty character vector of strings representing file paths to GPX files.
}
}
\value{
A data frame with the following columns:
\itemize{
  \item{\code{Index}}{: Sequential index of each track point across all input files.}
  \item{\code{Lat}}{: Latitude of the track point.}
  \item{\code{Long}}{: Longitude of the track point.}
  \item{\code{Time}}{: Timestamp of the track point (optional; parsed if available in the GPX file).}
  \item{\code{Day}}{: The day number of the expedition corresponding to the track point (optional; computed if the \code{Time} column is present).}
  \item{\code{Elev}}{: Elevation of the track point (optional; parsed if available in the GPX file).}
}
The data frame will contain one row per track point from the provided GPX file(s).
}
\examples{
# Example 1: Reading a single GPX file
route_gpx <- read_route_gpx("combined_days.gpx")

# Example 2: Reading multiple GPX files, e.g. one per day of expeditioning
route_gpx <- read_route_gpx(c("day_1.gpx", "day_2.gpx", "day_3.gpx"))
}

