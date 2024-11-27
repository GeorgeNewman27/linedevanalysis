\name{read_line_gpx}
\alias{read_line_gpx}
\title{Read Start and End Points of a Line}
\description{
Extracts the start and end points of a line from a GPX file or a 2x2 matrix of latitude/longitude coordinates.
This function is intended to identify the two defining points of the straight line you were following.
}
\usage{
read_line_gpx(input)
}
\arguments{
\item{input}{
Either a string representing the path to a .gpx file or a 2x2 numeric matrix of latitude/longitude values.
If a GPX file is provided, this file must have at least 2 track points. If it has more than 2 track points then the first and last points in the file are used.
If a matrix is provided, the first row represents the starting point, and the second row represents the ending point, the first row represents latitude, and the second row represents longitude.
}
}
\value{
A data frame with two rows containing the following columns:
\itemize{
  \item{\code{Index}}{: Index of the point, where 1 represents the start and 2 represents the end.}
  \item{\code{Lat}}{: Latitude of the point.}
  \item{\code{Long}}{: Longitude of the point.}
  \item{\code{Time}}{: Always \code{NA} (not applicable for this function).}
  \item{\code{Day}}{: Always \code{NA} (not applicable for this function).}
  \item{\code{Elev}}{: Always \code{NA} (not applicable for this function).}
}
The data frame contains only the start and end points, regardless of the number of intermediate points in the GPX file (if applicable).
}
\examples{
# Example 1: Extract start and end points from a GPX file
line_gpx <- read_line_gpx("straight_line.gpx")

# Example 2: Extract start and end points from a 2x2 matrix of lat/lon
line_matrix <- read_line_gpx(rbind(c(71.254, -40.398), c(70.922, -41.006)))
}
