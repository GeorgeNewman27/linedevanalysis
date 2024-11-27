# Helper function to read a single GPX file
read_single_gpx <- function(file_name) {

  # Check if the file exists
  if (!file.exists(file_name)) {
    stop(paste("Error: File not found:", file_name))
  }

  # Read the GPX file
  gpx <- tryCatch(
    xml2::read_xml(file_name),
    error = function(e) {
      stop(paste("Error reading file:", file_name, "-", e$message))
    }
  )

  # Manually define the namespace with the URL from the GPX file
  ns <- c(topo = "http://www.topografix.com/GPX/1/1")

  # Extract track points (<trkpt> elements) using the manually defined namespace
  trkpts <- tryCatch(
    xml2::xml_find_all(gpx, ".//topo:trkpt", ns = ns),
    error = function(e) {
      stop(paste("Error parsing GPX file:", file_name, "-", e$message))
    }
  )

  if (length(trkpts) == 0) {
    stop(paste("Error: No track points found in file:", file_name))
  }

  # Extract mandatory attributes and elements into vectors
  latitudes <- as.double(xml2::xml_attr(trkpts, "lat"))
  longitudes <- as.double(xml2::xml_attr(trkpts, "lon"))

  # Extract elevations and times, handling missing tags with NA
  elev_times <- sapply(trkpts, function(trkpt) {
    c(
      Elev = as.double(xml2::xml_text(xml2::xml_find_first(trkpt, "topo:ele", ns = ns))) %||% NA,
      Time = xml2::xml_text(xml2::xml_find_first(trkpt, "topo:time", ns = ns)) %||% NA
    )
  })

  # Create a data frame directly from the vectors
  data <- data.frame(
    Lat = latitudes,
    Long = longitudes,
    Time = tryCatch(
      as.POSIXct(elev_times["Time", ], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      error = function(e) {
        stop(paste("Error parsing time in file:", file_name, "-", e$message))
      }
    ),
    Elev = as.double(elev_times["Elev", ]),
    stringsAsFactors = FALSE
  )

  return(data)
}