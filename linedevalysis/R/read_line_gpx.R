# Function to extract start and end points from a GPX file or a lat/lon matrix
read_line_gpx <- function(input) {

  # If the input is a character, treat it as a file path
  if (is.character(input) && length(input) == 1) {
    # Check if the file exists
    if (!file.exists(input)) {
      stop("Error: File not found.")
    }

    # Read the GPX file and extract data using the helper function
    line_data <- tryCatch(
      read_single_gpx(input),
      error = function(e) {
        stop(paste("Error processing GPX file:", input, "-", e$message))
      }
    )

    # Ensure the resulting data frame is valid and has rows
    if (nrow(line_data) < 2) {
      stop(paste("Error: GPX file", input, "must contain at least two track points."))
    }

    # Set up in same structure as gpx_data
    line_data$Time = NA
    line_data$Elev = NA
    line_data$Day = NA

    # Retain only the first and last rows
    line_data <- line_data[c(1, nrow(line_data)), ]

    # Rebase the Index column
    line_data$Index <- seq_len(nrow(line_data))

  } else if (is.matrix(input) && all(dim(input) == c(2, 2)) && is.numeric(input)) {
    # If the input is a 2x2 numeric matrix, treat it as lat/lon data
    lat <- input[, 1]
    lon <- input[, 2]

    # Validate lat/lon ranges
    if (any(lat < -90 | lat > 90)) {
      stop("Error: Latitude values must be between -90 and 90.")
    }
    if (any(lon < -180 | lon > 180)) {
      stop("Error: Longitude values must be between -180 and 180.")
    }

    # Create the line data directly from the matrix
    line_data <- data.frame(
      Lat = lat,
      Long = lon,
      Time = NA,
      Day = NA,
      Elev = NA,
      Index = seq_len(2),
      stringsAsFactors = FALSE
    )

  } else {
    # If the input is neither a file path nor a 2x2 matrix, throw an error
    stop("Error: Input must be either a valid file path to a GPX file or a 2x2 matrix of lat/lon coordinates.")
  }

  return(line_data)
}