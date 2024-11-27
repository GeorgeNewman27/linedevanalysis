# Function to calculate Burdell score as laid out in https://scoremyline.com/
calculate_Burdell_score <- function(gpx_data, line_data = NULL, experience = "Pro") {

  # Validate gpx_data input
  if (!all(c("Signed_Distance", "Intersection_Distance") %in% names(gpx_data))) {
    stop("Error: gpx_data must contain 'Signed_Distance' and 'Intersection_Distance' columns.")
  }

  # Handle line_data: use first and last points of gpx_data if not provided
  if (is.null(line_data)) {
    if (!all(c("Lat", "Long") %in% names(gpx_data))) {
      stop("Error: gpx_data must contain 'Lat' and 'Long' columns to infer line_data.")
    }
    line_data <- gpx_data[c(1, nrow(gpx_data)), c("Lat", "Long")]
  } else {
    # Validate line_data input if provided
    if (nrow(line_data) != 2 || !all(c("Lat", "Long") %in% names(line_data))) {
      stop("Error: line_data must have exactly two rows with 'Lat' and 'Long' columns.")
    }
  }

  # Extract the geodesic line's start and end points
  line_start <- c(line_data$Long[1], line_data$Lat[1])
  line_end <- c(line_data$Long[2], line_data$Lat[2])

  # Length of the target line in meters
  target_length <- geosphere::distVincentySphere(line_start, line_end)  # in meters

  # Define penalty coefficient and interval based on experience level
  if (tolower(experience) == "pro") {
    coeff <- 1 / 150
    interval <- 1  # 1 meter for Pro
  } else if (tolower(experience) == "amateur") {
    coeff <- 1 / 175
    interval <- 5  # 5 meters for Amateur
  } else if (tolower(experience) == "newbie") {
    coeff <- 1 / 200
    interval <- 25  # 25 meters for Newbie
  } else {
    stop("Invalid experience level. Choose from 'Pro', 'Amateur', or 'Newbie'.")
  }

  # Calculate windowed deviations
  windowed_deviations <- calculate_windowed_deviations(gpx_data, target_length, interval)

  # Vectorized calculation of penalties
  penalties <- 100 * (windowed_deviations * coeff) ^ log(target_length, 10)

  # Total penalty is the sum of all penalties
  total_penalty <- sum(penalties, na.rm = TRUE)

  # Calculate the overall score (100 - total penalties)
  score <- max(100 - total_penalty, 0)

  return(score)
}
