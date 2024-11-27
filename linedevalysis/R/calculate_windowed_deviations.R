# Function to calculate windowed deviations along the target line
calculate_windowed_deviations <- function(gpx_data, line_length, interval) {

  # Create a sequence of intervals
  window_starts <- seq(0, line_length, by = interval)
  window_ends <- window_starts + interval

  # Find maximum deviation for each interval
  windowed_deviations <- sapply(seq_along(window_starts), function(i) {

    # Extract start and end of the current window
    start_meter <- window_starts[i]
    end_meter <- window_ends[i]

    # Points in the current interval
    points_in_segment <- gpx_data %>%
      dplyr::filter(Intersection_Distance >= start_meter & Intersection_Distance < end_meter)

    if (nrow(points_in_segment) > 0) {
      # If there are points in the interval, return the max Signed_Distance
      return(max(abs(points_in_segment$Signed_Distance)))
    } else {
      # If no points exist, compute previous and next deviations
      prev_meter <- max(
        gpx_data$Intersection_Distance[gpx_data$Intersection_Distance < start_meter],
        na.rm = TRUE
      )
      next_meter <- min(
        gpx_data$Intersection_Distance[gpx_data$Intersection_Distance >= end_meter],
        na.rm = TRUE
      )

      # Deviations for previous and next windows
      prev_deviation <- ifelse(
        is.finite(prev_meter),
        max(
          abs(gpx_data$Signed_Distance[gpx_data$Intersection_Distance == prev_meter])
        ),
        0
      )
      next_deviation <- ifelse(
        is.finite(next_meter),
        max(
          abs(gpx_data$Signed_Distance[gpx_data$Intersection_Distance == next_meter])
        ),
        0
      )

      # Return average of deviations
      return(mean(c(prev_deviation, next_deviation), na.rm = TRUE))
    }
  })

  return(windowed_deviations)
}