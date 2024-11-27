# Function to calculate signed deviation for each point in the data relative to the line
calculate_deviations <- function(gpx_data, line_data = NULL) {

  # Validate gpx_data input
  if (!all(c("Lat", "Long") %in% names(gpx_data))) {
    stop("Error: gpx_data must contain 'Lat' and 'Long' columns.")
  }

  # Determine line start and end points
  if (is.null(line_data)) {
    # Use first and last rows of gpx_data if line_data is not provided
    if (nrow(gpx_data) < 2) {
      stop("Error: gpx_data must contain at least two rows to infer the line.")
    }
    line_start <- c(gpx_data$Long[1], gpx_data$Lat[1])
    line_end <- c(gpx_data$Long[nrow(gpx_data)], gpx_data$Lat[nrow(gpx_data)])
  } else {
    # Validate line_data input
    if (nrow(line_data) != 2 || !all(c("Lat", "Long") %in% names(line_data))) {
      stop("Error: line_data must have exactly two rows with 'Lat' and 'Long' columns.")
    }
    line_start <- c(line_data$Long[1], line_data$Lat[1])
    line_end <- c(line_data$Long[2], line_data$Lat[2])
  }

  # Define a function to calculate the signed distance and intersection distance
  calculate_distances <- function(lat, lon) {
    point <- c(lon, lat)

    # Perpendicular distance using dist2Line
    closest_point_info <- geosphere::dist2Line(p = point, line = rbind(line_start, line_end))
    closest_point <- closest_point_info[1, 2:3]
    perpendicular_distance <- closest_point_info[1, 1]

    # Determine the sign of the perpendicular distance
    bearing_to_point <- geosphere::bearing(line_start, point)
    bearing_to_end <- geosphere::bearing(line_start, line_end)
    sign <- ifelse(
      (bearing_to_end - bearing_to_point + 360) %% 360 < 180,
      1,
      -1
    )
    signed_distance <- sign * perpendicular_distance

    # Calculate the distance along the main line to the intersection point
    intersection_distance <- geosphere::distGeo(line_start, closest_point)

    return(c(Signed_Distance = signed_distance, Intersection_Distance = intersection_distance))
  }

  # Apply the distance calculations to all GPX points
  gpx_data <- gpx_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Distances = list(calculate_distances(Lat, Long)),
      Signed_Distance = Distances[1],
      Intersection_Distance = Distances[2]
    ) %>%
    dplyr::select(-Distances) %>%
    dplyr::ungroup()

  return(gpx_data)
}