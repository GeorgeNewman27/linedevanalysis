# Function to read GPX files into a single data frame and assign day numbers
read_route_gpx <- function(file_names) {

  # Check if the input is valid
  if (!is.vector(file_names) || length(file_names) == 0) {
    stop("Error: 'file_names' must be a non-empty vector of file paths in the current working directory.")
  }

  # Process all files and combine into a single data frame
  combined_data <- tryCatch(
    do.call(rbind, lapply(file_names, read_single_gpx)),
    error = function(e) {
      stop(paste("Error combining GPX data frames:", e$message))
    }
  )

  # Extract the date part from the Time column
  combined_data$Date <- as.Date(combined_data$Time)

  # Find unique dates and assign day numbers
  unique_dates <- sort(unique(combined_data$Date))
  date_to_day <- setNames(seq_along(unique_dates), unique_dates)

  # Map the day numbers back to the combined data frame
  combined_data$Day <- date_to_day[as.character(combined_data$Date)]

  # Drop the Date column as not needed
  combined_data$Date <- NULL

  # Add an Index column
  combined_data$Index <- seq_len(nrow(combined_data))

  return(combined_data)
}