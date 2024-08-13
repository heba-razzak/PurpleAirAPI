#' getPurpleairApiHistoryV2
#'
#' R function to download historical data from PurpleAir sensors from API.
#' Adapted from the getPurpleairApiHistory function
#' Source: https://github.com/willianflores/getPurpleairApiHistory
#'
#' @param sensorIndex PurpleAir sensor’s index.
#' @param apiReadKey PurpleAir API read key with access to historical data.
#' @param startDate The beginning date in the format "YYYY-MM-DD".
#' @param endDate The end date in the format "YYYY-MM-DD".
#' @param average The desired average in minutes, one of the following:
#' "0" (real-time), "10", "30", "60", "360" (6 hour), "1440" (1 day).
#' @param fields The "Fields" parameter specifies which 'sensor data fields' to
#' include in the response.
#' See: https://community.purpleair.com/t/api-history-fields-descriptions/
#' @param printFlag Logical. Indicates whether to print progress messages.
#'
#' @return Dataframe of PurpleAir history data of one or multiple sensors.
#'
#' @import httr
#' @import jsonlite
#' @export
#'
getPurpleairApiHistoryV2 <- function(
    sensorIndex = NULL,
    apiReadKey = NULL,
    startDate = NULL,
    endDate = NULL,
    average = NULL,
    fields = NULL,
    printFlag = FALSE
) {
  # Define required parameters
  required_params <- c("sensorIndex", "apiReadKey", "startDate", "endDate",
                       "average", "fields")

  # Loop through each parameter and check if it is NULL
  for (param in required_params) {
    if (is.null(get(param))) {
      stop(paste(param, "not defined!"))
    }
  }

  # Convert fields to a comma-separated string if it's not already
  fields <- paste(fields, collapse = ", ")

  # Convert to POSIXct and set end time to 23:59:59
  start_time <- as.POSIXct(startDate, tz = "UTC")
  end_time <- as.POSIXct(paste(endDate, "23:59:59"), tz = "UTC")

  # Calculate time difference
  t_dif <- end_time - start_time

  # Check if time period is less than 2 weeks
  if (t_dif <= as.difftime(2, units = "weeks")) {
    start_timestamps <- start_time
    end_timestamps <- end_time
  } else {
    # If time period is greater than 2 weeks, make a sequence
    # start_timestamps will be every 2 weeks
    # end_timestamps will follow start_timestamps by 2 weeks - 1 sec until end
    start_timestamps <- seq(from = start_time, to = end_time, by = "2 weeks")
    end_timestamps <- seq(from = start_time + as.difftime(2, units = "weeks")
                          - as.difftime(1, units = "secs"),
                          to = end_time, by = "2 weeks")

    # If last end timestamp from sequence is not the same as end_time,
    # add end_time to the sequence
    if (end_timestamps[-1] != end_time) {
      end_timestamps <- c(end_timestamps, end_time)
    }
  }

  # Validate average
  average_num <- as.numeric(average)
  if (!average_num %in% c(10, 30, 60, 360, 1440)) {
    stop(paste("Unsupported average value:", average,
               "\nAverage value must be 10, 30, 60, 360, 1440"))
  }

  # Loop for multiples requests in PurpleAir API

  # Initialize dataframe that will contain results
  r <- data.frame()

  # List of unique sensors
  unique_sensors <- unique(sensorIndex)
  n <- length(unique_sensors)

  # For each sensor
  for (i in 1:n) {
    sensor <- unique_sensors[i]
    if (printFlag) {
      print(paste("sensor ", sensor, ": ", i, " of ", n))
    }
    url_base <- paste0("https://api.purpleair.com/v1/sensors/",
                      sensor, "/history")

    # Download in 2 week intervals
    for (j in 1:length(start_timestamps)) {
      # Set variables
      query_list <- list(
        start_timestamp = as.character(as.integer(as.POSIXct(start_timestamps[j],
                                                             tz = "UTC"))),
        end_timestamp = as.character(as.integer(as.POSIXct(end_timestamps[j],
                                                           tz = "UTC"))),
        average = average,
        fields = fields)

      # GET PurpleAir sensor history data
      result <- httr::GET(
        url_base,
        query = query_list,
        config = httr::add_headers("X-API-Key" = apiReadKey)
      )

      # If request failed show error message and stop
      if (httr::http_error(result)) {
        error_content <- httr::content(result, as = "text", encoding = "UTF-8")
        error_details <- jsonlite::fromJSON(error_content)
        error_message <- paste(
          "HTTP request failed with status:", httr::status_code(result), "\n",
          "Error type:", error_details$error, "\n",
          "Description:", error_details$description
        )
        stop(error_message)
      }

      # Convert the raw content returned by the API into a character string
      raw <- httr::content(result, as = "text", encoding = "UTF-8")
      # Convert the character string into a JSON object
      response_list <- jsonlite::fromJSON(raw)

      # Extract "data" element from JSON object, convert it to data frame
      r_df <- as.data.frame(response_list$data)

      # If dataframe is not empty
      if (nrow(r_df) > 0) {
        # Column names
        names(r_df) <- response_list$fields

        # Convert epoch to datetime format
        r_df$time_stamp <- format(as.POSIXct(as.integer(r_df$time_stamp),
                                                    origin = "1970-01-01",
                                                    tz = "UTC"),
                                         format = "%Y-%m-%d %H:%M:%S")

        # Order by date
        r_df <- r_df[order(r_df$time_stamp), ]

        # Add sensor_index
        r_df$sensor_index <- sensor
      }

      # Add to the result data frame
      r <- rbind(r, r_df)
      }
    }

  if (nrow(r) > 0) {
    # col_names of fields
    col_names <- strsplit(fields, ", ")[[1]]

    # drop rows where "fields" are empty
    r <- r[stats::complete.cases(r[, col_names]), ]
  }

  return(r)
}

#' Get PurpleAir Sensor Data
#'
#' Retrieves data from PurpleAir sensors based on specified fields.
#'
#' @param apiReadKey API key for accessing the PurpleAir API.
#' @param fields Vector specifying the fields to retrieve from PurpleAir API.
#'
#'               Default: c("latitude", "longitude",
#'                           "date_created", "last_seen")
#'
#' @return A data frame containing the required fields for all purpleair sensors
#' @import httr
#' @import jsonlite
#' @export
#'
#' @examples
#' getPurpleairSensors()
#' getPurpleairSensors(apiReadKey = "your_api_key",
#'                     fields = c("latitude", "longitude"))
getPurpleairSensors <- function(
    apiReadKey = NULL,
    fields = c("latitude", "longitude", "date_created", "last_seen")
) {
  api_endpoint <- paste0("https://api.purpleair.com/v1/sensors?fields=",
                         paste(fields, collapse = "%2C"))

  # Get Purple Air data using the following steps
  # Make the HTTP request to the PurpleAir API using the GET function
  result <- httr::GET(
    api_endpoint,
    config = httr::add_headers("X-API-Key" = apiReadKey)
  )

  # If request failed show error message and stop
  if (httr::http_error(result)) {
    error_content <- httr::content(result, as = "text", encoding = "UTF-8")
    error_details <- jsonlite::fromJSON(error_content)
    error_message <- paste(
      "HTTP request failed with status:", httr::status_code(result), "\n",
      "Error type:", error_details$error, "\n",
      "Description:", error_details$description
    )
    stop(error_message)
  }

  # Convert the raw content returned by the API into a character string
  raw <- httr::content(result, as = "text", encoding = "UTF-8")

  # Convert the character string into a JSON object
  response_list <- jsonlite::fromJSON(raw)

  # Extract "data" element from JSON object and convert it to data frame
  purpleair <- as.data.frame(response_list$data)
  names(purpleair) <- response_list$fields

  # Convert date columns to Date format if they exist
  date_cols <- c("date_created", "last_seen")
  for (col in date_cols) {
    if (col %in% names(purpleair)) {
      purpleair[[col]] <- as.Date(as.POSIXct(purpleair[[col]],
                                             origin = "1970-01-01"))
    }
  }

  return(purpleair)
}
