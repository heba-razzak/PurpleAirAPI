#' purpleAirAPI
#'
#' R function to download historical data from PurpleAir sensors from API.
#'
#' @param sensorIndex PurpleAir sensor’s index.
#' @param apiReadKey PurpleAir API read key.
#' @param startDate Date of the first required history entry.
#' @param endDate Date of the history to return. Uses end of date specified.
#' @param average The desired average in minutes: 0, 10, 30, 60, 360, 1440, 10080, 43200, 525600
#' @param fields The 'sensor data fields' to include in the response.
#'
#' @return Dataframe of PurpleAir history data of one or multiple sensors.
#'
#' @details Fields marked with an asterisk (*) may not be available when using averages.
#'
#' Station information and status fields:
#' \itemize{
#'   \item hardware*, latitude*, longitude*, altitude*, firmware_version*, private, rssi, uptime, pa_latency, memory
#' }
#'
#' Environmental fields:
#' \itemize{
#'   \item humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b
#' }
#'
#' Miscellaneous fields:
#' \itemize{
#'   \item voc, voc_a, voc_b, analog_input
#' }
#'
#' PM1.0 fields:
#' \itemize{
#'   \item pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b
#' }
#'
#' PM2.5 fields:
#' \itemize{
#'   \item pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b
#' }
#'
#' PM10.0 fields:
#' \itemize{
#'   \item pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b
#' }
#'
#' Visibility fields:
#' \itemize{
#'   \item scattering_coefficient, scattering_coefficient_a, scattering_coefficient_b, deciviews, deciviews_a, deciviews_b, visual_range, visual_range_a, visual_range_b
#' }
#'
#' Particle count fields:
#' \itemize{
#'   \item 0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count, 10.0_um_count_a, 10.0_um_count_b
#' }
#'
#' @references For more details on the available fields, see the \href{https://api.purpleair.com/#api-sensors-get-sensor-history-csv}{PurpleAir API documentation}.
#'
#' @import httr
#' @import jsonlite
#' @export
#'
getSensorHistory <- function(
    sensorIndex = NULL,
    apiReadKey = NULL,
    startDate = NULL,
    endDate = NULL,
    average = NULL,
    fields = NULL) {
  # Define required parameters
  required_params <- c(
    "sensorIndex", "apiReadKey", "startDate", "endDate",
    "average", "fields"
  )

  # Loop through each parameter and check if it is NULL
  for (param in required_params) {
    if (is.null(get(param))) {
      stop(paste(param, "not defined!"))
    }
  }

  # Convert to POSIXct and set end time to 23:59:59
  start_time <- as.POSIXct(startDate, tz = "UTC")
  end_time <- as.POSIXct(paste(endDate, "23:59:59"), tz = "UTC")

  # Ensure that start time is less than end time
  if (start_time >= end_time) {
    stop("Error: startDate must be earlier than endDate")
  }

  # Validate API key
  validate_api_key(apiReadKey)

  # Convert fields to a comma-separated string if it's not already
  fields <- paste(fields, collapse = ", ")

  # Validate average
  average <- as.numeric(average)

  # Time limit (in days) for each average value
  time_limits <- c(30, 60, 90, 180, 365, 730, 1825, 7300, 36500)

  # Possible values for average
  average_values <- c(0, 10, 30, 60, 360, 1440, 10080, 43200, 525600)

  names(time_limits) <- average_values

  if (!average %in% average_values) {
    stop(paste(
      "Unsupported average value:", average,
      "\nAverage value must be 0, 10, 30, 60, 360, 1440, 10080, 43200, 525600"
    ))
  }

  # Determine the time limit based on the average
  max_days <- as.integer(time_limits[as.character(average)])
  max_interval <- as.difftime(max_days, units = "days")

  # Calculate time difference
  download_interval <- end_time - start_time

  # Check if time period is less than the max time limit
  if (download_interval <= max_interval) {
    start_timestamps <- start_time
    end_timestamps <- end_time
  } else {
    start_timestamps <- seq(from = start_time, to = end_time, by = max_interval)
    end_timestamps <- start_timestamps + max_interval

    # Cap end_timestamps with end_time
    end_timestamps <- pmin(end_timestamps, end_time)
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
    message(paste("sensor ", sensor, ": ", i, " of ", n))
    url_base <- paste0(
      "https://api.purpleair.com/v1/sensors/",
      sensor, "/history"
    )

    # Download using maximum intervals
    for (j in 1:length(start_timestamps)) {
      # Set variables
      query_list <- list(
        start_timestamp = as.character(as.integer(as.POSIXct(start_timestamps[j],
          tz = "UTC"
        ))),
        end_timestamp = as.character(as.integer(as.POSIXct(end_timestamps[j],
          tz = "UTC"
        ))),
        average = average,
        fields = fields
      )

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
          httr::status_code(result),
          error_details$error, error_details$description
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
        r_df$time_stamp <- format(
          as.POSIXct(as.integer(r_df$time_stamp),
            origin = "1970-01-01",
            tz = "UTC"
          ),
          format = "%Y-%m-%d %H:%M:%S"
        )

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

    # Drop rows where all of the "fields" are empty
    r <- r[rowSums(is.na(r[, col_names])) != length(col_names), ]

    # Reset index
    rownames(r) <- NULL
  }

  return(r)
}

#' Get PurpleAir Sensor Data
#'
#' Retrieves data from PurpleAir sensors based on specified fields.
#'
#' @param apiReadKey API key for accessing the PurpleAir API.
#' @param fields Vector specifying the fields to retrieve from PurpleAir API.
#'               Default: c("latitude", "longitude", "date_created", "last_seen")
#'
#' @return A data frame containing the required fields for all PurpleAir sensors
#'
#' @details Available fields:
#'
#' \strong{Station information and status fields:}
#' \itemize{
#'   \item \code{name}, \code{icon}, \code{model}, \code{hardware}, \code{location_type}, \code{private}, \code{latitude}, \code{longitude}, \code{altitude}, \code{position_rating}, \code{led_brightness}, \code{firmware_version}, \code{firmware_upgrade}, \code{rssi}, \code{uptime}, \code{pa_latency}, \code{memory}, \code{last_seen}, \code{last_modified}, \code{date_created}, \code{channel_state}, \code{channel_flags}, \code{channel_flags_manual}, \code{channel_flags_auto}, \code{confidence}, \code{confidence_manual}, \code{confidence_auto}
#' }
#'
#' \strong{Environmental fields:}
#' \itemize{
#'   \item \code{humidity}, \code{humidity_a}, \code{humidity_b}, \code{temperature}, \code{temperature_a}, \code{temperature_b}, \code{pressure}, \code{pressure_a}, \code{pressure_b}
#' }
#'
#' \strong{Miscellaneous fields:}
#' \itemize{
#'   \item \code{voc}, \code{voc_a}, \code{voc_b}, \code{ozone1}, \code{analog_input}
#' }
#'
#' \strong{PM1.0 fields:}
#' \itemize{
#'   \item \code{pm1.0}, \code{pm1.0_a}, \code{pm1.0_b}, \code{pm1.0_atm}, \code{pm1.0_atm_a}, \code{pm1.0_atm_b}, \code{pm1.0_cf_1}, \code{pm1.0_cf_1_a}, \code{pm1.0_cf_1_b}
#' }
#'
#' \strong{PM2.5 fields:}
#' \itemize{
#'   \item \code{pm2.5_alt}, \code{pm2.5_alt_a}, \code{pm2.5_alt_b}, \code{pm2.5}, \code{pm2.5_a}, \code{pm2.5_b}, \code{pm2.5_atm}, \code{pm2.5_atm_a}, \code{pm2.5_atm_b}, \code{pm2.5_cf_1}, \code{pm2.5_cf_1_a}, \code{pm2.5_cf_1_b}
#' }
#'
#' \strong{PM2.5 pseudo (simple running) average fields:}
#' \itemize{
#'   \item \code{pm2.5_10minute}, \code{pm2.5_10minute_a}, \code{pm2.5_10minute_b}, \code{pm2.5_30minute}, \code{pm2.5_30minute_a}, \code{pm2.5_30minute_b}, \code{pm2.5_60minute}, \code{pm2.5_60minute_a}, \code{pm2.5_60minute_b}, \code{pm2.5_6hour}, \code{pm2.5_6hour_a}, \code{pm2.5_6hour_b}, \code{pm2.5_24hour}, \code{pm2.5_24hour_a}, \code{pm2.5_24hour_b}, \code{pm2.5_1week}, \code{pm2.5_1week_a}, \code{pm2.5_1week_b}
#' }
#'
#' \strong{PM10.0 fields:}
#' \itemize{
#'   \item \code{pm10.0}, \code{pm10.0_a}, \code{pm10.0_b}, \code{pm10.0_atm}, \code{pm10.0_atm_a}, \code{pm10.0_atm_b}, \code{pm10.0_cf_1}, \code{pm10.0_cf_1_a}, \code{pm10.0_cf_1_b}
#' }
#'
#' \strong{Particle count fields:}
#' \itemize{
#'   \item \code{0.3_um_count}, \code{0.3_um_count_a}, \code{0.3_um_count_b}, \code{0.5_um_count}, \code{0.5_um_count_a}, \code{0.5_um_count_b}, \code{1.0_um_count}, \code{1.0_um_count_a}, \code{1.0_um_count_b}, \code{2.5_um_count}, \code{2.5_um_count_a}, \code{2.5_um_count_b}, \code{5.0_um_count}, \code{5.0_um_count_a}, \code{5.0_um_count_b}, \code{10.0_um_count}, \code{10.0_um_count_a}, \code{10.0_um_count_b}
#' }
#'
#' \strong{ThingSpeak fields (for data retrieval from api.thingspeak.com):}
#' \itemize{
#'   \item \code{primary_id_a}, \code{primary_key_a}, \code{secondary_id_a}, \code{secondary_key_a}, \code{primary_id_b}, \code{primary_key_b}, \code{secondary_id_b}, \code{secondary_key_b}
#' }
#'
#' @references For more details on the available fields, see the \href{https://api.purpleair.com/#api-sensors-get-sensor-data}{PurpleAir API documentation}.
#'
#' @import httr
#' @import jsonlite
#' @export
#'
getPurpleairSensors <- function(
    apiReadKey = NULL,
    fields = c("latitude", "longitude", "date_created", "last_seen")) {
  # Check if required parameters
  if (is.null("apiReadKey")) {
    stop(paste("apiReadKey not defined!"))
  }
  # Define required parameters
  required_params <- c("apiReadKey")

  # Loop through each parameter and check if it is NULL
  for (param in required_params) {
    if (is.null(get(param))) {
      stop(paste(param, "not defined!"))
    }
  }

  # Validate API key
  validate_api_key(apiReadKey)

  api_endpoint <- paste0(
    "https://api.purpleair.com/v1/sensors?fields=",
    paste(fields, collapse = "%2C")
  )

  # GET PurpleAir sensor data
  result <- httr::GET(
    api_endpoint,
    config = httr::add_headers("X-API-Key" = apiReadKey)
  )

  # If request failed show error message and stop
  if (httr::http_error(result)) {
    error_content <- httr::content(result, as = "text", encoding = "UTF-8")
    error_details <- jsonlite::fromJSON(error_content)
    error_message <- paste(
      httr::status_code(result),
      error_details$error, error_details$description
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
        origin = "1970-01-01"
      ))
    }
  }

  return(purpleair)
}
#' Helper function to validate the API key
#'
#' @param apiReadKey
#' @keywords internal
validate_api_key <- function(apiReadKey) {
  apikey_check <- "https://api.purpleair.com/v1/keys"
  apikey_response <- httr::GET(apikey_check, config = httr::add_headers("X-API-Key" = apiReadKey))
  if (httr::http_error(apikey_response)) {
    error_content <- httr::content(apikey_response, as = "text", encoding = "UTF-8")
    error_details <- jsonlite::fromJSON(error_content)
    error_message <- paste(httr::status_code(apikey_response), error_details$error, error_details$description)
    stop(error_message)
  }
}
