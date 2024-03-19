#' getPurpleairApiHistoryV2
#'
#' R function to download historical data from PurpleAir sensors in the newer API.
#' Adapted from the getPurpleairApiHistory function
#' Source: https://github.com/willianflores/getPurpleairApiHistory
#'
#' @param sensorIndex PurpleAir sensor’s index.
#' @param apiReadKey PurpleAir API read key with access to historical data.
#' @param startTimeStamp The beginning date in the format "YYYY-MM-DD HH:mm:ss".
#' @param endTimeStamp The end date in the format "YYYY-MM-DD" HH:mm:ss.
#' @param average The desired average in minutes, one of the following: "0" (real-time), "10", "30", "60", "360" (6 hour), "1440" (1 day).
#' @param fields The "Fields" parameter specifies which 'sensor data fields' to include in the response. See: https://community.purpleair.com/t/api-history-fields-descriptions/
#'
#' @return Dataframe of PurpleAir history data of a single sensor or multiple sensors.
#' @export
#'
getPurpleairApiHistoryV2 <- function(
    sensorIndex=NULL,
    apiReadKey=NULL,
    startTimeStamp=NULL,
    endTimeStamp=NULL,
    average = NULL,
    fields = NULL
) {
  # Define required packages
  required_packages <- c("httr", "jsonlite", "tidyverse", "lubridate", "httpcode")

  # Loop through each package and check if it is installed
  for (package in required_packages) {
    if (!require(package, character.only = TRUE)) {
      stop(paste("Package", package, "not installed. Please install it before running this script."))
    }
  }

  # Define required parameters
  required_params <- c("sensorIndex", "apiReadKey", "startTimeStamp", "endTimeStamp", "average", "fields")

  # Loop through each parameter and check if it is NULL
  for (param in required_params) {
    if (is.null(get(param))) {
      stop(paste(param, "not defined!"))
    }
  }

  # Convert to POSIXct
  startTimeStamp <- as.POSIXct(startTimeStamp, tz="UTC")
  endTimeStamp <- as.POSIXct(endTimeStamp, tz="UTC")

  # Calculate time difference
  t_dif <- endTimeStamp - startTimeStamp

  if (t_dif <= as.difftime(2, units = 'weeks')) {
    start_timestamps <- startTimeStamp
    end_timestamps <- endTimeStamp
  } else {
    # If the difference is more than 2 weeks, make a sequence
    start_timestamps <- seq(from=startTimeStamp, to=endTimeStamp, by="2 weeks")
    end_timestamps <- seq(from=startTimeStamp + as.difftime(2, units = 'weeks') - as.difftime(1, units = 'secs'),
                          to=endTimeStamp, by="2 weeks")

    if(length(start_timestamps) != length(end_timestamps)) {
      end_timestamps <- c(end_timestamps, endTimeStamp)
    }
  }

  # Convert string average to numeric
  average_num <- as.numeric(average)

  # Validate average
  if (!average_num %in% c(10, 30, 60, 360, 1440)) {
    stop(paste("Unsupported average value:", average, "\nAverage value must be 10, 30, 60, 360, 1440"))
  }

  # Compute difference of average value
  dif <- as.difftime(average_num, units = 'mins')

  # Convert timestamps once
  start_time <- lubridate::parse_date_time(format(as.POSIXct(startTimeStamp), "%b %d %Y %H:%M:%S"), tz="UTC", orders = "b d Y H:M:S")
  end_time <- lubridate::parse_date_time(format(as.POSIXct(endTimeStamp), "%b %d %Y %H:%M:%S"), tz="UTC", orders = "b d Y H:M:S")

  # Generate sequence and create data frame of timestamps
  other_df <- data.frame(time_stamp = seq(from = start_time, to = end_time, by = dif))

  # Loop for multiples requests in PurpleAir API

    # dataframe that will contain our data
    r <- data.frame()

    # number of sensors
    n <- length(sensorIndex)

    for (i in 1:n) {
      sensor <- sensorIndex[i]
      print(paste('sensor ',sensor,': ',i,' of ',n))
      URLbase <- paste0('https://api.purpleair.com/v1/sensors/',sensor, '/history')

      for (j in 1:length(start_timestamps)) {
        # Set variables
        queryList = list(
          start_timestamp = as.character(as.integer(as.POSIXct(start_timestamps[j], tz="UTC"))),
          end_timestamp = as.character(as.integer(as.POSIXct(end_timestamps[j], tz="UTC"))),
          average = average,
          fields = fields)

        # GET PurpleAir sensor history data
        r_temp <- httr::GET(
          URLbase,
          query = queryList,
          config = add_headers("X-API-Key" = apiReadKey)
        )

        # Error response
        if ( httr::http_error(r_temp) ) {  # web service failed to respond
          status_code <- httr::status_code(r_temp)
          err_msg <- sprintf(
            "web service error %s from:\n  %s\n\n%s",
            status_code,
            URLbase,
            httpcode::http_code(status_code)$explanation
          )
          stop(err_msg)

        }

        # Structurized data in form of R vectors and lists
        r_parsed <- fromJSON(content(r_temp, as="text"))

        # Data frame from JSON data
        r_dataframe <- as.data.frame(r_parsed$data)

        if (nrow(r_dataframe) == 0) {
          r_dataframe <- data.frame(matrix(ncol = length(r_parsed$fields), nrow = 1))
          names(r_dataframe) <- r_parsed$fields
          r_dataframe$time_stamp <- as.character(as.integer(as.POSIXct(start_timestamps[j], tz="UTC")))
        }else{
          names(r_dataframe) <- r_parsed$fields
        }

        # Convert datetime format
        r_dataframe$time_stamp <- as.POSIXct(as.integer(r_dataframe$time_stamp), origin="1970-01-01", tz="UTC")

        # Fill missing dates
        if (average != "0") {
          other_df$time_stamp <- as.POSIXct(other_df$time_stamp)
          r_dataframe <- suppressMessages(dplyr::full_join(other_df, r_dataframe))

        # Order by date
        r_dataframe <- r_dataframe[order(r_dataframe$time_stamp),]

        # Add sensor_id
        r_dataframe$sensor_id <- sensor

        # Add to the result data frame
        r <- rbind(r, r_dataframe)

        }
      }
    }

    # colNames of fields
    colNames <- strsplit(fields, ", ")[[1]]

    # drop rows where "fields" are empty
    r <- r[complete.cases(r[, colNames]), ]

  return(r)

}
