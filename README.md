# PurpleAirAPI

`PurpleAirAPI` is an R package that provides functions to access historical data from PurpleAir sensors through their API. This package allows you to download, process, and analyze air quality data collected by PurpleAir sensors.

# Installation

To install the package, you can use `devtools` to install it directly from GitHub:

```r
# Install PurpleAirAPI from GitHub
devtools::install_github("heba-razzak/PurpleAirAPI")
```

# Getting Started

Here are some examples to help you get started with the `PurpleAirAPI` package.

## Load the Package

```r
library(PurpleAirAPI)
```

## Example 1: Get PurpleAir Sensor Data

The `getPurpleairSensors` function retrieves metadata about PurpleAir sensors, including their locations and the dates they were created and last seen.

```r
# Define your PurpleAir API read key
api_key <- "your_api_key_here"

# Get sensor data
sensor_data <- getPurpleairSensors(apiReadKey = api_key)

# View the first few rows of the sensor data
head(sensor_data)

  sensor_index date_created  last_seen latitude longitude
1           53   2016-02-04 2024-08-13 40.24674 -111.7048
2           77   2016-03-02 2024-08-13 40.75082 -111.8253
3          182   2016-08-01 2024-08-13 49.16008 -123.7423
4          195   2016-08-01 2024-08-13 41.06000 -124.1288
5          286   2016-09-06 2024-08-13 49.48426 -124.2666
6          314   2016-09-15 2024-08-13 39.43402 -104.7324
```

## Example 2: Download Historical Data from a PurpleAir Sensor

The `getSensorHistory` function allows you to download historical air quality data from a specific PurpleAir sensor.

```r
# Define your PurpleAir API read key and the sensor index
api_key <- "your_api_key_here"
sensor_index <- 2858

# Define the date range and the fields to retrieve
start_date <- "2018-01-01"
end_date <- "2018-03-31"
average <- "60" # Hourly averages
fields <- c("pm2.5_atm", "pm2.5_atm_a", "pm2.5_atm_b")

# Download historical data
historical_data <- getSensorHistory(
  sensorIndex = sensor_index,
  apiReadKey = api_key,
  startDate = start_date,
  endDate = end_date,
  average = average,
  fields = fields
)

# View the first few rows of the historical data
head(historical_data)

     time_stamp          pm2.5_atm pm2.5_atm_a pm2.5_atm_b sensor_index
1 2018-01-01 00:00:00      50.318      49.301      51.335         2858
2 2018-01-01 01:00:00      48.519      46.951      50.087         2858
3 2018-01-01 02:00:00      48.688      47.034      50.341         2858
4 2018-01-01 03:00:00      51.705      49.693      53.716         2858
5 2018-01-01 04:00:00      53.279      51.212      55.346         2858
6 2018-01-01 05:00:00      48.749      46.797      50.700         2858
```

# Additional Information

For more details on the available fields, API documentation, and other features of the PurpleAir API, please refer to the [PurpleAir API documentation](https://api.purpleair.com/).

## Fields Available for Historical Data

For a full list of fields that can be used with historical data, refer to the [PurpleAir API documentation on sensor history](https://api.purpleair.com/#api-sensors-get-sensor-history-csv).

## Fields Available for Sensor Data

For a full list of fields that can be used with sensor data, refer to the [PurpleAir API documentation on sensor data](https://api.purpleair.com/#api-sensors-get-sensor-data).
