% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PurpleAirAPI.R
\name{getSensorHistory}
\alias{getSensorHistory}
\title{getSensorHistory}
\usage{
getSensorHistory(
  sensorIndex = NULL,
  apiReadKey = NULL,
  startDate = NULL,
  endDate = NULL,
  average = NULL,
  fields = NULL
)
}
\arguments{
\item{sensorIndex}{PurpleAir sensor's index.}

\item{apiReadKey}{PurpleAir API read key.}

\item{startDate}{Date of the first required history entry.}

\item{endDate}{Date of the history to return. Uses end of date specified.}

\item{average}{The desired average in minutes: 0, 10, 30, 60, 360, 1440, 
10080, 43200, 525600}

\item{fields}{The 'sensor data fields' to include in the response.}
}
\value{
Dataframe of PurpleAir history data of one or multiple sensors.
}
\description{
Download historical data for PurpleAir sensors using API.
}
\details{
Available fields:\tabular{ll}{
  \code{Station Information and Status} \tab hardware*, latitude*, 
  longitude*, altitude*, firmware_version*, private, rssi, uptime,
   pa_latency, memory \cr
  \tab \cr
  \code{Environmental} \tab humidity, humidity_a, humidity_b, temperature,
   temperature_a, temperature_b, pressure, pressure_a, pressure_b \cr
  \tab \cr
  \code{Miscellaneous} \tab voc, voc_a, voc_b, analog_input \cr
  \tab \cr
  \code{PM1.0} \tab pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1,
   pm1.0_cf_1_a, pm1.0_cf_1_b \cr
  \tab \cr
  \code{PM2.5} \tab pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5_atm, 
  pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b \cr
  \tab \cr
  \code{PM10.0} \tab pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, 
  pm10.0_cf_1_a, pm10.0_cf_1_b \cr
  \tab \cr
  \code{Visibility} \tab scattering_coefficient, scattering_coefficient_a, 
  scattering_coefficient_b, deciviews, deciviews_a, deciviews_b, 
  visual_range, visual_range_a, visual_range_b \cr
  \tab \cr
  \code{Particle Count} \tab 0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 
  0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a,
   1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 
   5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count, 
   10.0_um_count_a, 10.0_um_count_b \cr
}
}
\examples{
\dontrun{
# Download hourly PM2.5 data for one month
history <- getSensorHistory(
  sensorIndex = c(6127, 9014, 3124),
  apiReadKey = "YOUR_API_KEY",
  startDate = "2024-01-01",
  endDate = "2024-01-31",
  average = 60,
  fields = c("pm2.5_alt", "temperature", "humidity")
)
}

}
\references{
For more details on the available fields, see the PurpleAir API 
documentation:
\url{https://api.purpleair.com/#api-sensors-get-sensor-history-csv}
}
