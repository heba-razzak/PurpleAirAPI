test_that("getPurpleairSensors validates parameters correctly", {
  expect_error(getPurpleairSensors(), "apiReadKey not defined!")
  expect_error(getPurpleairSensors(apiReadKey = NULL), "apiReadKey not defined!")
})

# Test case 1: Validate parameters without requiring an API key
test_that("getSensorHistory validates parameters without API key", {
  # Missing sensorIndex
  expect_error(getSensorHistory(), "sensorIndex not defined!")
  
  # Provided sensorIndex but missing API key
  expect_error(getSensorHistory(sensorIndex = 182), "apiReadKey not defined!")
  
  # Invalid date range
  expect_error(
    getSensorHistory(
      sensorIndex = 182,
      apiReadKey = "key",
      startDate = "2024-01-01",
      endDate = "2023-12-31",
      average = 60,
      fields = c("pm2.5")
    ),
    "Error: startDate must be earlier than endDate"
  )
})

test_that("getSensorHistory validates parameters with API key", {
  skip_on_cran()  # Skip this test on CRAN
  
  # Retrieve API key from environment
  api_key <- Sys.getenv("PURPLEAIR_API_KEY")
  
  # Skip test if the API key is not set
  skip_if(api_key == "", "API key not set in environment")
  
  # Invalid average value
  expect_error(
    getSensorHistory(
      sensorIndex = 182,
      apiReadKey = api_key,
      startDate = "2024-01-01",
      endDate = "2024-01-31",
      average = 45,  # Invalid average
      fields = c("pm2.5")
    ),
    "Unsupported average value: 45"
  )
})
