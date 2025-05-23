# Test suite for get_sealevel function
test_that("get_sealevel returns correct data structure", {
  # Test with default gauge
  result <- get_sealevel()
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check column names
  expected_cols <- c("gauge", "Year", "Month", "date", "msl_m", "msl_ft")
  expect_equal(names(result), expected_cols)
  
  # Check that all expected columns are present
  expect_true(all(expected_cols %in% names(result)))
  
  # Check data types
  expect_type(result$gauge, "double")
  expect_type(result$Year, "integer")
  expect_type(result$Month, "integer")
  expect_type(result$msl_m, "double")
  expect_s3_class(result$date, "Date")
  expect_type(result$msl_ft, "double")
})
