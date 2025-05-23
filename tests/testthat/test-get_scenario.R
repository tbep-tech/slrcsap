# Test suite for get_scenario function
test_that("get_scenario returns correct data structure with defaults", {
  # Test with all default parameters
  result <- get_scenario()
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check column names
  expected_cols <- c("id", "scenario", "year", "slr_m", "slr_ft")
  expect_equal(names(result), expected_cols)
  
  # Check data types
  expect_type(result$id, "double")
  expect_s3_class(result$scenario, "factor")
  expect_type(result$year, "double")
  expect_type(result$slr_m, "double")
  expect_type(result$slr_ft, "double")
  
  # Should have data
  expect_gt(nrow(result), 0)
})

test_that("get_scenario validates scenario parameter correctly", {
  # Test invalid scenarios
  expect_error(get_scenario(scenario = "InvalidScenario"),
               "Invalid scenario\\(s\\): InvalidScenario")
  
  # Test mix of valid and invalid scenarios
  expect_error(get_scenario(scenario = c("Int", "InvalidScenario", "AnotherInvalid")),
               "Invalid scenario\\(s\\): InvalidScenario, AnotherInvalid")
  
})
