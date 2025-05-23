library(ggplot2)

# Create sample data for testing (mimics get_scenario output)
create_sample_scenario_data <- function() {
  scenarios <- c("NOAA Intermediate Low", "NOAA Intermediate", "NOAA Intermediate High")
  years <- 2020:2030
  
  # Create expanded grid of all combinations
  expanded <- expand.grid(
    scenario = scenarios,
    year = years,
    stringsAsFactors = FALSE
  )
  
  data.frame(
    id = rep(520, nrow(expanded)),
    scenario = factor(expanded$scenario, levels = scenarios),
    year = expanded$year,
    slr_m = runif(nrow(expanded), 0, 0.5),
    slr_ft = runif(nrow(expanded), 0, 1.5),
    stringsAsFactors = FALSE
  )
}

# Test suite for plot_scenario function
test_that("plot_scenario returns a ggplot object with defaults", {
  dat <- create_sample_scenario_data()
  p <- plot_scenario(dat)
  
  # Check that result is a ggplot object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_scenario handles units parameter correctly", {
  dat <- create_sample_scenario_data()
  
  # Test feet units (default)
  p_ft <- plot_scenario(dat, units = "ft")
  expect_equal(p_ft$labels$y, "RSLC in Feet")
  
  # Test meter units
  p_m <- plot_scenario(dat, units = "m")
  expect_equal(p_m$labels$y, "RSLC in Meters")
  
  # Test invalid units
  expect_error(plot_scenario(dat, units = "invalid"), 
               "'arg' should be one of")
})

test_that("plot_scenario handles color palette correctly", {
  dat <- create_sample_scenario_data()
  
  # Test default colors
  p_default <- plot_scenario(dat)
  expect_s3_class(p_default, "ggplot")
  
  # Test custom colors
  custom_cols <- c("red", "blue", "green")
  p_custom <- plot_scenario(dat, cols = custom_cols)
  expect_s3_class(p_custom, "ggplot")
  
  # Test single color (should be expanded by colorRampPalette)
  p_single <- plot_scenario(dat, cols = "purple")
  expect_s3_class(p_single, "ggplot")
  
  # The plot should build without error
  expect_no_error(ggplot_build(p_custom))
})

test_that("plot_scenario handles caption parameter", {
  dat <- create_sample_scenario_data()
  
  # Test with caption (default)
  p_with_caption <- plot_scenario(dat, caption = TRUE)
  expect_false(is.null(p_with_caption$labels$caption))
  expect_true(grepl("Source:", p_with_caption$labels$caption))
  expect_true(grepl("sealevel.nasa.gov", p_with_caption$labels$caption))
  
  # Test without caption
  p_no_caption <- plot_scenario(dat, caption = FALSE)
  expect_null(p_no_caption$labels$caption)
})

test_that("plot_scenario handles x-axis range parameter", {
  dat <- create_sample_scenario_data()
  
  # Test with default xrng
  p_default <- plot_scenario(dat, xrng = c(2020, 2100))
  expect_s3_class(p_default, "ggplot")
  
  # Test with custom xrng
  custom_xrng <- c(2025, 2030)
  p_custom <- plot_scenario(dat, xrng = custom_xrng)
  
  # Check that custom range filters the data
  expect_true(all(p_custom$data$year >= custom_xrng[1]))
  expect_true(all(p_custom$data$year <= custom_xrng[2]))
  
  # Check that limits are set correctly
  built_plot <- ggplot_build(p_custom)
  x_limits <- built_plot$layout$panel_scales_x[[1]]$limits
  expect_equal(x_limits, custom_xrng)
})

test_that("plot_scenario handles y-axis range parameter", {
  dat <- create_sample_scenario_data()
  
  # Test with default yrng (NULL) - should start from 0
  p_default <- plot_scenario(dat, yrng = NULL)
  built_default <- ggplot_build(p_default)
  y_limits_default <- built_default$layout$panel_scales_y[[1]]$limits
  expect_equal(y_limits_default[1], 0)  # Should start from 0
  
  # Test with custom yrng
  custom_yrng <- c(-0.5, 2.0)
  p_custom <- plot_scenario(dat, yrng = custom_yrng)
  
  # Check that limits were set
  built_plot <- ggplot_build(p_custom)
  y_limits <- built_plot$layout$panel_scales_y[[1]]$limits
  expect_equal(y_limits, custom_yrng)
})