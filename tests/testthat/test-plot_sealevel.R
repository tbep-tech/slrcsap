library(ggplot2)

# Create sample data for testing (mimics get_sealevel output)
create_sample_data <- function() {
  data.frame(
    gauge = rep(8726520, 24),
    Year = rep(2020:2021, each = 12),
    Month = rep(1:12, 2),
    msl_m = runif(24, -0.1, 0.1),
    date = seq(as.Date("2020-01-01"), as.Date("2021-12-01"), by = "month"),
    msl_ft = runif(24, -0.3, 0.3),
    stringsAsFactors = FALSE
  )
}

# Test suite for plot_sealevel function
test_that("plot_sealevel returns a ggplot object with defaults", {
  dat <- create_sample_data()
  p <- plot_sealevel(dat)
  
  # Check that result is a ggplot object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_sealevel handles units parameter correctly", {
  dat <- create_sample_data()
  
  # Test feet units (default)
  p_ft <- plot_sealevel(dat, units = "ft")
  expect_equal(p_ft$labels$y, "Feet (MSL)")
  
  # Test meter units
  p_m <- plot_sealevel(dat, units = "m")
  expect_equal(p_m$labels$y, "Meters (MSL)")
  
  # Test invalid units
  expect_error(plot_sealevel(dat, units = "invalid"), 
               "'arg' should be one of")
})

test_that("plot_sealevel handles caption parameter", {
  dat <- create_sample_data()
  
  # Test with caption (default)
  p_with_caption <- plot_sealevel(dat, caption = TRUE)
  expect_false(is.null(p_with_caption$labels$caption))
  expect_true(grepl("Source:", p_with_caption$labels$caption))
  expect_true(grepl("tidesandcurrents.noaa.gov", p_with_caption$labels$caption))
  
  # Test without caption
  p_no_caption <- plot_sealevel(dat, caption = FALSE)
  expect_null(p_no_caption$labels$caption)
})

test_that("plot_sealevel handles x-axis range parameter", {
  dat <- create_sample_data()
  
  # Test with default xrng (NULL)
  p_default <- plot_sealevel(dat, xrng = NULL)
  built_plot <- ggplot_build(p_default)
  
  # Test with custom xrng
  custom_xrng <- as.Date(c("2020-06-01", "2020-12-01"))
  p_custom <- plot_sealevel(dat, xrng = custom_xrng)
  
  # Check that custom range filters the data
  expect_true(all(p_custom$data$date >= custom_xrng[1]))
  expect_true(all(p_custom$data$date <= custom_xrng[2]))
  
  # Test invalid xrng (not Date)
  expect_error(plot_sealevel(dat, xrng = c("2020-01-01", "2020-12-01")),
               "inherits\\(xrng, \"Date\"\\)")
})

test_that("plot_sealevel handles y-axis range parameter", {
  dat <- create_sample_data()
  
  # Test with default yrng (NULL)
  p_default <- plot_sealevel(dat, yrng = NULL)
  expect_s3_class(p_default, "ggplot")
  
  # Test with custom yrng
  custom_yrng <- c(-0.5, 0.5)
  p_custom <- plot_sealevel(dat, yrng = custom_yrng)
  
  # Check that limits were set
  built_plot <- ggplot_build(p_custom)
  y_limits <- built_plot$layout$panel_scales_y[[1]]$limits
  expect_equal(y_limits, custom_yrng)
})

test_that("plot_sealevel handles plotly output", {
  
  dat <- create_sample_data()
  
  # Test with plotly output
  p_plotly <- plot_sealevel(dat, plotly = TRUE)
  expect_s3_class(p_plotly, "plotly")
  
})