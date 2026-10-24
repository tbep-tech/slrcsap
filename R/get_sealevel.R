#' Get monthly Mean Sea Level (MSL) data for selected gauge
#'
#' @param gauge numeric, gauge number. Default is 8726520 (St. Petersburg, FL).
#'
#' @returns A data frame with columns for gauge, year, month, date, MSL in meters, and MSL in feet.
#' 
#' @details Information from <https://tidesandcurrents.noaa.gov/trends-and-extremes/> using the NOAA CO-OPS Derived Product API <https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/sealvltrends.json?station=8726520&details=monthlymeans&units=metric&trendType=single>, by default.  Results are monthly Mean Sea Level (MSL) in meters and feet, with the seasonal cycle removed. Months with no data are not returned.
#' 
#' @export
#'
#' @examples
#' dat <- get_sealevel(gauge = 8726520)
#' head(dat)
get_sealevel <- function(gauge = 8726520){
  
  url <- paste0(
    'https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/sealvltrends.json?station=', 
    gauge, '&details=monthlymeans&units=metric&trendType=single'
  )
  
  out <- jsonlite::fromJSON(url)$data |> 
    dplyr::select(Year = year, Month = month, msl_m = mslDeseasonalized) |> 
    dplyr::filter(!is.na(msl_m)) |> 
    dplyr::mutate(
      Year = as.integer(Year),
      Month = as.integer(Month),
      gauge = !!gauge,
      date = as.Date(paste(Year, Month, 1), format = '%Y %m %d'),
      msl_ft = msl_m * 3.28084
    ) |> 
    dplyr::select(gauge, Year, Month, date, msl_m, msl_ft)
  
  return(out)

}
