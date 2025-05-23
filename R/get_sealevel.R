#' Get monthly Mean Sea Level (MSL) data for selected gauge
#'
#' @param gauge numeric, gauge number. Default is 8726520 (St. Petersburg, FL).
#'
#' @returns A data frame with columns for gauge, year, month, date, MSL in meters, and MSL in feet.
#' 
#' @details Information from <https://tidesandcurrents.noaa.gov> using the URL <https://tidesandcurrents.noaa.gov/sltrends/data/8726520_meantrend.txt>, by default.  Results are monthly Mean Sea Level (MSL) in meters and feet, with the seasonal cycle removed. 
#' 
#' @export
#'
#' @examples
#' dat <- get_sealevel(gauge = 8726520)
#' head(dat)
get_sealevel <- function(gauge = 8726520){
  
  url <- paste0('https://tidesandcurrents.noaa.gov/sltrends/data/', gauge, '_meantrend.txt')
  
  out <- read.table(url, skip = 6, sep = '', header = F) |> 
    dplyr::select(Year = V1, Month = V2, msl_m = V3) |> 
    dplyr::mutate(
      gauge = !!gauge,
      date = as.Date(paste(Year, Month, 1), format = '%Y %m %d'),
      msl_ft = msl_m * 3.28084
    ) |> 
    dplyr::select(gauge, Year, Month, date, msl_m, msl_ft)
  
  return(out)

}