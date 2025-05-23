#' Get Sea Level Rise (SLR) scenario
#'
#' @param id numeric, gauge number. Default is 520 (St. Petersburg, FL).
#' @param scenario character, SLR scenario. Default is `'IntLow'`, `'Int'`, and `'IntHigh'` (NOAA Intermediate Low, Intermediate, and Intermediate High). See details for available options.
#' @param method character, download method. Default is `'curl'`, passed to `download.file()`.
#' @param quiet logical, suppress download messages. Default is `TRUE`.
#'
#' @returns A data frame with columns for id, scenario, year, SLR in meters, and SLR in feet.
#' 
#' @details Information from <https://sealevel.nasa.gov/task-force-scenario-tool?psmsl_id=520>, by default. Results are SLR in meters and feet for the intermediate low, intermediate, and intermediate high scenarios based on recommended scenarios from the Climate Science Advisory Panel.  Full options for scenarios are `'Low'`, `'IntLow'`, `'Int'`, `'IntHigh'`, and `'High'`. Values for SLR are relative change from 2020.
#' 
#' @export
#'
#' @examples
#' dat <- get_scenario()
#' head(dat)
get_scenario <- function(id = 520, scenario = c('IntLow', 'Int', 'IntHigh'), method = 'curl',
                         quiet = TRUE){
  
  scenarios <- list(
    Low = 'NOAA Low',
    IntLow = 'NOAA Intermediate Low',
    Int = 'NOAA Intermediate',
    IntHigh = 'NOAA Intermediate High', 
    High = 'NOAA High'
  )
  
  # check that scenario is valid
  chk <- scenario %in% names(scenarios)
  if(any(!chk)){
    stop(paste0('Invalid scenario(s): ', paste(scenario[!chk], collapse = ', ')))
  }
  
  # download url
  url <- paste0('https://d3qt3aobtsas2h.cloudfront.net/edge/ws/search/projection?psmsl_id=', id, '&format=csv&task_force=true')

  # create temp file and download
  temp <- tempfile(fileext = '.xlsx')
  download.file(url, destfile = temp, method = method, quiet = quiet)

  # units in mm, convert to feet
  # select scenarios
  out <- readxl::read_excel(temp, sheet = 'Total') |> 
    dplyr::filter(scenario %in% !!scenario) |> 
    dplyr::filter(quantile == 50) |> 
    dplyr::select(-psmsl_id, -process, -Units, -quantile) |> 
    tidyr::pivot_longer(cols = -scenario, names_to = 'year', values_to = 'slr_mm') |>
    dplyr::mutate(
      id = id,
      year = as.numeric(year),
      slr_m = slr_mm / 1000, # convert mm to meters
      slr_ft = slr_mm / 304.8, # convert mm to feet
      scenario = factor(scenario, levels = scenario, 
                        labels = scenarios[scenario])
    ) |> 
    dplyr::select(id, scenario, year, slr_m, slr_ft)
  
  unlink(temp)
  
  return(out)
  
}