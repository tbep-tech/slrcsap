#' Plot sea level scenario data
#'
#' @param dat Input data from `get_scenario()`.
#' @param cols character, color palette for the lines. Default is `c('deepskyblue', 'orange', 'red')`.
#' @param units character, units for the y-axis. Default is `'ft'`. Options are `'ft'` and `'m'`.
#' @param linewidth numeric, line width. Default is `1`.
#' @param caption logical, add caption with source. Default is `TRUE`.
#' @param title logical, add title. Default is `TRUE`.
#' @param subtitle character, subtitle for the plot. Default is `'Gauge 8726520, St. Petersburg, FL'`. Use `NULL` to omit.
#' @param xrng numeric, x-axis range. Default is `c(2020, 2100)`.
#' @param xbrk numeric, x-axis breaks. Default is `10`.
#' @param yrng numeric, y-axis range. Default is `c(0, 6)`.
#' @param ybrk numeric, y-axis breaks. Default is `1`.
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' dat <- get_scenario()
#' plot_scenario(dat)
plot_scenario <- function(dat, cols = c('deepskyblue', 'orange', 'red'), units = 'ft', linewidth = 1, 
                          caption = TRUE, title = TRUE, subtitle = 'Gauge 8726520, St. Petersburg, FL',
                          xrng = c(2020, 2100), xbrk = 10, yrng = c(0, 6), ybrk = 1){
  
  units <- match.arg(units, c('ft', 'm'))
  
  ylab <- ifelse(units == 'ft', 'RSLC in Feet', 'RSLC in Meters')
  
  # create color palette
  levs <- levels(dat$scenario)
  colval <- grDevices::colorRampPalette(cols)(length(levs))
  
  colnm <- names(dat)[grepl(paste0('slr_', units), names(dat))]
  
  toplo <- dat |> 
    dplyr::rename(
      yvl = !!colnm
    )  

  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = year, y = yvl, color = scenario, group = scenario)) +
    ggplot2::geom_line(linewidth = linewidth) +
    ggplot2::labs(
      x = NULL,
      y = ylab,
      color = NULL,
      subtitle = subtitle
    ) +
    ggplot2::scale_color_manual(values = colval) +
    ggplot2::scale_y_continuous(breaks = seq(yrng[1], yrng[2], by = ybrk), limits = yrng) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = xbrk), limits = xrng) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(),
      legend.position = 'bottom'
    )
  
  if(caption){
    cp <- paste0('Source: https://sealevel.nasa.gov/task-force-scenario-tool?psmsl_id=', unique(dat$id))
    p <- p + 
      ggplot2::labs(caption = cp) 
  }
  
  if(title){
    p <- p + 
      ggplot2::labs(title = 'Relative Sea Level Change Predictions')
  }
  
  return(p)

}