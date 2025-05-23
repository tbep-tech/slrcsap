#' Plot sea level scenario data
#'
#' @param dat Input data from `get_scenario()`.
#' @param cols character, color palette for the lines. Default is `c('deepskyblue', 'orange', 'red')`.
#' @param units character, units for the y-axis. Default is `'ft'`. Options are `'ft'` and `'m'`.
#' @param linewidth numeric, line width. Default is `1`.
#' @param caption logical, add caption with source. Default is `TRUE`.
#' @param xrng numeric, x-axis range. Default is `c(2020, 2100)`.
#' @param xbrk numeric, number of x-axis breaks. Default is `10`.
#' @param yrng numeric, y-axis range as two values. Default is `NULL`, which uses the range of the data.
#' @param ybrk numeric, number of y-axis breaks. Default is `7`.
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' dat <- get_scenario()
#' plot_scenario(dat)
plot_scenario <- function(dat, cols = c('deepskyblue', 'orange', 'red'), units = 'ft', linewidth = 1, 
                          caption = TRUE, xrng = c(2020, 2100), xbrk = 10, yrng = NULL, ybrk = 7){
  
  units <- match.arg(units, c('ft', 'm'))
  
  ylab <- ifelse(units == 'ft', 'RSLC in Feet', 'RSLC in Meters')
  
  # create color palette
  levs <- levels(dat$scenario)
  colval <- grDevices::colorRampPalette(cols)(length(levs))
  
  colnm <- names(dat)[grepl(paste0('slr_', units), names(dat))]
  
  toplo <- dat |> 
    dplyr::rename(
      yvl = !!colnm
    ) |> 
    dplyr::filter(
      year >= xrng[1] & year <= xrng[2]
    )

  if(is.null(yrng))
    yrng <- c(0, max(toplo$yvl, na.rm = TRUE))
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = year, y = yvl, color = scenario, group = scenario)) +
    ggplot2::geom_line(linewidth = linewidth) +
    ggplot2::labs(
      x = NULL,
      y = ylab,
      color = NULL
    ) +
    ggplot2::scale_color_manual(values = colval) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = ybrk), limits = yrng) +
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
  
  return(p)

}