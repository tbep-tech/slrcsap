#' Plot sea level data
#'
#' @param dat Input data from `get_sealevel()`.
#' @param col character, color for the line. Default is `'deepskyblue'`.
#' @param units character, units for the y-axis. Default is `'ft'`. Options are `'ft'` and `'m'`.
#' @param caption logical, add caption with source. Default is `TRUE`.
#' @param xrng Date, x-axis range. Default is `NULL`, which uses the range of the data.
#' @param xbrk numeric, x-axis breaks. Default is `10`.
#' @param yrng numeric, y-axis range. Default is `c(-1, 1)`.
#' @param ybrk numeric, y-axis breaks. Default is `0.5`.
#'
#' @returns a ggplot object
#' @export
#'
#' @examples
#' dat <- get_sealevel()
#' plot_sealevel(dat)
plot_sealevel <- function(dat, col = 'deepskyblue', units = 'ft', caption = TRUE, xrng = NULL, xbrk = 10, yrng = c(-1, 1), ybrk = 0.5) {
  
  units <- match.arg(units, c('ft', 'm'))
  
  if(is.null(xrng)){
    xrng <- range(dat$date, na.rm = TRUE)
  }
  if(!is.null(xrng))
    stopifnot(inherits(xrng, 'Date'))
  
  ylab <- ifelse(units == 'ft', 'Feet (MSL)', 'Meters (MSL)')
  ybrks <- seq(yrng[1], yrng[2], by = ybrk) 
  
  colnm <- names(dat)[grepl(paste0('msl_', units), names(dat))]
  
  toplo <- dat |> 
    dplyr::rename(
      yvl = !!colnm
    )  
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = date, y = yvl)) +
    ggplot2::geom_line(color = col) +
    ggplot2::labs(
      x = NULL,
      y = ylab
    ) +
    ggplot2::scale_y_continuous(breaks = ybrks, limits = yrng) +
    ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = xbrk), limits = xrng) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line()
    )
  
  if(caption){
    cp <- paste0('Source: https://tidesandcurrents.noaa.gov/sltrends/data/', unique(dat$gauge), '_meantrend.txt')
    p <- p + 
      ggplot2::labs(caption = cp) 
  }
  
  return(p)

}