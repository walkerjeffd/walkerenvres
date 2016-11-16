#' Get water day component of datetime(s)
#'
#' A water day is the number of days past the start of a water year beginning with 1.
#' For example, if a water year is defined as starting in October, then the water day for
#' 2007-09-30 is 365, 2007-10-01 is 1, 2007-10-02 is 2, etc.
#'
#' @param x vector of datetime objects
#' @param start_month integer specifying first month of water year (default = 10 for October)
#'
#' @return vector of water days as decimal numbers
#' @export
#'
#' @examples
#' x <- seq.Date(from = as.Date("2007-09-30"), to = as.Date("2007-10-02"), by = "day")
#' water_day(x)
water_day <- function(x, start_month = 10) {
  jd <- lubridate::yday(x)

  if (start_month == 1) {
    # same as julian day
    return(jd)
  } else {
    wy <- water_year(x, start_month = start_month)
    wy_start_date <- as.Date(paste(wy - 1, start_month, 1, sep = "-"))
    wd <- as.numeric(difftime(x, wy_start_date, units = "day")) + 1
    return(wd)
  }
}
