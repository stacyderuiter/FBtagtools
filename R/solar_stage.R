#' Add time columns to time-series
#'
#' Add time (in seconds since tagon and perhaps as datetimes in UTC and/or local timezone) to a tibble or data-frame with regularly sampled data
#'
#' @param utc_time UTC time
#' @param lat latitude
#' @param long longitude
#' @param time_zone Time zone for local time; defaults to 'America/Los_Angeles'
#' @return solar stage (categorical): "Day", "Night", "Dusk" or "Dawn"
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
solar_stage <- function(time, lat, long, time_zone = "America/Los_Angeles") {
  elev <- oce::sunAngle(time, long, lat)$altitude

  stage <- dplyr::case_when(elev > 6 ~ 'Day',
                          elev < -12 ~ 'Night',
                          lubridate::hour(lubridate::with_tz(time, tzone = time_zone)) >= 12 ~ 'Dusk',
                          TRUE ~ 'Dawn')

  return(stage)
}
