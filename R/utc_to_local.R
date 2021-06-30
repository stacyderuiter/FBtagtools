#' Add local time column to data frame with UTC time
#'
#' Given an input data frame that already has UTC datetimes, add another datetime column giving the local time
#'
#' @param x input data must contain a column that gives the UTC times to be converted to local
#' @param utc_var name of variable (in `x`) that gives the UTC times
#' @param tz desired time zone name -- for example the default, 'America/Los_Angeles'
#' @return data.frame `x` with one added column called local_time
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
utc_to_local <- function(x, utc_var,
                         tz = 'America/Los_Angeles'){
  utc_var <- rlang::enquo(utc_var)

  x <- x %>%
    dplyr::mutate(local_time = lubridate::with_tz(!!utc_var,
                                                  tzone = tz))
  }
