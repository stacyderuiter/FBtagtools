#' Add times of events (UTC or seconds_since)
#'
#' Add time (in seconds since tagon or datetimes in UTC and/or local timezone) to a tibble or data-frame with event times (conversion between datetime and seconds_since_tagon formats)
#'
#' @param x input data frame: vector, tibble, or data frame.  There should be one column that gives either seconds_since_tagon or UTC time already present.
#' @param start_x name of the variable in x that gives the times
#' @param tagon_time string giving the tag recording start time (or the `info` sensor data structure). Can be found in tag_dataset$info$dephist_device_datetime_start. Format: day month year hours minutes seconds.
#' @result The input `x`, but with additional column(s) sec_since_tagon with times in seconds since start of tag recording or utc_time (plus optionally local_time). Note, this function adds UTC times; if you want local times in addition, consider using  \code{\link[FBtagtools]{utc_to_local}} after this function.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
add_event_times <- function(x, start_x,
                            tagon_time){
  start_x <- rlang::enquo(start_x)

  if (utils::hasName(tagon_time, 'dephist_device_datetime_start')){
    # if info struct given for tagon_time
    tagon_time <- tagon_time$dephist_device_datetime_start
  }

  if (!missing(start_x)){
    t <- dplyr::pull(x, !!start_x)
  }


  if (!('data.frame' %in% class(x))){
    t <- x
    x <- as.data.frame(x)
  }

  if (is.character(t)){
    # ensure datetime strings
    t <- lubridate::dmy_hms(t)
  }

  if (is.numeric(t)){
    # we have sec_since_tagon and need UTC time
    x <- x %>%
      dplyr::mutate(utc_time = lubridate::dmy_hms(tagon_time) +
                    lubridate::seconds(t))
  }

  if (lubridate::is.Date(t)){
    # add sec_since_tagon
    x <- x %>%
      dplyr::mutate(sec_since_tagon = as.numeric(difftime(t,
                                                          lubridate::dmy_hms(tagon_time),
                                                          units = 'secs')))

  }

  return(x)

  }
