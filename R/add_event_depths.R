#' Add time columns to time-series
#'
#' Add time (in seconds since tagon and perhaps as datetimes in UTC and/or local timezone) to a tibble or data-frame with regularly sampled data
#'
#' @param x input data: vector or data frame with event times as datetimes, or in seconds since tagon. Datetimes are assumed to be UTC.
#' @param start_x name (no quotes needed) of variable in `x` that gives event times. Not needed if `x` is a vector or has only one column.
#' @param z depth data: sensor data structure, vector, matrix, tibble, or data frame. If not a sensor structure, then `sampling_rate` is required.
#' @param sampling_rate sampling rate of depth in Hz (required if x is not a sensor data structure, and ignored if it is)
#' @param start_offset time (in seconds) between start of tag recording and the first recorded sample in `z`. Ignored (and pulled from metadata) if `x` is a sensor data structure. default: 0
#' @param tagon_time string giving the tag recording start time. Can be found in tag_dataset$info$dephist_device_datetime_start. Format: day month year hours minutes seconds. Required if times in `x` are datetimes.
#' @return A data.frame with the data (one row per event) and additional column depth (in meters) of the animal at the start time of the event. Note: if you want to grab and summarize depth data for longer intervals, consider \code{\link[FBtagtools]{interval_join}} or \code{\link[tagtools]{find_dives}} and \code{\link[tagtools]{dive_stats}}
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
add_event_depths <- function(x, start_x,
                             z, sampling_rate, start_offset = 0, tagon_time){

  start_x <- rlang::enquo(start_x)

  if (utils::hasName(z, 'data')){ # if x is sensor data structure}
    sampling_rate <- z$sampling_rate
    start_offset <- z$start_offset
    z <- z$data # x is now a vector or matrix
  }

  if (!('data.frame' %in% class(z))){
    # vector or matrix input
    z <- data.frame(z)
    names(z) <- z
  }

  if (!('data.frame' %in% class(x))){
    # vector or matrix
    x <- data.frame(x)
  }

  # now z and x are data frame for sure.

  if (!missing(start_x)){
    tvec <- dplry::pull(x, !!start_x)
  }else{
    tvec <- dplyr::pull(x, 1)
  }

  if (missing(tagon_time) & 'POSIXct' %in% class(tvec)){
    stop('if event times are datetimes, then tagont_time is required')
  }

  if ('POSIXct' %in% class(tvec)){
    # convert to sec since start of data
    tvec = as.numeric(difftime(tvec,
                               tagon_time,
                               units = "secs"))
  }

  tvec <- tvec - start_offset
  # event start times in samples since start of depth data
  tix <- round(tvec * sampling_rate)

  x <- x %>%
    dplyr::mutate(event_depth = z[tix],
                  event_depth = as.numeric(event_depth))

  return(x)

  }
