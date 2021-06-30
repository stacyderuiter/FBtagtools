#' Add time columns to time-series
#'
#' Add time (in seconds since tagon and perhaps as datetimes in UTC and/or local timezone) to a tibble or data-frame with regularly sampled data
#'
#' @param x input data: sensor data structure, vector, matrix, tibble, or data frame.  If `x` is not a tag sensor data structure, then `sampling_rate` is required input.
#' @param sampling_rate sampling rate of x in Hz (required if x is not a sensor data structure, and ignored if it is)
#' @param start_offset time (in seconds) between start of tag recording and the first recorded sample. Ignored (and pulled from metadata) if `x` is a sensor data structure. default: 0
#' @param add_utc_time Logical: should UTC time column be added? Default is FALSE.
#' @param tagon_time string giving the tag recording start time. Can be found in tag_dataset$info$dephist_device_datetime_start. Format: day month year hours minutes seconds. Required if `add_dttm` is TRUE.
#' @return A data.frame with the data (one row per sample) and column sec_since_tagon with times in seconds since start of tag recording. Optionally, also includes column utc_time.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
add_times <- function(x, sampling_rate, start_offset = 0,
                      add_utc_time = FALSE,
                      tagon_time){
  if (missing(tagon_time) & add_utc_time){
    stop('tagon_time must be provided to add datetimes')
  }

  if (utils::hasName(x, 'data')){ # if x is sensor data structure}
    sampling_rate <- x$sampling_rate
    start_offset <- x$start_offset
    var_name <- stringr::str_replace(x$description, ' ', '_')
    x <- x$data # x is now a vector or matrix
  }

  if (!('data.frame' %in% class(x))){
    # vector or matrix input
    x <- data.frame(x)
    if (ncol(x) == 1){
      # vector data
      names(x) <- var_name
    }else{
      # tri-axial data
      var_name <- paste(var_name, c('x', 'y', 'z'), sep = '_')
      names(x) <- var_name[c(1:ncol(x))]
    }
  }

  # now x is a data frame for sure.

  # add sec_since_tagon
  x <- x %>%
    dplyr::mutate(sec_since_tagon = start_offset +
                    c(1:nrow(x)) / sampling_rate)

  # if needed, add datetime column(s)
  if (add_utc_time){
    x <- x %>%
      dplyr::mutate(utc_time = lubridate::dmy_hms(tagon_time) +
                      lubridate::seconds(sec_since_tagon))
  }

  return(x)

  }
