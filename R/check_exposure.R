#' Add time columns to time-series
#'
#' Add time (in seconds since tagon and perhaps as datetimes in UTC and/or local timezone) to a tibble or data-frame with regularly sampled data
#'
#' @param x input data: vector or data frame with event times in seconds since tagon.
#' @param start_x name (no quotes needed) of variable in `x` that gives event times. Not needed if `x` is a vector or has only one column.
#' @param rl_data dataset with RL data (output of \code{\link[FBtagtools]{extract_rls}})
#' @return A data.frame with the data and additional columns exposure_EventID (the ID number of the exposure event(s) during which this timestamp falls) and event_type (the type(s) of exposure events that were going on at this timestamp)
#' @importFrom magrittr "%>%"
#' @export
check_exposure <- function(x, start_x, rl_data){
  start_x <- rlang::enquo(start_x)
  # start/end and type of exposure events
  rl_events <- rl_data %>%
    dplyr::group_by(EventID, Type) %>%
    dplyr::summarise(event_type = dplyr::first(Type),
              event_start = min(sec_since_tagon, na.rm = TRUE),
              event_end = max(sec_since_tagon + duration, na.rm = TRUE),
              .groups = 'drop_last') %>%
    ungroup()
  # for each event time, check if in exposure periods and return the event ID and type(s)
  x$exposure_EventID <- 'None';
  x$exposure_type <- 'None';

  for (r in c(1:nrow(x))){
    thistime <- x %>% dplyr::pull(!!start_x) %>% dplyr::nth(r)
    ei <- which(thistime >= rl_events$event_start & thistime < rl_events$event_end)
    if (length(ei) > 0){
      x[r, 'exposure_EventID'] <- paste(unique(sort(dplyr::pull(dplyr::slice(rl_events,ei), EventID) )), collapse =',')
      x[r, 'exposure_type'] <- paste(unique(sort(dplyr::pull(dplyr::slice(rl_events,ei), event_type) )), collapse =',')
    }
  }
  return(x)
}
