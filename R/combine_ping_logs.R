#' Combine ping logs (acoustic audit data on anthropogenic signals) for one or more tags
#'
#' Summarize SMRT (and/or Lander2) tag data from .nc files for each foraging dive cycle. There are data at two, nested time-scales: the dive cycle (a foraging dive and all the dives that follow it) and, within that, time-periods of set length in which other metrics are averaged. This function does coarse scale only. The data are intended for use in a state-switching model.
#'
#' @param tag_id Character string or vector with tag IDs (without "-cal.nc"). Default: all SMRT ziphius tags.
#' @param ping_log_path Directory (quoted string) where ping log files are stored. Can be one string, or a list the same length as tag_ids.
#' @param nc_path Directory (quoted string) where nc files are kept (for fetching tag recording start times). Default: same as ping log path
#' @return A data.frame() with information about timing of anthropogenic sounds on all tagouts in tag_id
#' @export

combine_ping_logs <- function(tag_id = zc_smrt_tag_list$tag_id,
                              ping_log_path,
                              nc_path = ping_log_path){
  orig_log <- readr::read_csv(file.path(ping_log_path[1], 'qPing_log_corr_times_master.csv'),
                              show_col_types = FALSE)
  ping_log <- list()
  for (t in c(1:length(tag_id))){
    if (length(ping_log_path) == 1){
      ppath <- ping_log_path[1]
    }else{
      ppath <- ping_log_path[t]
    }
    fname <- file.path(ppath,
                       paste0(tag_id[t], '_Individual_MFA_Pings.csv'))
    if (file.exists(fname)){
      ping_log[[t]] <- readr::read_csv(fname,
                                       show_col_types = FALSE) |>
        dplyr::mutate(depid = tag_id[t]) |>
        distinct()
    }else{
      # ping_log[[t]] <- NULL
    }
  }
  ping_log <- dplyr::bind_rows(ping_log)
  # add in tagon times as new col
  pltags <- data.frame(depid = unique(pull(ping_log, depid)),
                       tagon_time = NA)
  for (plti in c(1:nrow(pltags))){
    pltags$tagon_time <- get_tagon_time(dplyr::pull(pltags, depid), nc_path = nc_path)
  }
  pltags <- pltags |>
    mutate(tagon_time = lubridate::ymd_hms(tagon_time))
  ping_log <- dplyr::left_join(ping_log, pltags, by = 'depid')
  ping_log <- ping_log |>
    dplyr::rename(`Tag ID` = depid,
                  Type = Label)
  # compute sec since tagon
  ping_log <- ping_log |>
    dplyr::mutate(UTC = lubridate::mdy_hms(UTC, tz = 'UTC'),
      sec_since_tagon = difftime(UTC, tagon_time, units = "secs") |> as.numeric()) |>
    dplyr::select(`Tag ID`, tagon_time, sec_since_tagon, UTC, Duration, Type, Note)

  orig_log <- orig_log |>
    dplyr::select(`Tag ID`, tagon_time, sec_since_tagon, Seconds, Type) |>
    dplyr::rename(Duration = Seconds)
  ping_log <- dplyr::bind_rows(orig_log, ping_log)
  ping_log <- dplyr::filter(ping_log, `Tag ID` %in% tag_id)
  ping_log <- tidyr::drop_na(ping_log, Type)
  return(ping_log)
}
