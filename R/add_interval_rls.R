#' Add RL information for time intervals
#'
#' Add information about broadband RMS RLs to a dataset where each row contains data on a time interval, with start/end times of the interval given in seconds since start of tag record
#'
#' @param x data frame or tibble with each row giving data about a time interval (for one tag)
#' @param ping_data data frame or tibble for one tag, with each row giving rl data from one sound (for example, from \code{\link[FBtagtools]{extract_rls}})
#' @param start_x name of variable in x (quotes not needed) with start times of intervals, in seconds since start of tag recording
#' @param end_x name of variable in x (quotes not needed) with end times of intervals in seconds since start of tag recording
#' @param start_ping name of variable in ping_data giving start times of pings in seconds since start of tag recording
#' @return x with additional columns giving average, min, max, etc. RLs in each time interval
#' @export
#'

add_interval_rls <- function(x, ping_data, start_x, end_x, start_ping){
  start_x = rlang::enquo(start_x)
  end_x = rlang::enquo(end_x)
  start_ping = rlang::enquo(start_ping)

  ping_data <- ping_data |>
    dplyr::rename_with(tolower)

  # note: may want to future edit so that does SEL too? Need to get all energy and then sum
  x <- interval_join(x,
                     ping_data |> dplyr::select(sec_since_tagon,
                                                 duration,
                                                 bb_rms,
                                                sel_db,
                                                signal_type) |>
                       dplyr::rename(ping_duration = duration),
                     start_x = !!start_x,
                     end_x = !!end_x,
                     start_y = !!start_ping)

  x <- x |>
    dplyr::group_by_all() |> # includes groups by signal signal_type
    dplyr::ungroup(sec_since_tagon, bb_rms, sel_db, ping_duration) |>
    dplyr::summarise(
      n_pings = sum(!is.na(sec_since_tagon)),
      ping_dur_mean_sec = mean(ping_duration, na.rm = TRUE),
      ping_dur_min_sec = min(ping_duration, na.rm = TRUE),
      ping_dur_max_sec = max(ping_duration, na.rm = TRUE),
      bb_rms_min = min(bb_rms, na.rm = TRUE),
      bb_rms_max = max(bb_rms, na.rm = TRUE),
      bb_rms_median = median(bb_rms, na.rm = TRUE),
      bb_rms_mean = suppressWarnings(10 * log10(mean(10 ^ (na.omit(bb_rms) / 10)))),
      bb_rms_mean = ifelse(is.infinite(bb_rms_mean) |
                             is.na(bb_rms_mean),
                           NA,
                           bb_rms_mean),
      csel = 10 * log10(
        sum(
          10^( ifelse(is.na(sel_db), 0, sel_db) /10 )
        )
      )
    )   |>
    dplyr::ungroup() |>
    dplyr::mutate(signal_type = tolower(signal_type),
                  signal_type = ifelse(signal_type == 'mfas', 'mfa', signal_type)) |>
    tidyr::pivot_wider(names_from = signal_type,
                       values_from = c(n_pings,
                                       ping_dur_mean_sec,
                                       ping_dur_min_sec,
                                       ping_dur_max_sec,
                                       bb_rms_min,
                                       bb_rms_max,
                                       bb_rms_median,
                                       bb_rms_mean,
                                       csel),
                       names_glue = "{signal_type}_{.value}" # put the mfa_ echo_ etc. FIRST not last
    ) |>
    # if there are no non-missing RLs then some of the results will be Inf instead of missing
    # but they should just be missing
    dplyr::select(!tidyselect::starts_with('NA_')) |>
    dplyr::mutate(dplyr::across(contains('mfa') | contains('echosounder') | contains('explos'),
                                ~ifelse(is.infinite(.x), NA, .x))) |>
    # say "there are 0 pings" instead of NA for ping counts
    dplyr::mutate(dplyr::across(contains('n_pings'),
                                ~ifelse(is.na(.x), 0, .x)))

  return(x)
}
