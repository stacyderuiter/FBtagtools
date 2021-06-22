#' extract RL data
#'
#' This function processes the "raw" format of RL data, allowing users to obtain RLs of desired types and in selected frequency bands. If you need direct access to the (more complicated) raw data, see \code{\link[FBtagtools]{download_drive_rls}}.
#'
#' @param rl_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param ping_log_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param email Email address (required for FB Google Drive authentication; optional if `rl_file` is provided). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param save_output Logical; whether or not to save results in a .csv file. Default is TRUE.
#' @param path Quoted string with the path to the directory where you want to save the output file. Defaults to the current working directory. Final "/" not needed. Use "/" rather than "\" to avoid possible headaches.
#' @param out_file Name (quoted string) for output .csv file with results. Optional; default file name is constructed based on signal type and frequency band requested.
#' @param signal Quoted string (or vector of them) indicating which signal types to return RLs for. Options are one or more of: 'MFAS', 'Echosounder', 'Explosive'. Default: 'MFAS'. Note: CAS is currently marked as MFAS type. You can isolate CAS pings and events by getting MFAS RLs and then keeping only pings with duration 20 seconds or longer.
#' @param overwrite Whether or not to overwrite an existing output file. Logical. Default: TRUE.
#' @return Returns a data.frame with RLs (one row per ping). If `save_output` is true, also saves a csv file in directory `path` with filename `out_file.csv` with the results.
#' @details For MFAS: Events are marked type "MFA". This also includes CAS exposures (which, if we have separated them out before, it is by choosing MFA pings of duration more than 20 seconds). Measurements were made in ANSI-standard 1/3 octave bands centered from 1-40kHz. For MFA events, bands with center frequencies less than 9kHz are considered. For max RMS level, Units are: dB re 1 muPa.  Measured in 200 msec windows; reported level is the highest in any one window. (Following conventions of SOCAL BRS, 3S projects.) For Echosounder events, selected bands are between 10-14kHz. For SPLs in individual 1/3 octave bands, the RL is "missing" (NA) if the SNR in that band was less than 6dB (and/or if the frequency band in question is no relevant for the selected signal type). For Explosions, all 1/3 octave bands below 5 kHz are included.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' extract_rls(email = "sld33@calvin.edu") # (this only works if you know sld33's password...)
extract_rls <- function(rl_file,
                        ping_log_file,
                        email,
                        save_output = TRUE,
                        path = getwd(),
                        out_file,
                        signal = 'MFAS',
                        overwrite = TRUE){
  if (missing(rl_file) |
      missing(ping_log_file)){
    if (missing(email)){
      stop('Email is required for Google Drive authentication and data download.')
    }else{
      # download file from Drive if needed
      googledrive::drive_auth(email = email)
      raw_rl_dribble <- download_drive_rls(path = path,
                         email = email,
                         overwrite = overwrite)
      rl_file <- file.path(path, 'RLs_3obank.csv')
      ping_log_file <- file.path(path, 'qPing_log_corr_times_master.csv')
    }
  }

  if (missing(out_file)){
    out_file = paste0('FB_RLs_', stringr::str_c(signal, collapse = '_'),
                      '.csv')
  }

  # check for .csv file extensions

  out_file <- ifelse(stringr::str_detect(out_file, pattern = '.csv'),
         out_file,
         paste0(out_file, '.csv'))

  rl_file_loc <- ifelse(stringr::str_detect(rl_file, pattern = 'csv'),
                        rl_file,
                        paste0(rl_file, '.csv'))

  ping_file_loc <- ifelse(stringr::str_detect(ping_log_file, pattern = 'csv'),
                        ping_log_file,
                        paste0(ping_log_file, '.csv'))


  # read in raw data

  raw_pings <- suppressWarnings(readr::read_csv(ping_file_loc,
                               col_types = readr::cols())) %>%
    dplyr::rename(duration = Seconds)

  raw_rls <- suppressWarnings(readr::read_csv(rl_file_loc,
                             col_types = readr::cols(),
                             na = c('', 'NA', 'NaN')))
  # remove idiotic spaces in variable names
  names(raw_rls) <- stringr::str_remove_all(names(raw_rls), pattern = ' ')

  ping_log <- dplyr::arrange(raw_pings,
                      `Tag ID`,
                      sec_since_tagon)

  ping_log <- dplyr::bind_cols(ping_log, raw_rls)

  # processing for specific signal types

  if (sum(grepl(pattern = 'mfa',
                x = signal,
                ignore.case = TRUE))){
    mfa_cols <- which(
      suppressWarnings(
        readr::parse_number(names(ping_log))
      ) < 9000 &
        suppressWarnings(
          stringr::str_detect(names(ping_log), 'SPL_rms')
          )
    )

    mfa_bb <- ping_log %>%
      dplyr::filter(stringr::str_detect(tolower(Type), pattern = "mfa")) %>%
      dplyr::select(tidyselect::all_of(mfa_cols),
                    `Tag ID`, Type, EventID, sec_since_tagon, duration,
                    tagon_time) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) %>%
      dplyr::group_by(`Tag ID`, tagon_time, Type, EventID,
                      sec_since_tagon, duration) %>%
      dplyr::summarize(BB_RMS = sum_rls(value)) %>%
      dplyr::ungroup()

    mfa_spl <- ping_log %>%
      dplyr::filter(stringr::str_detect(tolower(Type), pattern = "mfa")) %>%
      dplyr::select(tidyselect::all_of(mfa_cols),
                    `Tag ID`, Type, EventID, sec_since_tagon, duration,
                    tagon_time)

    mfa_pings <- dplyr::left_join(mfa_bb, mfa_spl,
                                  by = c("Tag ID", "tagon_time", "Type", "EventID", "sec_since_tagon", "duration"))
  }else{
    mfa_pings <- NULL
  }


  if (sum(grepl(pattern = 'echosound',
                x = signal,
                ignore.case = TRUE))){

    echo_cols <- which(
      suppressWarnings(
        readr::parse_number(names(ping_log))
        ) <= 14000 &
        suppressWarnings(
          readr::parse_number(names(ping_log))
          ) >= 10000 &
        stringr::str_detect(names(ping_log), 'SPL_rms')
      )

    echo_bb <- ping_log %>%
      dplyr::filter(stringr::str_detect(tolower(Type), pattern = "echo")) %>%
      dplyr::select(tidyselect::all_of(echo_cols),
                    `Tag ID`, Type, EventID, sec_since_tagon, duration,
                    tagon_time) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) %>%
      dplyr::group_by(`Tag ID`, tagon_time, Type, EventID,
                      sec_since_tagon, duration) %>%
      dplyr::summarize(BB_RMS = sum_rls(value)) %>%
      dplyr::ungroup()

    echo_spl <- ping_log %>%
      dplyr::filter(stringr::str_detect(tolower(Type), pattern = "echo")) %>%
      dplyr::select(tidyselect::all_of(echo_cols),
                    `Tag ID`, Type, EventID, sec_since_tagon, duration,
                    tagon_time)

    echo_pings <- dplyr::left_join(echo_bb, echo_spl,
                                  by = c("Tag ID", "tagon_time", "Type", "EventID", "sec_since_tagon", "duration"))

  }else{
    echo_pings <- NULL
  }

  if (sum(grepl(pattern = 'explos',
                x = signal,
                ignore.case = TRUE))){

    explos_cols <- which(
      suppressWarnings(
        readr::parse_number(names(ping_log))
      ) <= 5000 &
        stringr::str_detect(names(ping_log), 'SPL_rms')
    )

    explos_bb <- ping_log %>%
      dplyr::filter(stringr::str_detect(tolower(Type), pattern = "explos")) %>%
      dplyr::select(tidyselect::all_of(explos_cols),
                    `Tag ID`, Type, EventID, sec_since_tagon, duration,
                    tagon_time) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) %>%
      dplyr::group_by(`Tag ID`, tagon_time, Type, EventID,
                      sec_since_tagon, duration) %>%
      dplyr::summarize(BB_RMS = sum_rls(value)) %>%
      dplyr::ungroup()

    explos_spl <- ping_log %>%
      dplyr::filter(stringr::str_detect(tolower(Type), pattern = "explos")) %>%
      dplyr::select(tidyselect::all_of(explos_cols),
                    `Tag ID`, Type, EventID, sec_since_tagon, duration,
                    tagon_time)

    explos_pings <- dplyr::left_join(explos_bb, explos_spl,
                                   by = c("Tag ID", "tagon_time", "Type", "EventID", "sec_since_tagon", "duration"))

  }else{
    explos_pings <- NULL
  }

ping_log <- dplyr::bind_rows(mfa_pings, echo_pings, explos_pings) %>%
  dplyr::arrange(`Tag ID`,
                 sec_since_tagon) %>%
  dplyr::mutate(BB_RMS = ifelse(is.infinite(BB_RMS), NA, BB_RMS)) %>%
  dplyr::rename(TagID = `Tag ID`)

if (save_output){
  readr::write_csv(ping_log,
                   file = file.path(path, out_file))
}

  return(ping_log)


  }
