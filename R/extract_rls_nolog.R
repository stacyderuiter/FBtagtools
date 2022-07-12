#' extract RL data without ping long audit file
#'
#' This function processes the "raw" format of RL data, allowing users to obtain RLs of desired types and in selected frequency bands. If you need direct access to the (more complicated) raw data, see \code{\link[FBtagtools]{download_drive_rls}}. This "nolog" version of the function does not require a "ping log" file with acoustic audit information, instead relying on a signal_type column in the RL datafile itself.
#'
#' @param rl_file name (with path, if needed), or a vector of names, of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain all available RLs from the FB Google Drive.
#' @param email Email address (required for FB Google Drive authentication; optional if `rl_file` is provided). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param save_output Logical; whether or not to save results in a .csv file. Default is TRUE.
#' @param path Quoted string with the path to the directory where you want to save the output file. Defaults to the current working directory. Final "/" not needed. Use "/" rather than "\" to avoid possible headaches.
#' @param out_file Name (quoted string) for output .csv file with results. Optional; default file name is constructed based on signal type and frequency band requested.
#' @param signal Quoted string (or vector of them) indicating which signal types to return RLs for. Options are one or more of: 'MFAS', 'Echosounder', 'Explosive'. Default: 'MFAS'. Note: CAS is currently marked as MFAS type. You can isolate CAS pings and events by getting MFAS RLs and then keeping only pings with duration 20 seconds or longer.
#' @param overwrite Whether or not to overwrite an existing output file. Logical. Default: TRUE.
#' @return Returns a data.frame with RLs (one row per ping). If `save_output` is true, also saves a csv file in directory `path` with filename `out_file.csv` with the results.
#' @details For MFAS: Events are marked type "MFA" or "mfas". This also includes CAS exposures (which, if we have separated them out before, it is by choosing MFA pings of duration more than 20 seconds). Measurements were made in ANSI-standard 1/3 octave bands centered from 1-40kHz. For MFA events, bands with center frequencies less than 9kHz are considered. For max RMS level, Units are: dB re 1 muPa.  Measured in 200 msec windows; reported level is the highest in any one window. (Following conventions of SOCAL BRS, 3S projects.) For Echosounder events, selected bands are between 10-14kHz. For SPLs in individual 1/3 octave bands, the RL is "missing" (NA) if the SNR in that band was less than 6dB (and/or if the frequency band in question is no relevant for the selected signal type). For Explosions, all 1/3 octave bands below 5 kHz are included.
#' @importFrom magrittr "%>%"
#' @export
extract_rls_nolog <- function(rl_file,
                        email,
                        save_output = TRUE,
                        path = getwd(),
                        out_file,
                        signal = 'MFAS',
                        overwrite = TRUE){
  if (missing(rl_file)){
    if (missing(email)){
      stop('Email is required for Google Drive authentication and data download.')
    }else{
      # download file from Drive if needed
      googledrive::drive_auth(email = email,
                              scopes = c('https://www.googleapis.com/auth/spreadsheets',
                                         'https://www.googleapis.com/auth/drive'))
      raw_rl_dribble <- download_drive_rls(path = path,
                         email = email,
                         overwrite = overwrite)
      rl_file <- file.path(path, c('RLs_3obank.csv',
                                   'RLs_3obank_2022.csv'))
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
  raw_rls <- list()
  for (rf in c(1:length(rl_file_loc))){
  raw_rls[[rf]] <- suppressWarnings(readr::read_csv(rl_file_loc[rf],
                             col_types = readr::cols(),
                             na = c('', 'NA', 'NaN')))
  # remove idiotic spaces in variable names
  names(raw_rls) <- stringr::str_remove_all(names(raw_rls), pattern = ' ')
  }
  raw_rls <- dplyr::bind_rows(raw_rls)

  rl_log <- dplyr::arrange(raw_rls,
                            depid,
                            st) |>
    dplyr::mutate(st_UTC = lubridate::dmy_hms(st_UTC))

  # to properly join need to convert between st cst and start_time UTC
  # will add in UTC time stamps direct from audit file in to 2022+ RL files to ease matching up?
  # but original RL file does not have it and simply assumes rows correspond to each other

  # processing for specific signal types
  if (sum(grepl(pattern = 'orca',
                x = signal,
                ignore.case = TRUE))){
    orca_cols <- which(
      suppressWarnings(
        readr::parse_number(names(rl_log))
      ) < 24000 &
        suppressWarnings(
          readr::parse_number(names(rl_log))
        ) >= 500 &
        suppressWarnings(
          stringr::str_detect(names(rl_log), 'SPL_rms')
          )
    )

    orca_bb <- rl_log |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "orca")) |>
      dplyr::select(tidyselect::all_of(orca_cols),
                    depid, signal_type, # EventID, # event IDs are only for original few tags
                    st, st_UTC,
                    # tagon_time
                    ) |>
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) |>
      dplyr::group_by(depid, signal_type,
                      # EventID, tagon_time,
                      st, st_UTC) |>
      dplyr::summarize(BB_RMS = sum_rls(value)) |>
      dplyr::ungroup()

    orca_spl <- rl_log |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "orca")) |>
      dplyr::select(tidyselect::all_of(orca_cols),
                    depid, signal_type,
                    st, st_UTC
                    )

    orca_pings <- dplyr::left_join(orca_bb, orca_spl,
                                  by = c("depid", "signal_type",
                                         "st", "st_UTC"))
  }else{
    orca_pings <- NULL
  }

  # see original extract_rls() function for these key signal types
  mfa_pings <- NULL
  echo_pings <- NULL
  explos_pings <- NULL


ping_log <- dplyr::bind_rows(orca_pings, mfa_pings, echo_pings, explos_pings) |>
  dplyr::rename(TagID = depid,
                sec_since_tagon = st) |>
  dplyr::arrange(TagID,
                 sec_since_tagon) |>
  dplyr::mutate(BB_RMS = ifelse(is.infinite(BB_RMS), NA, BB_RMS))

if (save_output){
  readr::write_csv(ping_log,
                   file = file.path(path, out_file))
}

  return(ping_log)


  }
