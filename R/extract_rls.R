#' extract RL data
#'
#' This function processes the "raw" format of RL data, allowing users to obtain RLs of desired types and in selected frequency bands. If you need direct access to the (more complicated) raw data, see \code{\link[FBtagtools]{download_drive_rls}}.
#'
#' @param rl_file name (with path, if needed), or a vector of names, of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain all available RLs from the FB Google Drive.
#' @param email Email address (required for FB Google Drive authentication; optional if `rl_file` is provided). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param save_output Logical; whether or not to save results in a .csv file. Default is TRUE.
#' @param path Quoted string with the path to the directory where you want to save the output file. Defaults to the current working directory. Final "/" not needed. Use "/" rather than "\" to avoid possible headaches.
#' @param out_file Name (quoted string) for output .csv file with results. Optional; default file name is constructed based on signal type and frequency band requested.
#' @param signal Quoted string (or vector of them) indicating which signal types to return RLs for. Options are one or more of: 'MFAS', 'Echosounder', 'Explosive'. Default: 'MFAS'. Note: CAS is currently marked as MFAS type. You can isolate CAS pings and events by getting MFAS RLs and then keeping only pings with duration 20 seconds or longer.
#' @param overwrite Whether or not to overwrite an existing output file. Logical. Default: TRUE.
#' @return Returns a data.frame with RLs (one row per ping). If `save_output` is true, also saves a csv file in directory `path` with filename `out_file.csv` with the results.
#' @details For MFAS: Events are marked type "MFA". This also includes CAS exposures (which, if we have separated them out before, it is by choosing MFA pings of duration more than 20 seconds). Measurements were made in ANSI-standard 1/3 octave bands centered from 1-40kHz. For MFA events, bands with center frequencies less than 9kHz are considered. For max RMS level, Units are: dB re 1 muPa.  Measured in 200 msec windows; reported level is the highest in any one window. (Following conventions of SOCAL BRS, 3S projects.) For Echosounder events, selected bands are between 10-14kHz. For SPLs in individual 1/3 octave bands, the RL is "missing" (NA) if the SNR in that band was less than 6dB (and/or if the frequency band in question is no relevant for the selected signal type). For Explosions, all 1/3 octave bands below 5 kHz are included. Note that the function has been updated in 2024 along with updates to the RL calculation tool chain. Ping times from the audits are now kept in the output files from RL calculations, so it is not necessary to have the audit ("ping log") files AND the RL results to do this RL processing.
#' @importFrom magrittr "%>%"
#' @export
extract_rls <- function(rl_file = c("/Users/sld33/Dropbox/FBdata/RLs/Zica-20191012-145101.csv",
                                    '/Users/sld33/Dropbox/FBdata/RLs/Zica-20191117-195993.csv',
                                    '/Users/sld33/Dropbox/FBdata/RLs/Zica-20211112-94819.csv',
                                    '/Users/sld33/Dropbox/FBdata/RLs/Zica-20211113-195993.csv',
                                    '/Users/sld33/Dropbox/FBdata/RLs/Zica-20220112-195994.csv',
                                    '/Users/sld33/Dropbox/FBdata/RLs/Zica-20191012-144029.csv',
                                    '/Users/sld33/Dropbox/FBdata/RLs/Zica-20191111-94810.csv'),
                        email,
                        save_output = TRUE,
                        path = getwd(),
                        out_file,
                        signal = 'MFAS',
                        overwrite = TRUE){
  if (!(file.exists(rl_file[1]))){
    if (missing(email)){
      stop('Email is required for Google Drive authentication and data download.')
    }else{
      stop('Please locate correct RL files for analysis -- not able to automate yet.')
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

  # ping_file_loc <- ifelse(stringr::str_detect(ping_log_file, pattern = 'csv'),
  #                       ping_log_file,
  #                       paste0(ping_log_file, '.csv'))

#
#   # read in ping log data
#   raw_pings <- list()
#   for (f in c(1:length(ping_file_loc))){
#     raw_pings[[f]] <- suppressWarnings(readr::read_csv(ping_file_loc[f],
#                                                        col_types = readr::cols()))
#
#       # add Tag ID if not present
#     if (!("Tag ID" %in% names(raw_pings[[f]]))){
#       filepath <- ping_file_loc[f]
#       filepath <- stringr::str_split(filepath, '/', simplify = TRUE)
#       n <- length(unlist(filepath))
#       filepath <- filepath[n]
#       this_tag_id <- stringr::str_remove_all(filepath, '_Individual_MFA_Pings.csv')
#       raw_pings[[f]] <- raw_pings[[f]] |>
#         dplyr::mutate(`Tag ID` = this_tag_id)
#     }
#
#     if ('Seconds' %in% names(raw_pings[[f]])){
#       raw_pings[[f]] <- dplyr::rename(raw_pings[[f]],
#                     duration = Seconds)
#     }else{
#       if ('Duration' %in% names(raw_pings[[f]])){
#         raw_pings[[f]] <- dplyr::rename(raw_pings[[f]],
#                         duration = Duration)
#       }
#     }
#
#
#     if ('Start-Ping' %in% names(raw_pings[[f]])){
#       raw_pings[[f]] <- dplyr::rename(raw_pings[[f]],
#                     start_time = `Start-Ping`)
#     }else{
#       if ('UTC' %in% names(raw_pings[[f]])){
#         raw_pings[[f]] <- dplyr::rename(raw_pings[[f]],
#                       start_time = UTC)
#       }
#     }
#
#     raw_pings[[f]] <- raw_pings[[f]] |>
#       dplyr::mutate(start_time = lubridate::mdy_hms(start_time))
#     if ('correction' %in% names(raw_pings[[f]])){
#       raw_pings[[f]] <- raw_pings[[f]] |>
#         dplyr::mutate(start_time = start_time +
#                         lubridate::seconds(correction))
#     }
#
#     if ('Label' %in% names(raw_pings[[f]])){
#       raw_pings[[f]] <- raw_pings[[f]] |>
#         dplyr::rename(Type = Label)
#     }
#
#     # need to keep only info in common btw old and new ping log files:
#     # Tag ID, duration, start_time, Type
#     raw_pings[[f]] <- dplyr::select(raw_pings[[f]],
#                              `Tag ID`,
#                              start_time,
#                              duration,
#                              Type)
#     }
#
#   raw_pings <- dplyr::bind_rows(raw_pings)

  raw_rls <- list()
  for (rf in c(1:length(rl_file_loc))){
    new_names <- c(tag_id = 'depid',
                   tag_id = 'TagID')
  raw_rls[[rf]] <- suppressWarnings(readr::read_csv(rl_file_loc[rf],
                             col_types = readr::cols(),
                             na = c('', 'NA', 'NaN'))) |>
    dplyr::mutate(st_UTC = as.character(st_UTC)) |>
    dplyr::rename(dplyr::any_of(new_names))
  # remove idiotic spaces in variable names
  names(raw_rls) <- stringr::str_remove_all(names(raw_rls), pattern = ' ')
  }
  raw_rls <- dplyr::bind_rows(raw_rls) |>
    arrange(TagID, st)

  if (!("notes" %in% names(raw_rls))){
    raw_rls <- raw_rls |>
      dplyr::mutate(notes = NA)
  }
    # dplyr::mutate(start_UTC = lubridate::round_date(start_UTC, unit = ".1second"))

  # to properly join need to convert between st cst and start_time UTC
  # will add in UTC time stamps direct from audit file in to 2022+ RL files to ease matching up?
  # but original RL file does not have it and simply assumes rows correspond to each other
  # orig_tags <- c("Zica-20180113-173188",
  #                "Zica-20180330-173188",
  #                "Zica-20180331-173187",
  #                "Zica-20190111-173186",
  #                "Zica-20190113-151361",
  #                "Zica-20191012-144029",
  #                "Zica-20191012-145101",
  #                "Zica-20191111-94810",
  #                "Zica-20191117-195993",
  #                "Zica-20200106-195994")
  # ping_logA <- dplyr::bind_cols(ping_log |> dplyr::filter(`Tag ID` %in% orig_tags),
  #                        raw_rls |> dplyr::filter(depid %in% orig_tags))
  # ping_logB <- dplyr::left_join(ping_log |> dplyr::filter(!(`Tag ID` %in% orig_tags)) |> dplyr::distinct(),
  #                              rl_log |> dplyr::filter(!(depid %in% orig_tags)) |> dplyr::distinct(),
  #                              by = c("Tag ID" = "depid", "start_time" = "st_UTC"))
  # ping_log <- dplyr::bind_rows(ping_logA, ping_logB) |>
  #   dplyr::rename(sec_since_tagon = st)

  # processing for specific signal types
  if (sum(grepl(pattern = 'mfa',
                x = signal,
                ignore.case = TRUE))){
    mfa_cols <- which(
      suppressWarnings(
        readr::parse_number(names(raw_rls))
      ) < 9000 &
        suppressWarnings(
          stringr::str_detect(names(raw_rls), 'SPL_rms')
          )
    )

    mfa_bb <- raw_rls |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "mfa")) |>
      dplyr::select(tidyselect::all_of(mfa_cols), notes,
                    tag_id, signal_type,
                    st_UTC, st, dur) |>
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) |>
      dplyr::group_by(tag_id, signal_type,
                      st_UTC, st, dur, notes) |>
      dplyr::summarize(BB_RMS = sum_rls(value)) |>
      dplyr::ungroup()

    mfa_spl <- raw_rls |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "mfa")) |>
      dplyr::select(tidyselect::all_of(mfa_cols), notes,
                    tag_id, signal_type,
                    st_UTC, st, dur
                    )

    mfa_pings <- dplyr::left_join(mfa_bb, mfa_spl,
                                  by = c("tag_id", "signal_type", "st_UTC", "st", "dur", "notes"))
  }else{
    mfa_pings <- NULL
  }


  if (sum(grepl(pattern = 'echosound',
                x = signal,
                ignore.case = TRUE))){

    echo_cols <- which(
      suppressWarnings(
        readr::parse_number(names(raw_rls))
        ) <= 14000 &
        suppressWarnings(
          readr::parse_number(names(raw_rls))
          ) >= 10000 &
        stringr::str_detect(names(raw_rls), 'SPL_rms')
      )

    echo_bb <- raw_rls |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "echo")) |>
      dplyr::select(tidyselect::all_of(echo_cols), notes,
                    tag_id, signal_type, st_UTC,
                    st, dur) |>
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) |>
      dplyr::group_by(tag_id, signal_type,
                      st_UTC, st, dur, notes) |>
      dplyr::summarize(BB_RMS = sum_rls(value)) |>
      dplyr::ungroup()

    echo_spl <- raw_rls |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "echo")) |>
      dplyr::select(tidyselect::all_of(echo_cols), notes,
                    tag_id, signal_type,
                    st_UTC, st, dur)

    echo_pings <- dplyr::left_join(echo_bb, echo_spl,
                                  by = c("tag_id", "signal_type",
                                         "st_UTC", "st", "dur", "notes"))

  }else{
    echo_pings <- NULL
  }

  if (sum(grepl(pattern = 'explos',
                x = signal,
                ignore.case = TRUE))){

    explos_cols <- which(
      suppressWarnings(
        readr::parse_number(names(raw_rls))
      ) <= 5000 &
        stringr::str_detect(names(raw_rls), 'SPL_rms')
    )

    explos_bb <- raw_rls |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "explos")) |>
      dplyr::select(tidyselect::all_of(explos_cols),notes,
                    tag_id, signal_type,
                    st_UTC, st, dur) |>
      tidyr::pivot_longer(cols = tidyselect::starts_with('SPL_')) |>
      dplyr::group_by(tag_id, signal_type,
                      st_UTC, st, dur, notes,) |>
      dplyr::summarize(BB_RMS = sum_rls(value)) |>
      dplyr::ungroup()

    explos_spl <- raw_rls |>
      dplyr::filter(stringr::str_detect(tolower(signal_type), pattern = "explos")) |>
      dplyr::select(tidyselect::all_of(explos_cols), notes,
                    tag_id, signal_type,
                    st_UTC, st, dur)

    explos_pings <- dplyr::left_join(explos_bb, explos_spl,
                                     by = c("tag_id", "signal_type",
                                            "st_UTC", "st", "dur", "notes"))

  }else{
    explos_pings <- NULL
  }

rl_output <- dplyr::bind_rows(mfa_pings, echo_pings, explos_pings) |>
  dplyr::arrange(tag_id,
                 st) |>
  dplyr::mutate(BB_RMS = ifelse(is.infinite(BB_RMS), NA, BB_RMS)) |>
  dplyr::rename(duration = dur,
                sec_since_tagon = st)

if (save_output){
  readr::write_csv(rl_output,
                   file = file.path(path, out_file))
}

  return(rl_output)


  }
