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
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' extract_rls(email = "sld33@calvin.edu") # (this only works if you know sld33's password...)
extract_rls <- function(rl_file = NULL,
                        ping_log_file = NULL,
                        email,
                        save_output = TRUE,
                        path = getwd(),
                        out_file = NULL,
                        signal = 'MFAS',
                        overwrite = TRUE){
  if (is.null(rl_file) |
      is.null(ping_log_file) |
      stringr::str_length(rl_file) == 0){
    if (missing(email)){
      stop('Email is required for Google Drive authentication and data download.')
    }else{
      # download file from Drive if needed
      googledrive::drive_auth(email = email)
      raw_rl_dribble <- download_drive_rls(path = path,
                         email = email,
                         overwrite = overwrite)
      rl_file <- file.path(path, 'qPing_log_corr_times_master.csv')
      ping_log_file <- file.path(path, 'RLs_3obank.csv')
    }
  }

  if (missing(out_file) | is.null(out_file)){
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

  raw_pings <- readr::read_csv(ping_file_loc,
                               col_types = cols()) %>%
    dplyr::select(-snr, -RMS, -ZeroPeak, -PeakPeak, -SEL)

  raw_rls <- readr::read_csv(rl_file_loc,
                             col_types = cols(),
                             na = c('', 'NA', 'NaN'))
  # remove idiotic spaces in variable names
  names(raw_rls) <- stringr::str_remove_all(names(raw_rls), pattern = ' ')

  ping_log <- arrange(raw_pings,
                      `Tag ID`,
                      sec_since_tagon)

  ping_log <- bind_cols(ping_log, raw_rls)


  return(ping_log)


  }
