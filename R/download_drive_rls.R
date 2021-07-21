#' Download raw RL data from the FB Google Drive
#'
#' This function downloads the "raw" format of RL data, for all types of anthropogenic sounds, in all available variations of RL type and frequency band. Most users will want to also use \code{\link[FBtagtools]{extract_rls}} to generate data frames or text files with data in a simpler format.
#'
#' @param rl_file name of .csv file (on Google Drive) with ping log data. Defaults to: RLs_3obank.csv (most users should not need to change this, and at present there IS no other file.)
#' @param ping_log_file name of .csv file (on Google Drive) with ping log data. Defaults to: qPing_log_corr_times_master.csv (most users should not need to change this, and at present there IS no other file.)
#' @param email Email address (for FB Google Drive authentication). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param path Quoted string with the path to the directory where you want to save the downloaded file. Defaults to the current working directory. Final "/" not needed. Use "/" rather than "\" to avoid possible headaches.
#' @param overwrite Whether or not to overwrite an existing file. Logical. Default: TRUE.
#' @return Returns a "dribble" with information about the files and their location on google drive. (The function also downloads the requested files, of course.)
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' download_drive_nc(email = "sld33@calvin.edu") # (this only works if you know sld33's password...)
download_drive_rls <- function(rl_file = 'RLs_3obank',
                                 ping_log_file = 'qPing_log_corr_times_master',
                                  path = getwd(),
                              email,
                              overwrite = TRUE){
  if (missing(email)){
    stop('Email is required for Google Drive authentication and data download.')
  }

  googledrive::drive_auth(email = email)

  FB_dribble <- googledrive::shared_drive_find('FreakinBeakinTagData')

  # will find stuff that is in other places but has tagid-cal.nc in filename
  # but is a lot slower
  rl_file_dribble <- googledrive::drive_find(pattern = paste0('*',
                                          rl_file),
                                type = 'csv',
                         shared_drive = FB_dribble) %>%
    googledrive::drive_reveal(what = 'path')

  if (nrow(rl_file_dribble) > 1){
    rl_file_dribble <- rl_file_dribble %>%
      dplyr::filter(stringr::str_detect(path, 'OLD', negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(path, 'Old', negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(path, 'old', negate = TRUE))
  }

  if (nrow(rl_file_dribble) > 1){
    warning('More than one file with name matching the rl_file exists on the Drive. Downloading the first one; see output dribble for full list.')
  }

    googledrive::drive_download(file = rl_file_dribble[1,],
                                path = file.path(path,
                                                 rl_file_dribble %>%
                                                   dplyr::pull(name) %>%
                                                   dplyr::first()),
                                overwrite = overwrite)

    ping_file_dribble <- googledrive::drive_find(pattern = paste0('*',
                                                                ping_log_file),
                                               type = 'csv',
                                               shared_drive = FB_dribble) %>%
      googledrive::drive_reveal(what = 'path')

    if (nrow(ping_file_dribble) > 1){
      ping_file_dribble <- ping_file_dribble %>%
        dplyr::filter(stringr::str_detect(path, 'OLD', negate = TRUE)) %>%
        dplyr::filter(stringr::str_detect(path, 'Old', negate = TRUE)) %>%
        dplyr::filter(stringr::str_detect(path, 'old', negate = TRUE))
    }

    if (nrow(ping_file_dribble) > 1){
      warning('More than one file with name matching the ping_log_file exists on the Drive. Downloading the first one; see output dribble for full list.')
    }

    googledrive::drive_download(file = ping_file_dribble[1,],
                                path = file.path(path,
                                                 ping_file_dribble %>%
                                                   dplyr::pull(name) %>%
                                                   dplyr::first()),
                                overwrite = overwrite)

  return(bind_rows(rl_file_dribble, ping_file_dribble))


  }
