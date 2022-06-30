#' Download raw RL data from the FB Google Drive
#'
#' This function downloads the "raw" format of RL data, for all types of anthropogenic sounds, in all available variations of RL type and frequency band. Most users will want to also use \code{\link[FBtagtools]{extract_rls}} to generate data frames or text files with data in a simpler format.
#'
#' @param rl_file name of .csv file(s) (on Google Drive) with ping log data. Defaults to: RLs_3obank.csv and RLs_3obank_2022.csv (most users should not need to change this.)
#' @param ping_log_file name of .csv file (on Google Drive) with ping log data. Defaults to: qPing_log_corr_times_master.csv plus individual files for whales processed through June 2022 (most users should not need to change this.)
#' @param email Email address (for FB Google Drive authentication). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param path Quoted string with the path to the directory where you want to save the downloaded file. Defaults to the current working directory. Final "/" not needed. Use "/" rather than "\" to avoid possible headaches.
#' @param overwrite Whether or not to overwrite an existing file. Logical. Default: TRUE.
#' @return Returns a "dribble" with information about the files and their location on google drive. (The function also downloads the requested files, of course.)
#' @importFrom magrittr "%>%"
#' @export
download_drive_rls <- function(rl_file = c('RLs_3obank',
                                           'RLs_3obank_2022.csv'),
                                 ping_log_file = c('qPing_log_corr_times_master',
                                                   'Zica-20220112-195994_Individual_MFA_Pings.csv',
                                                   'Zica-20211113-195993_Individual_MFA_Pings.csv',
                                                   'Zica-20211112-94819_Individual_MFA_Pings.csv'),
                                  path = getwd(),
                              email,
                              overwrite = TRUE){
  if (missing(email)){
    stop('Email is required for Google Drive authentication and data download.')
  }

  googledrive::drive_auth(email = email,
                          scopes = c('https://www.googleapis.com/auth/spreadsheets',
                                     'https://www.googleapis.com/auth/drive'))

  FB_dribble <- googledrive::shared_drive_find('FreakinBeakinTagData')

  rl_file_dribble <- list()
  for (p in c(1:length(rl_file))){
    rl_file_dribble[[p]] <- googledrive::drive_find(pattern = paste0('*',
                                                                     rl_file[p]),
                                                    type = 'csv',
                                                    shared_drive = FB_dribble) %>%
      googledrive::drive_reveal(what = 'path')
  }
  rl_file_dribble <- dplyr::bind_rows(rl_file_dribble) |>
    dplyr::distinct()

  if (nrow(rl_file_dribble) > 1){
    rl_file_dribble <- rl_file_dribble %>%
      dplyr::filter(stringr::str_detect(path, 'OLD', negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(path, 'Old', negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(path, 'old', negate = TRUE))
  }

  if (nrow(rl_file_dribble) > 1){
    warning('More than one file with name matching the rl_file exists on the Drive. Downloading all; see output dribble for full list.')
  }

  for (p in c(1:nrow(rl_file_dribble))){
    googledrive::drive_download(file = rl_file_dribble[p,],
                                path = file.path(path,
                                                 rl_file_dribble %>%
                                                   dplyr::pull(name) %>%
                                                   dplyr::nth(p)),
                                overwrite = overwrite)
  }

  ping_file_dribble <- list()
  for (p2 in c(1:length(ping_log_file))){
    ping_file_dribble[[p2]] <- googledrive::drive_find(pattern = paste0('*',
                                                                        ping_log_file[p2]),
                                                       type = 'csv',
                                                       shared_drive = FB_dribble) %>%
      googledrive::drive_reveal(what = 'path')
  }
  ping_file_dribble <- dplyr::bind_rows(ping_file_dribble) |>
    dplyr::distinct()

  if (nrow(ping_file_dribble) > 1){
    ping_file_dribble <- ping_file_dribble %>%
      dplyr::filter(stringr::str_detect(path, 'OLD', negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(path, 'Old', negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(path, 'old', negate = TRUE))
  }

  if (nrow(ping_file_dribble) > 1){
    warning('More than one file with name matching the ping_log_file exists on the Drive. Downloading all and concatenating; see output dribble for full list.')
  }

  for (p2 in c(1:nrow(ping_file_dribble))){
    googledrive::drive_download(file = ping_file_dribble[p2,],
                                path = file.path(path,
                                                 ping_file_dribble %>%
                                                   dplyr::pull(name) %>%
                                                   dplyr::nth(p2)),
                                overwrite = overwrite)
  }

  return(dplyr::bind_rows(rl_file_dribble, ping_file_dribble))


}
