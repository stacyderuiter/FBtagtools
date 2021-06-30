#' Download bathymetry data, from FB google drive
#'
#' @param email Email address (for FB Google Drive authentication). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param bathy_path directory in which to save the zip file. Defaults to the current working directory.
#' @param overwrite Whether or not to overwrite existing zip file. Default: TRUE
#' @return A zip file with bathymetry data will be downloaded. To use, you'll then need to unzip it.
#' @examples #examples not yet provided, sorry :(

download_bathy <- function(email, bathy_path = getwd(), overwrite = TRUE) {
  if (missing(email)){
    stop('Email is required for Google Drive authentication and data download.')
  }

  googledrive::drive_auth(email = email)

  FB_dribble <- googledrive::team_drive_find('FreakinBeakinTagData')

  bathy_files <- googledrive::drive_find(pattern = 'Resources-Seafloor.zip',
                          team_drive = FB_dribble) %>%
    googledrive::drive_reveal()

  for (f in c(1:nrow(bathy_files))){
    googledrive::drive_download(file = bathy_files[f,],
                                path = file.path(bathy_path,
                                                 bathy_files %>%
                                                   dplyr::pull(name) %>%
                                                   dplyr::nth(f)),
                                overwrite = overwrite)

  }

}
