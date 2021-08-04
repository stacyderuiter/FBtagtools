#' Download .nc files with tag data from the FB Google Drive
#'
#' Default is to download all Ziphius SMRT tag datasets (or you can specify the tag(s) for which you want to download data). Data are downloaded from the FB project shared Google Drive.
#'
#' @param tag_id Character string or vector with tag IDs to download (without "-cal.nc" and without additional file path information). Default: all SMRT ziphius tags.
#' @param path Directory (quoted string) where .nc files should be stored. Can be one string, or a list the same length as tag_ids. Default: current working directory. Note: use "/" and not "\" to avoid headaches. Final "/" not necessary.
#' @param overwrite Logical; whether to overwrite existing files. Default is TRUE.
#' @param email Email address (for FB Google Drive authentication). You may also be asked to sign in or verify your Google identity as this function runs.
#' @return Returns a "dribble" with information about the files and their location on google drive. (The function also downloads the requested files, of course.)
#' @export
download_drive_nc <- function(tag_id = zc_smrt_tag_list,
                                  path = getwd(),
                              email,
                              overwrite = TRUE){
  if (missing(email)){
    stop('Email is required for Google Drive authentication and data download.')
  }

  googledrive::drive_auth(email = email)

  FB_dribble <- googledrive::shared_drive_find('FreakinBeakinTagData')

  nc_files <- list()


  # will find stuff that is in other places but has tagid-cal.nc in filename
  # but is a lot slower
  for (t in c(1:nrow(tag_id))){
    nc_files[[t]] <- googledrive::drive_find(pattern = paste0('*',
                                                 tag_id %>%
                                                   dplyr::pull(tag_id) %>%
                                                   dplyr::nth(t),
                                                 '-cal.nc'),
                           shared_drive = FB_dribble)
  }

  nc_files <- dplyr::bind_rows(nc_files) %>%
    googledrive::drive_reveal(what = 'path') %>%
    dplyr::filter(stringr::str_detect(path, 'OLD', negate = TRUE)) %>%
    dplyr::filter(stringr::str_detect(path, 'Old', negate = TRUE)) %>%
    dplyr::filter(stringr::str_detect(path, 'old', negate = TRUE)) %>%
    dplyr::filter(stringr::str_detect(path, 'OFFICIAL', negate = TRUE))

  for (f in c(1:nrow(nc_files))){
    googledrive::drive_download(file = nc_files[f,],
                   path = file.path(path,
                                 nc_files %>%
                                   dplyr::pull(name) %>%
                                   dplyr::nth(f)),
                   overwrite = overwrite)

  }


  return(nc_files)


  }
