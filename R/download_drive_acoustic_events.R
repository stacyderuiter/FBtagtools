#' Download spreadsheets with information about acoustic events from the FB Google Drive
#'
#' Default is to download data related to all Ziphius SMRT tag datasets (or you can specify the tag(s) for which you want to download data). Data are downloaded from the FB project shared Google Drive.
#'
#' @param tag_id Character string or vector with tag IDs to download (without "-cal.nc" and without additional file path information). Default: all SMRT ziphius tags.
#' @param ae_path Directory (quoted string) where spreadsheet files should be stored locally. Can be one string, or a list the same length as tag_ids. Default: current working directory. Note: use "/" and not "\" to avoid headaches. Final "/" not necessary.
#' @param email Email address (for FB Google Drive authentication). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param overwrite Logical; whether to overwrite existing files. Default is TRUE.
#' @param type The types of acoustic events files to find and download. Options are one or more of: 'AllClicks', 'Buzzes', 'PostProcessedEvents'. Default is all three.
#' @return Returns a "dribble" with information about the files and their location on google drive. (The function also downloads the requested files, of course.)
#' @export
#' @examples
#' download_drive_nc(email = "sld33@calvin.edu") # (this only works if you know sld33's password...)
download_drive_acoustic_events <- function(tag_id = zc_smrt_tag_list,
                                  ae_path = getwd(),
                              email,
                              overwrite = TRUE,
                              type = c('AllClicks', 'Buzzes', 'PostProcessedEvents')){
  if (missing(email)){
    stop('Email is required for Google Drive authentication and data download.')
  }

  googledrive::drive_auth(email = email)

  FB_dribble <- googledrive::shared_drive_find('FreakinBeakinTagData')

  ae_file_dribble <- list()

  for (i in c(1:length(type))){
    search_pattern <- dplyr::case_when(grepl(x = tolower(type[[i]]), pattern = 'clicks') ~ '*AllClickData*',
                                       grepl(x = tolower(type[[i]]), pattern = 'buzz') ~ '*PG_BUZZ_Events*',
                                       grepl(x = tolower(type[[i]]), pattern = 'processed') ~ '*PG_Post_Processed_Events*')
    ae_file_dribble[[i]] <- googledrive::drive_find(pattern = search_pattern,
                                                    shared_drive = FB_dribble)
    ae_file_dribble[[i]] <- ae_file_dribble[[i]] %>%
      googledrive::drive_reveal(what = 'path') %>%
      dplyr::mutate(type = type[[i]]) %>%
      tidyr::separate(col = name,
               into = c('tag'),
               sep = '_',
               remove = FALSE, extra = 'drop')

  }# end of loop over file type

  ae_file_dribble <- dplyr::bind_rows(ae_file_dribble) %>%
    # lose any files with "old" or "archive" in the name or that are csv
    dplyr::filter(stringr::str_detect(tolower(path), 'old', negate = TRUE)) %>%
    dplyr::filter(stringr::str_detect(tolower(path), 'archive', negate = TRUE)) %>%
    dplyr::filter(stringr::str_detect(name, 'csv', negate = TRUE)) %>%
    # keep only files with corrected times ('corr_times' in the name) that are in "AcousticAudits"
    dplyr::filter(stringr::str_detect(name, pattern = 'corr_times')) %>%
    dplyr::filter(stringr::str_detect(path, 'AcousticAudit')) %>%
    dplyr::arrange(tag, name)

  for (f in c(1:nrow(ae_file_dribble))){
    googledrive::drive_download(file = ae_file_dribble[f,],
                   path = file.path(ae_path,
                                 ae_file_dribble %>%
                                   dplyr::pull(name) %>%
                                   dplyr::nth(f)),
                   overwrite = overwrite)

  }


  return(ae_file_dribble)


  }
