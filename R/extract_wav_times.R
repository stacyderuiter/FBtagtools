#' extract timing info for SMRT wav files
#'
#' This function pulls data on start times (and other metadata) about acoustic wav files recorded by SMRT tags. It assumes that you already have downloaded the nc files for the deployments of interest to you; if not, see \code{\link[FBtagtools]{download_drive_nc}}
#'
#' @param tag_id Deployment IDs for the tag(s) for which you want to pull information. Defaults to the Ziphius deployments in `zc_smrt_tag_list`.
#' @param nc_path Quoted string with the path to the directory where nc files are stored. Defaults to current working directory.
#' @return Returns a data.frame with information about each wav file for each deployment of interest. If there are multiple rows per wav file, that is indicative of a (filled or un-filled) gap; the first row for a given deployment gives the file start time.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' extract_rls(email = "sld33@calvin.edu") # (this only works if you know sld33's password...)
extract_wav_times <- function(tag_id = zc_smrt_tag_list,
                        nc_path = getwd()){
  if ('data.frame' %in% class(tag_id)){
    tag_id <- tag_id[,'tag_id']
  }

  # paste together file path(s) and tag file name(s)
  tags <- file.path(nc_path, tag_id)

  wav_cues <- list()

  for (t in c(1:length(tags))){
    wav_cues[[t]] <- tagtools::load_nc(tags[t],
                                 which_vars = c('info', 'SA'))
    # grab tag ID and other metadata from info

    # grab file info from SA

  }

  wav_cues <- dplyr::bind_rows(wav_cues)

  return(wav_cues)

  }
