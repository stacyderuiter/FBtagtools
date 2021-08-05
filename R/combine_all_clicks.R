#' Dive-level acoustic and tag data, for one or more tags
#'
#' Summarize SMRT (and/or Lander2) tag data on all clicks; add data on dive depth and sound exposure.
#'
#' @param tag_id Character string or vector with tag IDs (without "-cal.nc"). Default: all SMRT/Lander2 ziphius tags.
#' @param nc_path Directory (quoted string) where .nc files are stored. Can be one string, or a list the same length as tag_ids. Note: to download latest versions from Google drive, try function: \code{\link[FBtagtools]{download_drive_nc}}. Default: current working directory. Note: use "/" and not "\" to avoid headaches.
#' @param ae_path Directory (quoted string) where text files with info about acoustic events are stored. If needed, you can use \code{\link[FBtagtools]{download_drive_acoustic_events}} to get these. Default is the current working directory.
#' @param rl_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param ping_log_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param email Email address (required for FB Google Drive authentication; optional if `rl_file` is provided). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param save_csv Logical; whether or not to save a csv file with the results. Default: TRUE (and will overwrite any existing file)
#' @param click_csv_name File name (with path, if desired) in which to save results in csv format. Default is combined_zc_clicks.csv.
#' @param buzz_csv_name File name for buzz file. Default combined_zc_buzzes.csv
#' @return A named list of two tibbles, one for clicks and one for buzzes
#' @importFrom magrittr "%>%"
#' @export
combine_all_clicks <- function(tag_id = zc_smrt_tag_list,
                                  nc_path = getwd(),
                                  ae_path = getwd(),
                                  rl_file,
                                  ping_log_file,
                                  email,
                                  save_csv = TRUE,
                                  click_csv_name = 'combined_zc_clicks.csv',
                                  buzz_csv_name = 'combined_zc_buzzes.csv'){
  if ('data.frame' %in% class(tag_id)){
    tag_id <- tag_id[,'tag_id']
  }

  # paste together file path(s) and tag file name(s)
  tags <- file.path(nc_path, tag_id)


  # list of all ae files
  ae_files <- dir(path = ae_path)

  if (!missing(rl_file) & !missing(ping_log_file)){
    # if RL data files already downloaded
    all_rls <- extract_rls(rl_file = rl_file,
                           ping_log_file = ping_log_file,
                           signal = c('MFAS', 'Echosounder', 'Explosive'),
                           save_output = FALSE)
  }else{
    if (missing(email)){
      stop('email is needed to download RL data from Drive')
    }
    all_rls <- extract_rls(signal = c('MFAS', 'Echosounder', 'Explosive'),
                           save_output = FALSE)
  }

  data_out <- list()

  # loop over tags
  for (t in c(1:length(tags))){
    cat(paste('tag: ', t, '\n'))
    if (exists('this_allclicks')){
      rm(this_allclicks)
    }
    if (exists('this_data')){
      rm(this_data)
    }
    if (exists('this_buzz')){
      rm(this_buzz)
    }

    # garbage collection/free memory
    gc()

    # check if the tags filename contains ".nc" and add it if not
    if (stringr::str_detect(tags[t],
                            pattern = '.nc',
                            negate = TRUE)){
      if (stringr::str_detect(tags[t],
                              pattern = stringr::fixed('.'))){
        warning('tag_id inputs to dive_acoustic_summary must be .nc files; no other file types can be used.')
      }
      tags[t] <- paste0(tags[t], '-cal.nc')
    }

    # load in the data for this tag
    this_data <- tagtools::load_nc(tags[t])

    # RLS for this whale
    these_rls <- all_rls %>%
      dplyr::filter(TagID == tag_id[t])

    # read in data on acoustic events from files, for this tag t
    ci <- stringr::str_detect(ae_files, pattern = 'AllClickData') &
      stringr::str_detect(ae_files, pattern = tag_id[t])

    bi <- stringr::str_detect(ae_files, pattern = 'BUZZ') &
      stringr::str_detect(ae_files, pattern = tag_id[t])

    this_buzz <- data.frame() #placeholder

    if (sum(bi) == 1){
      this_buzz <- readxl::read_xlsx(file.path(ae_path, ae_files[bi]))
    }

    if (sum(bi) > 1){
      this_buzz <- list()
      idx <- which(bi)
      for (i in c(1:sum(bi))){
        this_buzz[[i]] <- readxl::read_xlsx(file.path(ae_path, ae_files[idx[i]]))
      }
      this_buzz <- dplyr::bind_rows(this_buzz) %>%
        dplyr::distinct()
    }

    #FOR BUZZES

    if (nrow(this_buzz) > 0){
      this_buzz <- this_buzz %>%
        # per SC keep ones that are labelled "BW Buzz" but not "Poss" or "Possible" and NOT probably
        dplyr::filter(stringr::str_detect(note, pattern = 'BW Buzz')) %>%
        dplyr::filter(stringr::str_detect(tolower(note),
                                          pattern = 'poss',
                                          negate = TRUE)) %>%
        dplyr::filter(stringr::str_detect(tolower(note),
                                          pattern = 'probabl',
                                          negate = TRUE)) %>%
        # make sure in chronological order
        arrange(sec_since_tagon)
      # add depths
      this_buzz <- add_event_depths(this_buzz,
                                    start_x = sec_since_tagon,
                                    z = this_data$depth)
      # add info on exposure
      this_buzz <- check_exposure(this_buzz,
                                  start_x = sec_since_tagon,
                                  rl_data = these_rls)
    }

    # FOR CLICKS
    this_allclicks <- data.frame() #placeholder

    if (sum(ci) == 1){
      this_allclicks <- readxl::read_xlsx(file.path(ae_path, ae_files[ci]))
    }

    if (sum(ci) > 1){
      this_allclicks <- list()
      idx <- which(ci)
      for (i in c(1:sum(ci))){
        this_allclicks[[i]] <- readxl::read_xlsx(file.path(ae_path, ae_files[idx[i]]))
      }
      this_allclicks <- dplyr::bind_rows(this_allclicks) %>%
        dplyr::distinct()
    }

    if (nrow(this_allclicks) > 0){
      # remove surfacings
      this_allclicks <- this_allclicks %>%
        dplyr::filter(stringr::str_detect(click_event_label, 'Surf', negate = TRUE))
      # sort in time order
      this_allclicks <- arrange(this_allclicks, sec_since_tagon)
      # add click ICIs
      this_allclicks <- this_allclicks %>%
        dplyr::mutate(ICI_sec = c(NaN, diff(dplyr::pull(this_allclicks, sec_since_tagon))))

      # add click depths
      this_allclicks <- add_event_depths(this_allclicks,
                                         start_x = sec_since_tagon,
                                         z = this_data$depth)
      # add exposure events
      this_allclicks <- check_exposure(this_allclicks,
                                       start_x = sec_since_tagon,
                                       rl_data = these_rls)

      # add tag ID
      this_allclicks <- this_allclicks %>%
        mutate(tag_id = tag_id[t])

    }

    # tibble-concatenating
    buzz_out[[t]] <- this_buzz
    click_out[[t]] <- this_allclicks
    buzz_out_all <- dplyr::bind_rows(buzz_out)
    click_out_all <- dplyr::bind_rows(click_out)
    if (save_csv){
      bout <- buzz_out_all %>%
        dplyr::mutate(dplyr::across(tidyselect::where(tidyselect::ends_with('UTC')), as.character))
      readr::write_csv(bout, file = buzz_csv_name)
      cout <- click_out_all %>%
        dplyr::mutate(dplyr::across(tidyselect::where(tidyselect::ends_with('UTC')), as.character))
      readr::write_csv(cout, file = click_csv_name)
    }

  } # end of loop over TAGS


  return(list(click_out_all, buzz_out_all))
  }

