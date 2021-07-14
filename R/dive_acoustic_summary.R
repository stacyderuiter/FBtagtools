#' Dive-level acoustic and tag data, for one or more tags
#'
#' Summarize SMRT (and/or Lander2) tag data from .nc files for each foraging dive cycle. Note: Currently for only SMRT tags; function and help will be updated to allow inclusion of Lander2 data as well when possible.
#'
#' @param tag_id Character string or vector with tag IDs (without "-cal.nc"). Default: all SMRT ziphius tags.
#' @param nc_path Directory (quoted string) where .nc files are stored. Can be one string, or a list the same length as tag_ids. Note: to download latest versions from Google drive, try function: \code{\link[FBtagtools]{download_drive_nc}}. Default: current working directory. Note: use "/" and not "\" to avoid headaches.
#' @param ae_path Directory (quoted string) where text files with info about acoustic events are stored. If needed, you can use \code{\link[FBtagtools]{download_drive_acoustic_events}} to get these. Default is the current working directory.
#' @param bathy_path A directory path to the folder containing all bathymetry data. Use \code{\link[FBtagtools]{download_bathy}} if you don't have this data already. If not provided, the bathymetry data will not be included in the output dataset.
#' @param rl_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param ping_log_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param email Email address (required for FB Google Drive authentication; optional if `rl_file` is provided). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param save_csv Logical; whether or not to save a csv file with the results. Default: TRUE (and will overwrite any existing file)
#' @param csv_name File name (with path, if desired) in which to save results in csv format. Default is dive_acoustic_summary.csv.
#' @return A data.frame() with one row per dive, per whale
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
dive_acoustic_summary <- function(tag_id = zc_smrt_tag_list,
                                  nc_path = getwd(),
                                  ae_path = getwd(),
                                  bathy_path,
                                  rl_file,
                                  ping_log_file,
                                  email,
                                  save_csv = TRUE,
                                  csv_name = 'dive_acoustic_summary.csv'){
  if ('data.frame' %in% class(tag_id)){
    tag_id <- tag_id[,'tag_id']
  }

  # paste together file path(s) and tag file name(s)
  tags <- file.path(nc_path, tag_id)


  # list of all ae files
  ae_files <- dir(path = ae_path)

  if (!missing(rl_file) & !missing(ping_log_file)){
    # if RL data files already downloaded
    all_mfa_rls <- extract_rls(rl_file = rl_file,
                               ping_log_file = ping_log_file,
                               save_output = FALSE)
  }else{
    if (missing(email)){
      stop('email is needed to download RL data from Drive')
    }
    all_mfa_rls <- extract_rls(save_output = FALSE)
  }

  data_out <- list()

  # loop over tags
  for (t in c(1:length(tags))){
    if (exists('these_dives')){
      rm(these_dives)
    }
    if (exists('this_allclicks')){
      rm(this_allclicks)
    }
    if (exists('this_data')){
      rm(this_data)
    }
    if (exists('this_buzz')){
      rm(this_buzz)
    }
    if (exists('these_dives_bz')){
      rm(this_buzz)
    }
    if (exists('this_bz')){
      rm(this_bz)
    }
    if (exists('this_focal_clicks')){
      rm(this_focal_clicks)
    }
    if (exists('this_nf_clicks')){
      rm(this_nf_clicks)
    }
    if (exists('this_events')){
      rm(this_events)
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
    these_rls <- all_mfa_rls %>%
      dplyr::filter(TagID == tag_id[t])

    # Detect dives
    # record:
    #  dive duration, start, end

    # one whale has some missing data in sensors.
    # this will mess up find_dives
    # make new depth vector with 0 rather than NA for this case
    if (sum(is.na(this_data$depth$data)) > 0){
      z <- tidyr::replace_na(this_data$depth$data,
                             replace = 0)
      these_dives <- find_limpet_dives(p = z,
                                          sampling_rate = this_data$depth$sampling_rate,
                                          mindepth = 50,
                                          deep_dur = 30,
                                          findall = 1)
    }else{
      these_dives <- find_limpet_dives(p = this_data$depth,
                                          mindepth = 50,
                                          deep_dur = 30,
                                          findall = 1)
    }

    these_dives <- these_dives %>%
      dplyr::mutate(tag_id = tag_id[t])

    # read in data on acoustic events from files, for this tag t
    bi <- stringr::str_detect(ae_files, pattern = 'BUZZ') &
      stringr::str_detect(ae_files, pattern = tag_id[t])

    ci <- stringr::str_detect(ae_files, pattern = 'AllClickData') &
      stringr::str_detect(ae_files, pattern = tag_id[t])

    ei <- stringr::str_detect(ae_files, pattern = 'Post_Processed_Events') &
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
    # per SC keep ones that are labelled "BW Buzz" but not "Poss" or "Possible" and NOT probably
    if (nrow(this_buzz) > 0){
    this_buzz <- this_buzz %>%
      dplyr::filter(stringr::str_detect(note, pattern = 'BW Buzz')) %>%
      dplyr::filter(stringr::str_detect(tolower(note),
                                        pattern = 'poss',
                                        negate = TRUE)) %>%
      dplyr::filter(stringr::str_detect(tolower(note),
                                        pattern = 'probabl',
                                        negate = TRUE)) %>%
      # make function to do this: add depth data to a df with sec_since_tagon
      dplyr::mutate(buzz_depth = this_data$depth$data[round(sec_since_tagon -
                                                       this_data$depth$start_offset) *
                                                 this_data$depth$sampling_rate] %>%
               as.numeric())
    }

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

    this_events <- data.frame() #placeholder

    if (sum(ei) == 1){
      this_events <- readxl::read_xlsx(file.path(ae_path, ae_files[ei]))
    }

    if (sum(ei) > 1){
      this_events <- list()
      idx <- which(ei)
      for (i in c(1:sum(ei))){
        this_events[[i]] <- readxl::read_xlsx(file.path(ae_path, ae_files[idx[i]]))
      }
      this_events <- dplyr::bind_rows(this_events) %>%
        dplyr::distinct()
    }

    if (nrow(this_allclicks) > 0){
      # focal clicks
      this_focal_clicks <- this_allclicks %>%
        dplyr::filter(stringr::str_detect(click_event_label, 'FD'))

      # nonfocal clicks
      this_nf_clicks = this_allclicks %>%
        dplyr::filter(stringr::str_detect(click_event_label, 'Other BW'))
    }else{
      this_focal_clicks <- this_nf_clicks <- this_allclicks
    }

    these_dives <- these_dives %>%
      dplyr::mutate(dive_dur_sec = end - start)

    # check if there is clicking in each dive; if so fill in clicking variables
    if (nrow(this_focal_clicks) > 0){
    # add focal clicks to the dive dataset (there will temp be one row per CLICK)
    these_dives <- interval_join(these_dives,
                                 this_focal_clicks %>%
                                   dplyr::select(click_UID,
                                                 sec_since_tagon),
                                 start_x = start, end_x = end,
                                 start_y = sec_since_tagon)


    these_dives <- these_dives %>%
      dplyr::group_by_all() %>%
      dplyr::ungroup(click_UID, sec_since_tagon) %>%
      dplyr::summarise(
        n_clicks = sum(!is.na(sec_since_tagon)),
        click_start_sec = ifelse(n_clicks > 0,
                                 min(sec_since_tagon, na.rm = TRUE),
                                 NA),
        click_end_sec = ifelse(n_clicks > 0,
                               max(sec_since_tagon, na.rm = TRUE),
                               NA),
        click_dur_sec = ifelse(n_clicks > 0,
                               diff(range(sec_since_tagon, na.rm = TRUE)),
                               NA),
        dive_to_click1_sec = ifelse(n_clicks > 0,
                                    min(sec_since_tagon) - start,
                                    NA),
        focal_click_UID = paste(click_UID, collapse = '|')
      ) %>%
      dplyr::ungroup()
    }

    # Now add nf clicks to dataset
    if (nrow(this_nf_clicks) > 0){
    these_dives <- interval_join(these_dives,
                                 this_nf_clicks %>%
                                   dplyr::select(click_UID,
                                                 sec_since_tagon),
                                 start_x = start, end_x = end,
                                 start_y = sec_since_tagon)

    these_dives <- these_dives %>%
      dplyr::group_by_all() %>%
      dplyr::ungroup(sec_since_tagon, click_UID) %>%
      dplyr::summarise(
        nonfocal_clicks = ifelse(any(!is.na(sec_since_tagon)),
                              'Present',
                              'Absent'),
        n_nonfocal_clicks = sum(!is.na(sec_since_tagon)),
        nonfocal_click_UID = paste(click_UID, collapse = '|')
      ) %>%
      dplyr::ungroup()
    }


    # Add info about clicking depths
    # need dive data as a dataframe
    this_depth <- add_sensor_times(this_data$depth)

    if (nrow(this_focal_clicks) > 0 &
        sum(these_dives$n_clicks, na.rm = TRUE) > 0){
    # join depth data and dive data so far
    these_dives_ckd <- interval_join(these_dives %>%
                                       dplyr::filter(n_clicks > 0),
                                 this_depth,
                                 start_x = click_start_sec,
                                 end_x = click_end_sec,
                                 start_y = sec_since_tagon)

    these_dives_ckd <- these_dives_ckd %>%
      dplyr::group_by_all() %>%
      dplyr::ungroup(dive_depth, sec_since_tagon) %>%
      dplyr::summarise(
        click_start_depth = dplyr::first(dive_depth),
        click_end_depth = dplyr::last(dive_depth),
        click_min_depth = min(dive_depth, na.rm = TRUE),
        click_max_depth = max(dive_depth, na.rm = TRUE)
        ) %>%
      dplyr::ungroup()

    these_dives <- dplyr::left_join(these_dives, these_dives_ckd,
                                    by = intersect(names(these_dives),
                                                   names(these_dives_ckd)))
    }

    # add info about buzzes
    if (nrow(this_buzz) > 0 & 'n_clicks' %in% names(these_dives)){
      if (sum(dplyr::pull(these_dives, n_clicks), na.rm = TRUE) > 0){
      # note: this subsetting may be unneccessary -
      # may just be that before I forgot to ungroup after the last summarize :()
    these_dives_bz <- interval_join(these_dives %>%
                                      dplyr::filter(n_clicks > 0) %>%
                                      dplyr::select(start, end),
                                 this_buzz %>% dplyr::select(sec_since_tagon,
                                                             buzz_duration_s,
                                                             buzz_depth),
                                 start_x = start,
                                 end_x = end,
                                 start_y = sec_since_tagon)

    these_dives_bz <- these_dives_bz %>%
      dplyr::group_by(start, end) %>%
      dplyr::summarise(
        n_buzzes = sum(!is.na(sec_since_tagon)),
        buzz_mean_depth = mean(buzz_depth, na.rm = TRUE),
        buzz_median_depth = stats::median(buzz_depth, na.rm = TRUE),
        buzz_sd_depth = stats::sd(buzz_depth, na.rm = TRUE),
        buzz_iqr_depth = stats::IQR(buzz_depth, na.rm = TRUE),
        buzz_min_depth = suppressWarnings(min(buzz_depth, na.rm = TRUE)),
        buzz_max_depth = suppressWarnings(max(buzz_depth, na.rm = TRUE)),
        buzz_mean_dur = mean(buzz_duration_s, na.rm = TRUE),
        buzz_median_dur = stats::median(buzz_duration_s, na.rm = TRUE),
        buzz_sd_dur = stats::sd(buzz_duration_s, na.rm = TRUE),
        buzz_iqr_dur = stats::IQR(buzz_duration_s, na.rm = TRUE),
        buzz_min_dur = suppressWarnings(min(buzz_duration_s, na.rm = TRUE)),
        buzz_max_dur = suppressWarnings(max(buzz_duration_s, na.rm = TRUE))
      ) %>%
      dplyr::mutate(dplyr::across(starts_with('buzz'),
                                  ~ifelse(is.infinite(.x), NA, .x))) %>%
      dplyr::ungroup()

    these_dives <- dplyr::left_join(these_dives,
                             these_dives_bz,
                             by = intersect(names(these_dives),
                                            names(these_dives_bz)))
    }}

    if (nrow(these_rls) > 0){

    # Add RLs to dataset
    these_dives <- interval_join(these_dives,
                                 these_rls %>% dplyr::select(sec_since_tagon,
                                                      BB_RMS),
                                 start_x = start,
                                 end_x = end,
                                 start_y = sec_since_tagon)

    these_dives <- these_dives %>%
      dplyr::group_by_all() %>%
      dplyr::ungroup(sec_since_tagon, BB_RMS) %>%
      dplyr::summarise(
        n_mfa_pings = sum(!is.na(sec_since_tagon)),
        mfa_bb_rms_min = min(BB_RMS, na.rm = TRUE),
        mfa_bb_rms_min = max(BB_RMS, na.rm = TRUE),
        mfa_bb_rms_median = median(BB_RMS, na.rm = TRUE),
        mfa_bb_rms_mean = suppressWarnings(10 * log10(mean(10 ^ (na.omit(BB_RMS) / 10)))),
        mfa_bb_rms_mean = ifelse(is.infinite(mfa_bb_rms_mean) |
                                   is.na(mfa_bb_rms_mean),
                                 NA,
                                 mfa_bb_rms_mean))   %>%
      dplyr::mutate(dplyr::across(starts_with('mfa'),
                                      ~ifelse(is.infinite(.x), NA, .x))) %>%
      dplyr::ungroup()
    }

    # Add GPS info
      # note -- make function to turn GPS stuff into data frame?
    these_locs <- data.frame(this_data$GPS_position$data)
    if (ncol(these_locs) < 3){
      these_locs <- data.frame(this_data$GPS_position$sampling_rate,
                               these_locs[1,1],
                               these_locs[2,1])
    }
    these_sats <- data.frame(this_data$GPS_satellites$data)
    if (ncol(these_sats) < 2){
      these_sats <- data.frame(this_data$GPS_satellites$sampling_rate,
                               these_sats)
    }

    these_resids <- data.frame(this_data$GPS_residual$data)
    if (ncol(these_resids) < 2){
      these_resids <- data.frame(this_data$GPS_residual$sampling_rate,
                               these_resids)
    }
    these_timeerr <- data.frame(this_data$GPS_time_error$data)
    if (ncol(these_timeerr) < 2){
      these_timeerr <- data.frame(this_data$GPS_time_error$sampling_rate,
                                 these_timeerr)
    }

    names(these_locs) <- c('sec_since_tagon', 'latitude', 'longitude')
    names(these_sats) <- c('sec_since_tagon', 'satellites')
    names(these_resids) <- c('sec_since_tagon', 'residual')
    names(these_timeerr) <- c('sec_since_tagon', 'time_error')

    these_locs <- dplyr::left_join(these_locs, these_sats, by = 'sec_since_tagon')
    these_locs <- dplyr::left_join(these_locs, these_resids, by = 'sec_since_tagon')
    these_locs <- dplyr::left_join(these_locs, these_timeerr, by = 'sec_since_tagon')

    these_locs <- these_locs %>%
      dplyr::filter(time_error < 3 &
                      time_error > -3 &
                      residual < 35 &
                      satellites >= 4) %>%
      dplyr::select(sec_since_tagon,
                    latitude,
                    longitude) %>%
      dplyr::filter(sec_since_tagon < max(dplyr::pull(these_dives, end), na.rm = TRUE))

    # add in all locs DURING the dive
    these_dives <- suppressWarnings(interval_join(these_dives,
                                 these_locs,
                                 start_x = start,
                                 end_x = end,
                                 start_y = sec_since_tagon))

    these_dives <- these_dives %>%
      dplyr::group_by_all() %>%
      dplyr::ungroup(sec_since_tagon, latitude, longitude) %>%
      dplyr::summarise(
        lat_initial = dplyr::first(latitude),
        lon_initial = dplyr::first(longitude),
        lat_final = dplyr::last(latitude),
        lon_final = dplyr::last(longitude)
      ) %>%
      dplyr::ungroup()

    # fill in tagon location as first position
    these_dives[1, 'lat_initial'] <- ifelse(is.na(these_dives[1, 'lat_initial']),
                                            this_data$info$dephist_deploy_location_lat,
                                            these_dives[1, 'lat_initial'])

    these_dives[1, 'lon_initial'] <- ifelse(is.na(these_dives[1, 'lon_initial']),
                                            this_data$info$dephist_deploy_location_lon,
                                            these_dives[1, 'lon_initial'])

    # locations will be NA if there was no position that dive so fill in with "last known" posn
    these_dives <- these_dives %>%
      tibble::as_tibble() %>% # needed for fill() and I don't know WHYYYYYY
      dplyr::mutate(lat_last_known = ifelse(is.na(lat_initial), NA, lat_initial),
                    lon_last_known = ifelse(is.na(lon_initial), NA, lon_initial)) %>%
      tidyr::fill(lat_last_known,
                  .direction = 'down') %>%
      tidyr::fill(lon_last_known,
                  .direction = 'down')



    # use add_sensor_times() and utc_to_local()
    these_dives <- these_dives %>%
      dplyr::mutate(start_UTC = lubridate::dmy_hms(this_data$info$dephist_device_datetime_start) +
                      lubridate::seconds(start),
                    start_local = lubridate::with_tz(start_UTC, tzone = 'America/Los_Angeles'))

    # fill in times-of-day
    these_dives <- these_dives %>%
      dplyr::mutate(solar_phase = solar_stage(start_UTC,
                                            lat = lat_last_known,
                                            lon = lon_last_known))


    # add distance traveled during foraging
    these_dives <- these_dives %>%
      dplyr::mutate(distance_traveled_km = oce::geodDist(latitude1 = dplyr::pull(these_dives, lat_initial),
                                                 latitude2 = c(dplyr::pull(these_dives, lat_initial) %>%
                                                   utils::tail(-1),
                                                   dplyr::pull(these_dives, lat_initial) %>%
                                                     dplyr::last()),
                                                 longitude1 = lon_initial,
                                                 longitude2 = c(dplyr::pull(these_dives, lon_initial) %>%
                                                                  utils::tail(-1),
                                                                dplyr::pull(these_dives, lon_initial) %>%
                                                                  dplyr::last())))
    # Add bathy info
    if (!missing(bathy_path)){
      # only if bathy data are available
      this_bathy <- add_bathy(x = these_dives,
                               lat_var = lat_initial,
                               lon_var = lon_initial,
                               z_radius = 1,
                               bathy_path = bathy_path
                               )

      these_dives <- dplyr::left_join(these_dives,
                               this_bathy,
                               by = intersect(names(these_dives),
                                              names(this_bathy))
      )

    }

    # tibble-concatenating
    data_out[[t]] <- these_dives
    data_out_all <- dplyr::bind_rows(data_out)
    if (save_csv){
      dout <- data_out_all %>%
        dplyr::rename(max_depth = max,
                      dive_start_sec = start,
                      dive_end_sec = end) %>%
        # so local times can survive csv read/write
        dplyr::mutate(start_local = as.character(start_local)) %>%
        dplyr::select(-tmax, -start_local)
      readr::write_csv(dout, file = csv_name)
    }

  } # end of loop over TAGS

  data_out_all <- data_out_all %>%
    dplyr::rename(max_depth = max,
                  dive_start_sec = start,
                  dive_end_sec = end) %>%
    # so local times can survive csv read/write
    dplyr::mutate(start_local = as.character(start_local)) %>%
    dplyr::select(-tmax, -start_local)

  return(data_out_all)
  }

