#' Dive-cycle-level acoustic and tag data, for one or more tags
#'
#' Summarize SMRT (and/or Lander2) tag data from .nc files for each foraging dive cycle. There are data at two, nested time-scales: the dive cycle (a foraging dive and all the dives that follow it) and, within that, time-periods of set length in which other metrics are averaged. The data are intended for use in a state-switching model.
#'
#' @param tag_id Character string or vector with tag IDs (without "-cal.nc"). Default: all SMRT ziphius tags.
#' @param nc_path Directory (quoted string) where .nc files are stored. Can be one string, or a list the same length as tag_ids. Note: to download latest versions from Google drive, try function: \code{\link[FBtagtools]{download_drive_nc}}. Default: current working directory. Note: use "/" and not "\" to avoid headaches.
#' @param ae_path Directory (quoted string) where text files with info about acoustic events are stored. If needed, you can use \code{\link[FBtagtools]{download_drive_acoustic_events}} to get these. Default is the current working directory.
#' @param bathy_path A directory path to the folder containing all bathymetry data. Use \code{\link[FBtagtools]{download_bathy}} if you don't have this data already. If not provided, the bathymetry data will not be included in the output dataset.
#' @param ETOP01_bathy whether to try to fill-in bathymetry info for locations outside NEPAC dataset bounds using NOAA online ETOP01 database. Default is FALSE. This is slow and no animals have gone out-of-bounds yet as of July 2021.
#' @param rl_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param ping_log_file name (with path, if needed) of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain it from the FB Google Drive.
#' @param rl_model_dir directory where files with results of RL modeling (for non-acoustic tags) are stored locally
#' @param dive_class_file name (with path, if needed) of locally-stored .csv file with information on machine-learning classification of dives as foraging or not. This should match perfectly with presence/absence of clicking from acoustic recordings, but is mainly for tags/dives without acoutics.
#' @param acoustic_summary_file optional - if you have already run \code{\link[FBtagtools]{dive_acoustic_summary}} and have saved results as a csv, you can use that file to get results faster. Otherwise, this function will call \code{\link[FBtagtools]{dive_acoustic_summary}} and re-do the dive-level processing.
#' @param email Email address (required for FB Google Drive authentication; optional if `rl_file` is provided). You may also be asked to sign in or verify your Google identity as this function runs.
#' @param save_csv Logical; whether or not to save a csv file with the results. Default: TRUE (and will overwrite any existing file)
#' @param save_tracks Logical; whether or not to save csv file with geo-referenced pseudotrack (in meters northing/easting relative to initial location)
#' @param track_dir Directory to store tracks in (if save_tracks is TRUE). Defaults to working directory.
#' @param csv_name File name (with path, if desired) in which to save results in csv format. Default is dive_acoustic_summary.csv.
#' @return A data.frame() with one row per dive, per whale
#' @importFrom magrittr "%>%"
#' @export

dive_cycle_summary <- function(tag_id = zc_smrt_tag_list,
                               nc_path = getwd(),
                               ae_path = getwd(),
                               bathy_path,
                               ETOP01_bathy = FALSE,
                               ping_log_file,
                               rl_file,
                               rl_model_dir,
                               dive_class_file,
                               acoustic_summary_file,
                               email,
                               save_csv = TRUE,
                               save_tracks = FALSE,
                               track_dir,
                               csv_name = 'dive_cycle_summary.csv'){
  # notes:

  # - include fluke rate - in line with MJ method. this is @ fine scale.
  # - incorporate RL/signal type info from Navy modeling efforts at both time scales
  # - also incorporate actual measured RLs at both time scales
  # probably want to do coarse and fine scales separately (diff functions?) and then combine @ start of HMM analysis
  # create finescale file for each whale in matlab (for speed) and add RL data in R (for ease of joins)

  # coarse scale variables:
  # dive start, end, max depth, click duration, number of clicks,
  # number of buzzes, buzz duration total, mean, ...
  # number of shallow dives, number of breaths, surface time,
  # nf click info, path/step/turn/tortuosity (GPS and ptrack??)
  # season, lunar cycle, time of day, ?known exposure history

  # short scale variables:
  # median fluke rate, MSA: mean, 90th percentile EVD;
  # rms stroke amplitude, peak stroke amplitude, strokes per second
  # count of buzzes, focal and non?
  # dive wiggliness, median vertical velocity, median horiz velocity,
  # pitch, roll, head variance,
  #
  # Notes: get_br function (and others?) only in matlab, and some datasets are big, so maybe do the coarse part in matlab??
  #


  if (!missing(acoustic_summary_file) && !is.null(acoustic_summary_file)){
    # if possible, read in dive level data from previous run of dive_acoustic_summary()
    das <- readr::read_csv(acoustic_summary_file,
                           guess_max = 2700,
                           show_col_types = FALSE)
  }else{
    # if not provided, compute dive summary data
    das <- dive_acoustic_summary(tag_id = tag_id,
                                 nc_path = nc_path,
                                 ae_path = ae_path,
                                 bathy_path = bathy_path,
                                 ETOP01_bathy = ETOP01_bathy,
                                 rl_file = rl_file,
                                 ping_log_file = ping_log_file,
                                 email = email,
                                 save_csv = FALSE)
  }


  # read in data on foraging dives/not and add to das
  # note: a few fewer dives that in das :(
  dive_class <- readr::read_csv(dive_class_file,
                                guess_max = 2000,
                                show_col_types = FALSE) %>%
    dplyr::mutate(rounded_start = round(Start/10) * 10) %>%
    dplyr::select(TagID, rounded_start, preds_prob.bottom.tree.Yes, preds_class.bottom.tree) %>%
    dplyr::rename(tag_id = TagID,
                  prob_foraging = preds_prob.bottom.tree.Yes,
                  dive_class = preds_class.bottom.tree) %>%
    dplyr::mutate(dive_class = ifelse(dive_class == 'Yes', 'foraging', 'non-foraging'),
                  tag_id = ifelse(tag_id == "Zc-20180331-173187", "Zica-20180331-173187", tag_id))

  das <- das %>%
    dplyr::mutate(rounded_start = round(dive_start_sec/10) * 10)

  das <- dplyr::left_join(das,
                          dive_class,
                          by = c('tag_id', 'rounded_start'))

  # fill in "foraging"/non-foraging for missing IF acoustic data present
  # leave missing if no acoustic data and not classed by ML tree algorithm (about 9 dives)
  das <- das %>%
    dplyr::mutate(dive_class = ifelse(is.na(dive_class) & n_clicks > 0, 'foraging', dive_class)) %>%
    dplyr::mutate(dive_class = ifelse(is.na(dive_class) & n_clicks == 0, 'non-foraging', dive_class))

  # aggregate to dive CYCLES instead of just dives
  # give a dive-number to each foraging dive by whale and then fill down to create groups?
  FD <- das %>%
    dplyr::arrange(tag_id, dive_start_sec) %>%
    dplyr::group_by(tag_id) %>%
    dplyr::filter(dive_class == 'foraging') %>%
    dplyr::mutate(foraging_dive_id = as.numeric(c(1:dplyr::n()))) %>%
    dplyr::select(tag_id, dive_start_sec, foraging_dive_id)

  das <- dplyr::left_join(das, FD, by = c('tag_id', 'dive_start_sec')) %>%
    dplyr::group_by(tag_id) %>%
    tidyr::fill(foraging_dive_id, .direction = 'down') %>%
    dplyr::ungroup()

  # group by foraging dive cycle and compute stuff
  dcs <- das %>%
    # get rid of dives that are not part of a foraging dive cycle
    dplyr::filter(!is.na(foraging_dive_id)) %>%
    # coarse scale variables:
    # path/step/turn/tortuosity (GPS and ptrack??)

    dplyr::group_by(tag_id, foraging_dive_id) %>%
    dplyr::summarise(dive_cycle_start_sec = dplyr::first(dive_start_sec),
                     dive_cycle_end_sec = dplyr::last(next_start),
                     dive_cycle_dur_sec = dive_cycle_end_sec - dive_cycle_start_sec,
                     fd_end_sec = dplyr::first(dive_end_sec),
                     fd_dur_sec = dplyr::first(dive_dur_sec),
                     fd_start_UTC = dplyr::first(start_UTC),
                     fd_max_depth = dplyr::first(max_depth),
                     fd_breath_count_post_dive = dplyr::first(breath_count_post_dive),
                     n_shallow_dives = dplyr::n() - 1,
                     fd_surface_interval = dive_cycle_dur_sec - fd_dur_sec,
                     dive_cycle_surface_sec = sum(next_start - dive_end_sec, na.rm = TRUE),
                     dive_cycle_breath_count = sum(breath_count_post_dive, na.rm = TRUE),
                     fd_bathy = dplyr::first(bathy),
                     fd_bathy_slope = dplyr::first(bathy_slope),
                     fd_bathy_aspect = dplyr::first(bathy_aspect),
                     bathy_source = dplyr::first(bathy_source),
                     bathy_radius_km = dplyr::first(bathy_radius_km),
                     lat_initial = dplyr::first(lat_initial),
                     lon_initial = dplyr::first(lon_initial),
                     lat_final = dplyr::last(lat_final),
                     lon_final = dplyr::last(lon_final),
                     gps_fd_dist_km = dplyr::first(distance_traveled_km),
                     gps_dive_cycle_path_km = sum(distance_traveled_km, na.rm = TRUE),
                     horiz_speed_mean_km_h = gps_dive_cycle_path_km / ((dplyr::last(final_loc_time) - dplyr::first(initial_loc_time)) / 3600),
                     solar_phase_initial = dplyr::first(solar_phase),
                     solar_phase_final = dplyr::last(solar_phase),
                     lunar_phase = oce::moonAngle(t = dplyr::first(start_UTC),
                                                  longitude = dplyr::first(na.omit(lon_initial)),
                                                  latitude = dplyr::first(na.omit(lat_initial)))$illuminatedFraction,
                     quarter_of_year = lubridate::quarter(dplyr::first(start_UTC), fiscal_start = 12),
                     # click variables
                     n_clicks = sum(n_clicks, na.rm = TRUE),
                     across(starts_with("click_"), ~ dplyr::first(.x)),
                     dive_to_click1_sec = dplyr::first(dive_to_click1_sec),
                     across(starts_with("nonfocal_click_"), ~ mean(.x, na.rm = TRUE)),
                     n_nonfocal_clicks = sum(n_nonfocal_clicks, na.rm = TRUE),
                     # buzz variables
                     n_buzzes = sum(n_buzzes, na.rm = TRUE),
                     across(starts_with("buzz_"), ~ dplyr::first(.x))
                     # # RL variables
                     # across(contains('_n_pings'), ~ sum(.x, na.rm = TRUE)),
                     # across(contains('_ping_dur_max'), ~ max(.x, na.rm = TRUE)),
                     # across(contains('_ping_dur_min'), ~ min(.x, na.rm = TRUE)),
                     # across(contains('_bb_rms_min'), ~ min(.x, na.rm = TRUE)),
                     # across(contains('_bb_rms_max'), ~ max(.x, na.rm = TRUE)),
                     # NOTE: should add these after using dive cycle intervals.
    ) %>%
    dplyr::ungroup()

  step_turn_data <- moveHMM::prepData(dcs %>% dplyr::select(tag_id, dive_cycle_start_sec, lat_initial, lon_initial) %>%
                                        data.frame() %>%
                                        dplyr::mutate(ID = tag_id),
                                      type = "LL",
                                      coordNames = c("lon_initial", "lat_initial"),
                                      LLangle = TRUE) %>%
    dplyr::select(tag_id, dive_cycle_start_sec, step, angle) %>%
    data.frame() %>%
    dplyr::rename(gps_dive_cycle_net_km = step,
                  turn_angle_rad = angle)

  # join back in
  dcs <- dplyr::left_join(dcs, step_turn_data,
                          by = c('tag_id', 'dive_cycle_start_sec'))

  if ('data.frame' %in% class(tag_id)){
    tag_id <- tag_id[,'tag_id']
  }

  # read in all RL data for all signals, for all these tags
  all_rls <- extract_rls(rl_file = rl_file,
                         ping_log_file = ping_log_file,
                         signal = c('MFAS', 'Echosounder', 'Explosive'),
                         save_output = FALSE)

  # read in metadata about modeled RLs
  acous_model_meta <- readxl::read_xlsx(file.path(rl_model_dir,
                                                  'FB_ModelingDelivery_20210709_Unclass.xlsx'))

  # paste together file path(s) and tag file name(s)
  tags <- file.path(nc_path, tag_id)
  data_out <- list()

  # loop over tags
  for (t in c(1:length(tags))){
    cat(paste('tag: ', t, '\n'))
    if (exists('this_data')){
      rm(this_data)
    }
    if (exists('these_dive_cycles')){
      rm(these_dive_cycles)
    }
    if (exists('trk')){
      rm(trk)
    }
    if (exists('spd')){
      rm(spd)
    }
    if (exists('ptk')){
      rm(ptk)
    }
    if (exists('step_turn_data')){
      rm(step_turn_data)
    }
    if (exists('T')){
      rm(T)
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
    # if there's a file with lf A and M already in then use it
    # these were precreated in matlab
    this_data <- tryCatch(tagtools::load_nc(stringr::str_replace(tags[t],
                                                                 pattern = '.nc',
                                                                 replacement = '-lo.nc')),
                          error = function(e){
                            return(tagtools::load_nc(tags[t]))
                          })

    if (this_data$info$depid == "Zc-20180331-173187"){
      this_data$info$depid <- "Zica-20180331-173187"
    }



    # Add GPS info
    # note -- make function to turn GPS stuff into data frame?
    these_locs <- data.frame(this_data$GPS_position$data)

    if (ncol(these_locs) < 3){
      #   these_locs <- data.frame(this_data$GPS_position$sampling_rate,
      #                            these_locs[1,1],
      #                            these_locs[2,1])
      these_locs <- data.frame(t(this_data$GPS_position$data))
    }

    these_sats <- data.frame(this_data$GPS_satellites$data)
    if (ncol(these_sats) < 2){
      # these_sats <- data.frame(this_data$GPS_satellites$sampling_rate,
      # these_sats)
      these_sats <- data.frame(t(this_data$GPS_satellites$data))
    }

    these_resids <- data.frame(this_data$GPS_residual$data)
    if (ncol(these_resids) < 2){
      # these_resids <- data.frame(this_data$GPS_residual$sampling_rate,
      #                          these_resids)
      these_resids <- data.frame(t(this_data$GPS_residual$data))
    }
    these_timeerr <- data.frame(this_data$GPS_time_error$data)
    if (ncol(these_timeerr) < 2){
      # these_timeerr <- data.frame(this_data$GPS_time_error$sampling_rate,
      #                            these_timeerr)
      these_timeerr <- data.frame(t(this_data$GPS_time_error$data))
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
                      satellites >= 4)

    these_dive_cycles <- dcs %>%
      dplyr::filter(tag_id == this_data$info$depid)

    these_locs <- these_locs %>%
      dplyr::select(sec_since_tagon,
                    latitude,
                    longitude) %>%
      dplyr::filter(sec_since_tagon < max(dplyr::pull(these_dive_cycles, dive_cycle_end_sec) + 615, na.rm = TRUE)) %>%
      tidyr::drop_na(latitude, longitude)

    # add northing and easting
    these_locs <- dplyr::bind_cols(these_locs, tagtools::lalo2llf(as.matrix(these_locs[,2:3])) )

    # some GPS points are impossible. Remove by hand (will thus need to review all tagouts, removing ind points more than 2km off track)
    if (this_data$info$depid == "Zica-20191012-144029"){
      these_locs <- these_locs %>%
        dplyr::filter(northing < 5000 & easting > -1000 & easting < 20000)
    }
    # if (this_data$info$depid == "Zica-20190113-151361"){ # think this one OK
    #   these_locs <- these_locs %>%
    #     dplyr::filter()
    # }
    # if (this_data$info$depid == "Zica-20191012-145101"){ # this one OK
    #   these_locs <- these_locs %>%
    #     dplyr::filter()
    # }
    # if (this_data$info$depid == "Zica-20191111-94810"){ # this one OK
    #   these_locs <- these_locs %>%
    #     dplyr::filter()
    # }
    # if (this_data$info$depid == "Zica-20191117-195993"){ # this one OK
    #   these_locs <- these_locs %>%
    #     dplyr::filter()
    # }

    # add tagon time to dataset
    this_tagont <- lubridate::dmy_hms(this_data$info$dephist_device_datetime_start)
    these_dive_cycles$tag_recording_start_UTC <- this_tagont

    # compute tortuosity for each dive cycle
    these_dive_cycles$tortuosity = NA

    if ('Mw' %in% names(this_data) & 'Aw' %in% names(this_data)){ #only for tags with magnetometer and accel
      # compute anchored ptrack
      if ('Alo' %in% names(this_data)){
        this_data$Alo$data <- tidyr::replace_na(this_data$Alo$data, 0)
        this_data$Mlo$data <- tidyr::replace_na(this_data$Mlo$data, 0)
        this_data$Aw <- NULL
        this_data$Mw <- NULL
        gc()
      }else{
        this_data$Aw$data <- tidyr::replace_na(this_data$Aw$data, 0)
        this_data$Mw$data <- tidyr::replace_na(this_data$Mw$data, 0)
        this_data$Alo <- tagtools::decdc(this_data$Aw, df = this_data$Aw$sampling_rate)
        this_data$Aw <- NULL
        gc()
        this_data$Mlo <- tagtools::decdc(this_data$Mw, df = this_data$Mw$sampling_rate)
        this_data$Mw <- NULL
        gc()
      }

      this_data$depth$data <- zoo::na.approx(this_data$depth$data, x = c(1:nrow(this_data$depth$data)), na.rm = FALSE)
      this_data$depth$data <- tidyr::fill(data.frame(dpth = this_data$depth$data), dpth, .direction = "downup") %>%
        dplyr::pull(dpth)
      this_data$Alo$data <- zoo::na.approx(this_data$Alo$data,
                                           x = c(1:nrow(this_data$Alo$data)),
                                           na.rm = FALSE)
      spd <- abs(tagtools::speed_from_depth(tagtools::interp2length(this_data$depth,
                                                                    this_data$Alo),
                                            this_data$Alo))
      spd <- zoo::na.approx(spd,
                            x = c(1:length(spd)),
                            na.rm = FALSE)
      spd <- zoo::na.approx(spd, x = c(1:length(spd)),
                            na.rm = FALSE)
      spd <- tidyr::fill(data.frame(spd = spd),
                         spd, .direction = "downup") %>%
        dplyr::pull(spd) # to get initial NAs
      ptk <- tagtools::ptrack(A = this_data$Alo,
                              M = this_data$Mlo,
                              s = spd)
      # here pause and graph and verify that all GPS points are OK
      # gf_point(northing ~ easting, data = ptk) %>%
      # gf_point(northing ~ easting, data = these_locs, color = 'red')
      if (nrow(these_locs) == 1){
        # if there are not any GPS points other than the first then don't bother
        trk <- ptk
      }else{
        trk <- tagtools::fit_tracks(P = these_locs[,c('northing', 'easting')],
                                    T = these_locs %>% dplyr::pull(sec_since_tagon),
                                    D = ptk[,c('northing', 'easting')],
                                    sampling_rate = this_data$Alo$sampling_rate)
      }

      trk <- trk %>%
        dplyr::mutate(sec_since_tagon = (-1 + c(1:nrow(trk))) / this_data$Alo$sampling_rate,
                      tagon_lat = these_locs %>% dplyr::pull(latitude) %>% dplyr::first(),
                      tagon_lon = these_locs %>% dplyr::pull(longitude) %>% dplyr::first())


      if (save_tracks){
        if (missing(track_dir) || is.null(track_dir)){
          track_dir <- getwd()
        }
        track_fname <- file.path(track_dir,
                                 paste0(this_data$info$depid, '-georef-ptrack.csv'))
        readr::write_csv(trk, file = track_fname)
      }

      for (cy in c(1:nrow(these_dive_cycles))){
        cysi <- round(dplyr::pull(these_dive_cycles[cy, "dive_cycle_start_sec"]) * this_data$Alo$sampling_rate)
        cyei <- round(dplyr::pull(these_dive_cycles[cy, "dive_cycle_end_sec"]) * this_data$Alo$sampling_rate)
        T <- trk[seq(from = cysi, to = cyei), c('northing', 'easting')]
        these_dive_cycles[cy, 'tortuosity'] <- tagtools::tortuosity(T,
                                                                    sampling_rate = this_data$Alo$sampling_rate,
                                                                    intvl = (cyei - cysi + 1)/ this_data$Alo$sampling_rate)[1]
      } # end of loop over dive cycles
    } # end of "if there is Aw and Mw"

    # add in RL data
    # measured RL data for this whale
    these_rls <- all_rls %>%
      dplyr::filter(TagID == tag_id[t])

    # add in measured RLs
    if (nrow(these_rls) > 0){
      # add in measured RLs
      these_dive_cycles <- add_interval_rls(these_dive_cycles,
                                            ping_data = these_rls,
                                            start_x = dive_cycle_start_sec,
                                            end_x = dive_cycle_end_sec,
                                            start_ping = sec_since_tagon)
    }

    # add in MODELED RLs
    #model_fnames <- list.files(rl_model_dir, pattern = paste0(tag_id[t], '.*', '.csv'))
    these_model_meta <- acous_model_meta %>%
      dplyr::filter(TagID == tag_id[t])

    # add model results file names for each dive cycle plus modeled RLs?
    these_dive_cycles <- these_dive_cycles %>%
      dplyr::mutate(model_fnames = NA,
                    model_ids = NA,
                    model_data_source = NA,
                    model_sonar_type = NA,
                    model_rl_max_depth = NA,
                    model_rl_min_depth = NA,
                    model_rl_min = NA,
                    model_rl_max = NA,
                    model_rl_median = NA)

    for (cy in c(1:nrow(these_dive_cycles))){
      dcst <- these_dive_cycles %>%
        dplyr::pull(fd_start_UTC) %>%
        dplyr::nth(cy)
      dcet <- these_dive_cycles$fd_start_UTC[cy] +
        lubridate::seconds(these_dive_cycles %>%
                             dplyr::pull(dive_cycle_dur_sec) %>%
                             dplyr::nth(cy))
      this_cycle_meta <- these_model_meta %>%
        dplyr::filter(LocTime >= dcst &
                        LocTime < dcet)

      if (nrow(this_cycle_meta) > 0){
        these_dive_cycles$model_fnames[cy] <- list(paste(this_cycle_meta$TagID, '_',
                                                         this_cycle_meta$SonarID, '_',
                                                         this_cycle_meta$Type, '_',
                                                         stringr::str_pad(as.character(this_cycle_meta$ID),
                                                                          width = 5,
                                                                          side = 'left',
                                                                          pad = '0'), '.csv',
                                                         sep = '' ))
        these_dive_cycles$model_ids[cy] <- this_cycle_meta %>% dplyr::pull(ID) %>% list()
      }

      this_model <- list()
      if (nrow(this_cycle_meta) > 0){
        for (mf in c(1:nrow(this_cycle_meta))){
          this_file <- file.path(rl_model_dir, these_dive_cycles %>%
                                   dplyr::pull(model_fnames) %>%
                                   dplyr::nth(cy))
          this_file <- this_file[mf]

          ss <- dplyr::case_when(stringr::str_detect(string = this_file,
                                                     pattern = 'SPORTS') ~
                                   'SPORTS',
                                 stringr::str_detect(string = this_file,
                                                     pattern = 'ARCHIVE') ~
                                   'Archive',
                                 TRUE ~ 'Other')
          sty <- stringr::str_split(this_file, '_', simplify = TRUE)
          sty <- sty %>% dplyr::nth(length(sty) - 1)
            if (!file.exists(this_file)){
              # need to check if this_cycle_meta[mf] has ready = jpg or error = low snr
              this_model[[mf]] <- tibble::tibble(
                hyd_lat = NA,
                hyd_lon = NA,
                rl_depth_m = NA,
                rl = 0,
                model_data_source = ss,
                model_sonar_type = sty
              )
              if (dplyr::pull(this_cycle_meta, ready) %>% dplyr::nth(mf) == 'jpg'){
                this_model[[mf]] <- this_model[[mf]] %>%
                  dplyr::mutate(model_status = 'too quiet')
              }
            }else{
              this_model[[mf]] <- readr::read_csv(this_file,
                                                  show_col_types = FALSE) %>%
                dplyr::rename(hyd_lat = `Hyd Lat (deg)`,
                              hyd_lon = `Hyd Lon (deg)`,
                              rl_depth_m = `Received Depth (m)`,
                              rl = `Received Level (dB // 1uPa)`) %>%
                dplyr::mutate(model_data_source = ss,
                              model_sonar_type = sty,
                              model_status = 'OK')
            } # if file exists
          } # loop over mf
        this_model <- dplyr::bind_rows(this_model)
      }# end of if nrow(this_cycle_meta > 0)


          if ('rl_depth_m' %in% names(this_model)) {
            this_model2 <- this_model %>%
              dplyr::filter(rl_depth_m <= (dplyr::pull(these_dive_cycles, fd_max_depth) %>%
                                             dplyr::nth(cy)))

            these_dive_cycles$model_rl_max_depth[cy] <- this_model %>%
              dplyr::filter(rl_depth_m == plyr::round_any((these_dive_cycles %>%
                                                             dplyr::pull(fd_max_depth) %>%
                                                             dplyr::nth(cy)),
                                                          5,
                                                          f = floor) ) %>%
              dplyr::pull(rl) %>%
              max(na.rm = TRUE)

            # this will be the RL at the min modeled depth (since during a dive cycle the whale is @ zero depth sometimes)
            these_dive_cycles$model_rl_min_depth[cy] <- this_model %>%
              dplyr::filter(rl_depth_m == min(pull(this_model, rl_depth_m), na.rm = TRUE)) %>%
              dplyr::pull(rl) %>%
              max(na.rm = TRUE)

            these_dive_cycles$model_rl_min[cy] <- this_model2 %>%
              dplyr::pull(rl) %>%
              min(na.rm = TRUE)

            these_dive_cycles$model_rl_max[cy] <- this_model2 %>%
              dplyr::pull(rl) %>%
              max(na.rm = TRUE)
            these_dive_cycles$model_rl_median[cy] <- this_model2 %>%
              dplyr::pull(rl) %>%
              median(na.rm = TRUE)
            these_dive_cycles$model_data_source[cy] <- this_model %>%
              dplyr::pull(model_data_source) %>%
              unique() %>%
              sort() %>%
              stringr::str_c(collapse = ', ')
            these_dive_cycles$model_sonar_type[cy] <- this_model %>%
              dplyr::pull(model_sonar_type) %>%
              unique() %>%
              sort() %>%
              stringr::str_c(collapse = ', ')

          }# end of if nrow(this_data) > 0)
    } # end loop over dive cycles cy

    these_dive_cycles <- these_dive_cycles %>%
      dplyr::mutate(dplyr::across(starts_with('model_rl'),
                                  ~ifelse(.x < 0, 0, .x)))
    # tibble-concatenating
    data_out[[t]] <- these_dive_cycles
    data_out_all <- dplyr::bind_rows(data_out)
    if (save_csv){
      readr::write_csv(data_out_all, file = csv_name)
    }

  } # end of loop over TAGS

  return(data_out_all)
  }

## WHEW! This is the dive-cycle-scale data.


# Also need to make fine-scale (evenly sampled in time windows) data
# and add it in as additional rows (ie join the dive-cycle and fine scale @ end...)
