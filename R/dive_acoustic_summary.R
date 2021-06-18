#' Dive-level acoustic and tag data, for one or more tags
#'
#' Summarize SMRT (and/or Lander2) tag data from .nc files for each foraging dive cycle. Note: Currently for only SMRT tags; function and help will be updated to allow inclusion of Lander2 data as well when possible.
#'
#' @param tag_id Character string or vector with tag IDs (without "-cal.nc"). Default: all SMRT ziphius tags.
#' @param path Directory (quoted string) where .nc files are stored. Can be one string, or a list the same length as tag_ids. Note: to download latest versions from Google drive, try function: \code{\link[FBtagtools]{download_drive_nc}}. Default: current working directory. Note: use "/" and not "\" to avoid headaches.
#' @param
#' @export
#' @examples
#' Examples will go here
dive_acoustic_summary <- function(tag_id = zc_smrt_tag_list,
                                  path = ''
){

  # paste together file path(s) and tag file name(s)
  tags <- file.path(path, tag_id)

  # empty list to store results
  data_out <- list()

  # loop over tags
  for (t in c(1:length(tags))){

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

    # placeholder to make sure the file-reading, tibble-concatenating code works
    data_out[[t]] <- tibble::tibble(depth = this_data$depth$data,
                            tag_id = tag_id[t])

  } # end of loop over tags

  # make sure we have one df and not a list of them
  data_out <- dplyr::bind_rows(data_out)
  return(data_out)
  }

#
#
#   1. Duration of clicking - start time of clicking to end time of clicking
# 2. Duration of dive  - end time of the surfacing prior to the foraging dive to the start time of the surfacing after the foraging event.
# 3. Time it takes for clicking to start once dive begins and time it takes once clicking stops for the animal to surface.
# 4. When is clicking occurring? Day or night?
#   5. What depth was the animal at when clicking started and ended?
#   6. Min/Max dive depth while foraging
# 7. Estimated distance traveled during foraging dive
# 8. How many foraging dives had other BWs detected? This would be noted by "Other BW" as the event label right next to a "FD".
# 9. Where is clicking occurring as it relates to bottom depth  For the abstract, David used this for the bottom depth: https://github.com/dasweeney4423/tagproc/blob/master/R/bathy.sync.R. Can we add something like this to your code? Is there a better way?
#
#
# 10. Number of buzzes that occurred during the foraging dive? This would be using labels where it says "BW Buzz", NOT poss or probable BW buzz. If we need to clean up our buzz dataset a bit more before we look at buzzes I can do that.
# 11. What depth do buzzes occur?
#   12. Number of clicks per foraging dive
# 13. Duration of dives between day and night
# 14. Percent of time clicking and total clicks produced
# 15. Percent of time clicking compared to the full dive time
# 16. Did clicking start/end deeper/shallower during day or night?
#
#   There is a folder of seafloor data here that Eric Keene obtained and we have used for various west coast things: FreakinBeakinTagData\Acoustic Preliminary Results\Resources-Seafloor.zip
#
# The code at the following link can also be used to pull it for desired locations (Eric's code that I have put into a github repo): https://github.com/dasweeney4423/tagproc/blob/master/R/bathy.sync.R
#
# This code can also pull data from a NOAA server using marmap::getNOAA.bathy, but the NOAA server version gives errors that I think have to do with limits to how many users can access the server at a time, which I can't figure out how to determine and it has poorer resolution.
#
#                                                                                   Also, the one time we have used dusk and dawn classifications for SOCAL Zc, we used the following code:
#
#                                                                                     solarstage <- function(time, lat, long) {
#                                                                                       elev <- oce::sunAngle(time, long, lat)$altitude
#                                                                                       #categorical returns
#                                                                                       if (elev > 6) {

