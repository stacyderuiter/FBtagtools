#' Get tagon time from an nc file. Returns one datetime.
#'
#' Read in data from .nc file to obtain the tag recording start time
#'
#' @param tag_id Character string or vector with tag IDs (without "-cal.nc"). Default: all SMRT ziphius tags.
#' @param nc_path Directory (quoted string) where nc files are kept (for fetching tag recording start times). Default: same as ping log path
#' @return a list of datetimes (when the tag record(s) started)
#' @export

get_tagon_time <- function(tag_id,
                           nc_path){
  if (stringr::str_ends(nc_path, '/')){
    nc_path <- stringr::str_trunc(nc_path, width = stringr::str_length(nc_path) - 1,
                                  side = 'right', ellipsis = '')
  }

  if (length(tag_id) > 1 && length(nc_path) == 1){
    nc_path = rep(nc_path, times = length(tag_id))
  }else{
    if (length(tag_id) != length(nc_path)){
      stop('tag_id and nc_path inputs to get_tagon_time() are not the same length.')
    }
  }

  sts <- list()
  for (t in c(1:length(tag_id))){
  thisNC <- tagtools::load_nc(file.path(nc_path[t], paste0(tag_id[t], '-cal.nc')))
  this_info <- thisNC$info
  # UTC start time of tag record
  this_st <- this_info$dephist_device_datetime_start |>
    lubridate::dmy_hms(tz = 'UTC')
  sts[[t]] <- this_st
  rm(thisNC, this_info)
  gc(verbose = FALSE)
  }

  return(do.call('c', sts))
}
