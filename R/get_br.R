#' Get fluke stroking signal from magnetometer data
#'
#'For derivation see: Martin López L, Aguilar de Soto N, Miller P, Johnson M 2016: Tracking the kinematics of caudal-oscillatory swimming: A comparison  of two on-animal sensing methods. J Exp Biol 219:2103-2109. Note: to estimate the stroking rate of small animals for which the specific acceleration is larger, it may be simpler to find the zero crossings on the 	high-pass-filtered acceleration (z axis for cetaceans, y axis for pinnipeds and fish) directly rather than using get_br.
#'
#' @param Ma input data: sensor data structure, or matrix with magnetometer data
#' @param sampling_rate sampling rate of Ma in Hz (required if Ma is not a sensor data structure, and ignored if it is)
#' @param fh is the high-pass filter frequency in Hz to use to separate orientation changes from locomotory strokes. It should be about half of the dominant stroke frequency if needed. Use \code{\link[tagtools]{dsf}} to estimate the dominant stroke frequency.
#' @param thr is an optional minimum field strength threshold to prevent errors in the computation. Errors arise if the plane of rotations is nearly perpendicular to the local magnetic field vector. To avoid these, the body rotation signal is replaced with NA if the field strength in the locomotory plane drops below thr fraction of the total field strength.
#' @param ax is an optional indicator that the locomotion is in the x-y plane. The function expects the locomotion to be in the x-z plane (e.g., cetacean swimming) by default. To comoute body rotations in x-y plane (e.g., for pinnipeds and many fish), use ax='y'.
#' @return The body rotation signal in radians. It is a vector with the same sampling rate and number of samples as Ma.
#' @importFrom magrittr "%>%"
#' @export

get_br <- function(Ma, sampling_rate, fh, thr = 0.2, ax = 'z'){
  if (missing(sampling_rate) && !(is.list(Ma))){
    error('If Ma is not an animaltags sensor data structure, then sampling_rate is required.\n')
  }

  if (is.list(Ma) & 'data' %in% names(Ma)){
    Ma0 <- Ma
    sampling_rate <- Ma$sampling_rate
    Ma <- Ma$data
  }

  mfs <- mean(tagtools::norm2(Ma), na.rm = TRUE) # mean magnetic field strength
  Mf <- tagtools::comp_filt(Ma, sampling_rate, fh) # split the M signals into low-pass and high-pass
  Ml <- Mf[[1]]
  Mh <- Mf[[2]]

  if (ax == 'y'){
    m2 <- Ml[,1]^2 + Ml[,2]^2 # the magnitude-squared of Ml in the [x,y] sub-space
    ph <- Re(asin((Mh[,1]*Ml[,2] - Mh[,2]*Ml[,1]) / m2)) # estimate the body rotations
  }else{
    m2 <- Ml[,1]^2 + Ml[,3]^2 # the magnitude-squared of Ml in the [x,z] sub-space
    ph <- Re(asin((Mh[,1]*Ml[,3] - Mh[,3]*Ml[,1]) / m2)) # estimate the body rotations
  }


  ph[m2 < thr*mfs^2] <- NA # blank out rotations when the planar field is too small

  return(ph)

  }
