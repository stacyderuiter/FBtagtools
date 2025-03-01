% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_rls.R
\name{extract_rls}
\alias{extract_rls}
\title{extract RL data}
\usage{
extract_rls(
  rl_file = c("/Users/sld33/Dropbox/FBdata/RLs/Zica-20191012-145101.csv",
    "/Users/sld33/Dropbox/FBdata/RLs/Zica-20191117-195993.csv",
    "/Users/sld33/Dropbox/FBdata/RLs/Zica-20211112-94819.csv",
    "/Users/sld33/Dropbox/FBdata/RLs/Zica-20211113-195993.csv",
    "/Users/sld33/Dropbox/FBdata/RLs/Zica-20220112-195994.csv",
    "/Users/sld33/Dropbox/FBdata/RLs/Zica-20191012-144029.csv",
    "/Users/sld33/Dropbox/FBdata/RLs/Zica-20191111-94810.csv"),
  email,
  save_output = TRUE,
  path = getwd(),
  out_file,
  signal = "MFAS",
  overwrite = TRUE
)
}
\arguments{
\item{rl_file}{name (with path, if needed), or a vector of names, of locally-stored .csv file with raw RL data. If not provided, the default is to use \code{\link[FBtagtools]{download_drive_rls}} to obtain all available RLs from the FB Google Drive.}

\item{email}{Email address (required for FB Google Drive authentication; optional if \code{rl_file} is provided). You may also be asked to sign in or verify your Google identity as this function runs.}

\item{save_output}{Logical; whether or not to save results in a .csv file. Default is TRUE.}

\item{path}{Quoted string with the path to the directory where you want to save the output file. Defaults to the current working directory. Final "/" not needed. Use "/" rather than "\" to avoid possible headaches.}

\item{out_file}{Name (quoted string) for output .csv file with results. Optional; default file name is constructed based on signal type and frequency band requested.}

\item{signal}{Quoted string (or vector of them) indicating which signal types to return RLs for. Options are one or more of: 'MFAS', 'Echosounder', 'Explosive'. Default: 'MFAS'. Note: CAS is currently marked as MFAS type. You can isolate CAS pings and events by getting MFAS RLs and then keeping only pings with duration 20 seconds or longer.}

\item{overwrite}{Whether or not to overwrite an existing output file. Logical. Default: TRUE.}
}
\value{
Returns a data.frame with RLs (one row per ping). If \code{save_output} is true, also saves a csv file in directory \code{path} with filename \code{out_file.csv} with the results.
}
\description{
This function processes the "raw" format of RL data, allowing users to obtain RLs of desired types and in selected frequency bands. If you need direct access to the (more complicated) raw data, see \code{\link[FBtagtools]{download_drive_rls}}.
}
\details{
For MFAS: Events are marked type "MFA". This also includes CAS exposures (which, if we have separated them out before, it is by choosing MFA pings of duration more than 20 seconds). Measurements were made in ANSI-standard 1/3 octave bands centered from 1-40kHz. For MFA events, bands with center frequencies less than 9kHz are considered. For max RMS level, Units are: dB re 1 muPa.  Measured in 200 msec windows; reported level is the highest in any one window. (Following conventions of SOCAL BRS, 3S projects.) For Echosounder events, selected bands are between 10-14kHz. For SPLs in individual 1/3 octave bands, the RL is "missing" (NA) if the SNR in that band was less than 6dB (and/or if the frequency band in question is no relevant for the selected signal type). For Explosions, all 1/3 octave bands below 5 kHz are included. Note that the function has been updated in 2024 along with updates to the RL calculation tool chain. Ping times from the audits are now kept in the output files from RL calculations, so it is not necessary to have the audit ("ping log") files AND the RL results to do this RL processing.
}
