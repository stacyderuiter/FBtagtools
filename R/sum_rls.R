#' Sum RLs in dB
#'
#' Add up RLs in several frequency bands. Used by \code{\link[FBtagtools]{extract_rls}}.
#'
#' @param rls numeric vector with RLs in dB
#' @param energy Whether or not the RLs are in energy units (alternative is intensity)
#' @return sum of the received levels in `rls`
#' @export
#' @examples
#' coming soon
sum_rls <- function(rls,
                    energy = FALSE){
  if (!energy){
    rls <- na.omit(rls)
    10 * log10( sum(10 ^ (rls / 10)))
  }else{
    rls <- na.omit(rls)
    20 * log10( sum(10 ^ (rls / 20)))
  }
}
