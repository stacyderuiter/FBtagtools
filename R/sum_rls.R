#' Sum RLs in dB
#'
#' Add up RLs in several frequency bands. Used by \code{\link[FBtagtools]{extract_rls}}.
#'
#' @param rls numeric vector with RLs in dB
#' @return sum of the received levels in `rls`
#' @export
#' @examples
#' # coming soon
sum_rls <- function(rls){
    rls <- stats::na.omit(rls)
    10 * log10( sum(10 ^ (rls / 10)))
}
