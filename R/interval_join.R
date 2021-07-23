#' Join data at different time-scales
#'
#' Given one dataset where each row represents an interval at coarser scale (for example, one row per dive with dive start/end times), pull in summary information about finer-time-scale data. For example, add the max RL during each dive, or the median MSA during each dive.
#'
#' @param x interval data frame or tibble (data with one row per dive, dive-cycle, day, etc.)  Should have columns that provide start and end times of each interval; these can be date-times or any numeric time indicator
#' @param y data frame at finer time-scale with information to summarize and pull into `x`
#' @param start_x name of variable with interval start times in `x`
#' @param start_y name of variable in `y` that contains sample times or event-start times
#' @param end_x name of variable in `x` that contains interval end times (defaults to `start_x` if not given)
#' @param end_y name of variable in `y` that contains event or sample end times (defaults to `start_y` if not given)
#' @param suffix 	(Passed to \code{\link[dplyr]{left_join}}; most users can ignore) If there are non-joined duplicate variables in x and y, these suffixes will be added to the output to disambiguate them. Should be a character vector of length 2.
#' @param keep (Passed to \code{\link[dplyr]{left_join}}; most users can ignore and keep the default value, FALSE) Should the join keys from both x and y be preserved in the output?
#' @param ... Additional arguments to pass to \code{\link[dplyr]{left_join}}
#' @return A data.frame like the input interval dataset `x`, but with additional columns for the new summarized variables
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' Examples will go here
interval_join <- function(x, y, start_x, start_y,
                          end_x = start_x, end_y,
                          suffix = c("", ".new"),
                          ..., keep = FALSE){
    start_x = rlang::enquo(start_x)
    end_x = rlang::enquo(end_x)
    start_y = rlang::enquo(start_y)

    if (missing(end_y)) {
      end_y = start_y
    } else {
      end_y = rlang::enquo(end_y)
    }

    x <- x %>% dplyr::arrange(!!start_x)
    y <- y %>% dplyr::arrange(!!start_y)
    x <- x %>%
      dplyr::mutate(..index.. = 1:dplyr::n())

    # original version - perhaps slower?
    y <- y %>%
      dplyr::mutate(..index1.. = purrr::map_dbl(!!start_y, ~ max(which(x %>% dplyr::pull(!!start_x) <= .x)))) %>%
      dplyr::mutate(..index2.. = purrr::map_dbl(!!end_y,   ~ min(which(x %>% dplyr::pull(!!end_x)   >= .x)))) %>%
      dplyr::mutate(..index..  = ..index1..) %>%
      dplyr::filter(..index1.. == ..index2..)

    # # start of new bit
    # y <- y %>%
    #         dplyr::mutate(
    #           ..index1.. =
    #             findInterval(y %>% dplyr::pull(!!start_y),
    #                          x %>% dplyr::pull(!!start_x),
    #                          rightmost.closed = TRUE),
    #           ..index2.. =
    #             findInterval(y %>% dplyr::pull(!!end_y),
    #                          x %>% dplyr::pull(!!end_x),
    #                          rightmost.closed = TRUE),
    #           ..index..  =
    #             ..index1..)
    #
    # y <- y %>% dplyr::filter(..index1.. == ..index2.. + 1)
    # # end of new bit

    dplyr::left_join(x, y, by = "..index..",
                     na_matches = "never") %>%
      dplyr::select( - tidyselect::matches("..index"))
  }
