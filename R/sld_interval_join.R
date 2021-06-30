# #
# # # POC for joining based on intervals
# #
# # library(rlang)
# # set.seed(12345)
# #
# # tt <- sort(sample(1:100, 20))
# #
# # A <-
# #   tibble(
# #     start = tt[c(TRUE, FALSE)],
# #     end = tt[c(FALSE, TRUE)],
# #     other = sample(LETTERS, 10)
# #   )
# #
# # B <- tibble(
# #   time = sample(1:100, 25),
# #   more = sample(LETTERS, 25)
# # )
#
# # x, y -- data frames
# # join to rows of x, the rows of y where x$start_x <= y$start_y and x$end_x >= y$end_y
# # if end_y is not specified, then start_x is used. this allows y to contain "point events" rather than
# #   interval events
#
# interval_join_faster <-
#   function(x, y, start_x, start_y, end_x = start_x, end_y = NULL, suffix = c("", ".new"),
#            ..., keep = FALSE) {
#     start_x = enquo(start_x)
#     end_x = enquo(end_x)
#     start_y = enquo(start_y)
#
#     if (is.null(end_y)) {
#       end_y = start_y
#     } else {
#       end_y = enquo(end_y)
#     }
#
#     x <- x |> arrange(!!start_x)
#     y <- y |> arrange(!!start_y)
#     x <- x |>
#       mutate(..index.. = 1:n())
#
#     # y <- y |>
#     #   mutate(..index1.. = purrr::map_dbl(!!start_y, ~ max(which(x |> pull(!!start_x) <= .x)))) |>
#     #   mutate(..index2.. = purrr::map_dbl(!!end_y,   ~ min(which(x |> pull(!!end_x)   >= .x)))) |>
#     #   mutate(..index..  = ..index1..) |>
#     #   filter(..index1.. == ..index2..)
#
#     y <- y |>
#       mutate(
#         ..index1.. =
#           findInterval(y |> pull(!!start_y), x |> pull(!!start_x), rightmost.closed = TRUE),
#         ..index2.. =
#           findInterval(y |> pull(!!end_y), x |> pull(!!end_x), rightmost.closed = TRUE),
#         ..index..  =
#           ..index1..)
#
#     y <- y |> filter(..index1.. == ..index2.. + 1)
#
#     left_join(x, y, by = "..index..") |>
#       select(- matches("\\.\\.index"))
#   }

# A |>
#   interval_join(B, start_x = start, end_x = end, start_y = time) |>
#   reactable::reactable()
#
# # example use combining info across rows of B that match a row of A
# A |>
#   interval_join(B, start_x = start, end_x = end, start_y = time) |>
#   filter(!is.na(time)) |>
#   group_by(start, end) |>
#   summarise(
#     n = n(),
#     mean_time = round(mean(time), 2),
#     more = paste(more, collapse = " | ")
#   ) |>
#   reactable::reactable()
