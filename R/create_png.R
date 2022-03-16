#' Creates a png file
#'
#' @param seeds Values to set the random seeds in \code{gen_simplex}.
#' @param size Number of points.
#' @param anchor_layout Either "random", "spiral", or "diamond".
#' @param hue_turn Degree to rotate all hue values.
#' @param color_scheme Either "subset" or "full" for hue range.
#' @param color_subset_center Center hue value when using subset color_scheme
#' @param color_subset_width Width hue value when using subset color_scheme
#' @param alpha_taper Direction the paths taper. Options are "start", "end",
#' and "both". "start" fades away at the anchor points while "end" fades away in
#' the opposite direction. "both" fades in both direction.
#' @param output_file File to save the png.
#' @importFrom stats median
#' @importFrom rlang .data
#' @return png file
#' @export
create_png <- function(seeds, size, anchor_layout, hue_turn, color_scheme,
                       color_subset_center, color_subset_width,
                       alpha_taper, output_file) {
  points <- get_anchor_points(
    seeds, size, anchor_layout,
    hue_turn, color_scheme,
    color_subset_center, color_subset_width
  )
  paths <- get_paths(points, seeds)
  point_paths <- get_point_paths(points, paths)

  if (alpha_taper == "start") {
    paths <- paths %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(alpha_value = 1 - (max(.data$time) - .data$time) /
        (max(.data$time) + 1)) %>%
      dplyr::ungroup()
  } else if (alpha_taper == "end") {
    paths <- paths %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(alpha_value = 1 - (.data$time / (max(.data$time) + 1))) %>%
      dplyr::ungroup()
  } else {
    paths <- paths %>%
      dplyr::group_by(.data$id) %>%
      dplyr::mutate(alpha_value = 1 - (abs(.data$time - stats::median(.data$time)) /
        (median(.data$time) + 1))) %>%
      dplyr::ungroup()
  }

  axes_limits <- max(c(abs(c(
    paths$x,
    paths$y,
    paths$xend,
    paths$yend
  ))))

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = point_paths,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        color = .data$hex_color,
        alpha = .data$alpha_value
      ),
      size = .25,
      stroke = 0, shape = 16
    ) +
    ggplot2::geom_segment(
      data = paths,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        xend = .data$xend, yend = .data$yend,
        color = .data$hex_color,
        alpha = .data$alpha_value
      ),
      lineend = "round",
      linejoin = "round",
      size = .15
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = c(-axes_limits, axes_limits)) +
    ggplot2::scale_y_continuous(limits = c(-axes_limits, axes_limits)) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(
      color = "white",
      fill = "white"
    ))

  ggplot2::ggsave(
    filename = output_file,
    device = "png",
    widt = 2.5,
    height = 2.5
  )
}
