#' Creates a mp4 file
#'
#' @param seeds Values to set the random seeds in \code{gen_simplex}.
#' @param size Number of points.
#' @param anchor_layout Either "random", "spiral", or "diamond".
#' @param hue_turn Degree to rotate all hue values.
#' @param color_scheme Either "subset" or "full" for hue range.
#' @param color_subset_center Center hue value when using subset color_scheme
#' @param color_subset_width Width hue value when using subset color_scheme
#' @param movement Determines how the paths are animated. Options are "march"
#' and "glide". "march" moves small parts along the path. "glide" fills the path
#' entirely then shrinks it away.
#' @param output_file File to save the mp4
#' @importFrom rlang .data
#' @return mp4 file
#' @export
create_mp4 <- function(seeds, size, anchor_layout, hue_turn, color_scheme,
                       color_subset_center, color_subset_width,
                       movement, output_file) {
  points <- get_anchor_points(
    seeds, size, anchor_layout,
    hue_turn, color_scheme,
    color_subset_center, color_subset_width
  )
  paths <- get_paths(points, seeds)
  point_paths <- get_point_paths(points, paths)

  axes_limits <- max(c(abs(c(
    paths$x,
    paths$y,
    paths$xend,
    paths$yend
  ))))

  tp_dr <- tempdir()

  if (movement == "march") {
    max_frame <- max(paths$time) + 9
    for (frame in 0:max_frame) {
      sub_paths <- paths %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$time > frame - 10 & .data$time <= frame) %>%
        dplyr::mutate(alpha_value = 1 - (max(.data$time) - .data$time) / 10)

      if (frame <= 10) {
        point_paths <- point_paths %>%
          dplyr::mutate(alpha_value = (10 - frame) / 10)
      } else {
        point_paths <- point_paths[0, ]
      }

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
          data = sub_paths,
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
        filename = file.path(
          tp_dr,
          paste0("image_", stringr::str_pad(frame, 3, pad = "0"), ".png")
        ),
        widt = 2.5,
        height = 2.5
      )
    }
  } else {
    max_frame <- max(paths$time) * 2
    for (frame in 0:max_frame) {
      if (frame <= max_frame / 2) {
        sub_paths <- paths %>%
          dplyr::filter(.data$time <= frame)
      } else {
        sub_paths <- paths %>%
          dplyr::filter(.data$time >= frame - max_frame / 2)

        point_paths <- point_paths[0, ]
      }

      ggplot2::ggplot() +
        ggplot2::geom_point(
          data = point_paths,
          ggplot2::aes(
            x = .data$x, y = .data$y,
            color = .data$hex_color
          ),
          size = .25,
          stroke = 0, shape = 16
        ) +
        ggplot2::geom_segment(
          data = sub_paths,
          ggplot2::aes(
            x = .data$x, y = .data$y,
            xend = .data$xend, yend = .data$yend,
            color = .data$hex_color
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
        filename = file.path(
          tp_dr,
          paste0("image_", stringr::str_pad(frame, 3, pad = "0"), ".png")
        ),
        widt = 2.5,
        height = 2.5
      )
    }
  }

  imgs <- file.path(tp_dr, list.files(tp_dr, pattern = "^image_...\\.png$"))
  av::av_encode_video(imgs,
                      output = output_file,
                      framerate = 15,
                      verbose = FALSE)

  # Clean up
  unlink(imgs)
}
