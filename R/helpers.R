#' Gets the hex code color based on angle and percentage.
#'
#' \code{flowfieldsfigment} uses a base color scheme covering all hues.
#'
#' @param angle Angle in degrees \[0, 360\]. Closely matches hue in HCL.
#' @param percentage Percent \[0, 100\]. Closely matches chroma in HCL.
#' @return Hex code for a color value.
#' @importFrom grDevices hcl
#' @examples
#' get_color(0, 80)
#' get_color(90, 50)
#' @export
get_color <- function(angle, percentage) {
  vectors <- data.frame(v1 = c(0.99258009214842,
                               0,
                               -0.121592601216663),
                        v2 = c(0.0172593263893888,
                               0.989874705747223,
                               0.14089067596698),
                        p = c(0,
                              0,
                              74.8443331534229))

  v <- vectors[["p"]] +
    (percentage / 100 * 63) * cos(angle * pi/180) * vectors[["v1"]] +
    (percentage / 100 * 63) * sin(angle * pi/180) * vectors[["v2"]]

  hue <- (atan2(v[2], v[1]) * 180/pi) %% 360
  chroma <- sqrt(v[1]^2 + v[2]^2)
  luminance <- v[3]

  grDevices::hcl(hue, chroma, luminance, fixup = FALSE)
}

#' Gets the hex code color from get_color augmented by center and width.
#'
#' This function converts the color scheme to use a subset of hues.
#' \code{get_color_subset} maps the the angle to be \code{center} when 0 and 180
#' and stretch to \code{center - width} at 90 to \code{center + width} as 270.
#'
#' @param center Center hue angle \[0, 360\] for the color scheme.
#' @param width Width in angles from center to edge for hue values.
#' @param angle Angle in degrees \[0, 360\]. Closely matches hue in HCL.
#' @param percentage Percent \[0, 100\]. Closely matches chroma in HCL.
#' @return Hex code for a color value.
#' @examples
#' get_color_subset(90, 30, 90, 100)
#' get_color_subset(180, 90, 270, 80)
#' @export
get_color_subset <- function(center, width, angle, percentage) {
  get_color(width * sin(angle * pi/180) + center, percentage)
}

#' Internal function to get directional vectors.
#'
#' This function uses the \code{ambient} package to generate noise for the
#' points to follow.
#'
#' @param points Current position of the points.
#' @param seeds Values to set the random seeds in \code{gen_simplex}.
#' @importFrom rlang .data
#' @return The direction vectors the points should move.
get_vectors <- function(points, seeds) {
  vectors <- points %>%
    dplyr::mutate(x_direction = ambient::gen_simplex(.data$x,
                                                     .data$y,
                                                     frequency = .01,
                                                     seed = seeds[1]),
                  y_direction = ambient::gen_simplex(.data$x,
                                                     .data$y,
                                                     frequency = .01,
                                                     seed = seeds[2])) %>%
    dplyr::mutate(vector_length = sqrt(.data$x_direction^2 +
                                         .data$y_direction^2)) %>%
    dplyr::mutate(x_direction = .data$x_direction / .data$vector_length,
                  y_direction = .data$y_direction / .data$vector_length) %>%
    dplyr::select(-.data$vector_length)
}

#' Internal function to move points by one time period.
#'
#' This function uses \code{get_vectors} to determine where
#' the points should go.
#'
#' @param points Current position of the points.
#' @param seeds Values to set the random seeds in \code{gen_simplex}.
#' @importFrom rlang .data
#' @return The next steps for points.
move_points <- function(points, seeds) {
  points <- points %>%
    get_vectors(seeds) %>%
    dplyr::mutate(x = .data$x + .data$x_direction * .5,
                  y = .data$y + .data$y_direction * .5,
                  time = .data$time + 1)
  return(points)
}

#' Internal function to get the starting points and their attributes.
#'
#' @param seeds Values to set the random seeds in \code{gen_simplex}.
#' @param size Number of points.
#' @param anchor_layout Either "random", "spiral", or "diamond".
#' @param hue_turn Degree to rotate all hue values.
#' @param color_scheme Either "subset" or "full" for hue range.
#' @param color_subset_center Center hue value when using subset color_scheme
#' @param color_subset_width Width hue value when using subset color_scheme
#' @importFrom stats pnorm
#' @importFrom stats sd
#' @importFrom rlang .data
#' @return Dataframe of points with attributes
get_anchor_points <- function(seeds, size, anchor_layout,
                              hue_turn, color_scheme,
                              color_subset_center, color_subset_width){
  if(anchor_layout == "random") {
    # random layout
    points <- as.data.frame(mvtnorm::rmvnorm(n = size,
                                             sigma = diag(size * 4 / (floor(log10(size)) + 1),
                                                          nrow = 2))) %>% # sd is number 4/digits
      dplyr::rename(x = .data$V1,
                    y = .data$V2) %>%
      dplyr::mutate(id = dplyr::row_number())

  } else if(anchor_layout == "spiral") {
    # spiral layout
    golden <- ((sqrt(5) + 1) / 2) * (2 * pi)

    points <- data.frame(x = sqrt(seq(1, size)) * cos(golden * seq(1, size)) * 2.5,
                         y = sqrt(seq(1, size)) * sin(golden * seq(1, size)) * 2.5) %>%
      dplyr::mutate(id = dplyr::row_number())

  } else {
    # diamond grid
    grid_width <- ifelse(size <= 1500/2, ceiling(sqrt(size)), floor(sqrt(size)))

    points <- tidyr::expand_grid(x_start = seq(1, grid_width) - (grid_width / 2) - .5,
                                 y_start = seq(1, grid_width) - (grid_width / 2) - .5) %>%
      dplyr::mutate(x = (.data$x_start * cos(45 * pi/180) -
                           .data$y_start * sin(45 * pi/180)) * 5,
                    y = (.data$x_start * sin(45 * pi/180) +
                           .data$y_start * cos(45 * pi/180)) * 5,
                    id = dplyr::row_number()) %>%
      dplyr::select(-.data$x_start, -.data$y_start)
  }

  if(color_scheme == "subset") {
    points <- get_vectors(points, seeds) %>%
      dplyr::mutate(distance = ambient::gen_simplex(.data$x,
                                                    .data$y,
                                                    frequency = .01,
                                                    seed = seeds[3])) %>%
      dplyr::mutate(angle =
                      (atan2(.data$y_direction, .data$x_direction) * 180/pi) %%
                      360,
                    percentage = stats::pnorm(.data$distance, mean = 0,
                                       stats::sd(.data$distance)) * 100) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hex_color = get_color_subset(color_subset_center,
                                                 color_subset_width,
                                                 .data$angle,
                                                 .data$percentage)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(x_color = .data$percentage * cos(.data$angle * pi/180),
                    y_color = .data$percentage * sin(.data$angle * pi/180),
                    time = 0) %>%
      dplyr::select(.data$id, .data$x, .data$y,
                    .data$hex_color, .data$percentage, .data$time)
  } else {
    points <- get_vectors(points, seeds) %>%
      dplyr::mutate(distance = ambient::gen_simplex(.data$x,
                                                    .data$y,
                                                    frequency = .01,
                                                    seed = seeds[3])) %>%
      dplyr::mutate(angle =
                      (atan2(.data$y_direction, .data$x_direction) * 180/pi) %%
                      360 + hue_turn,
                    percentage = stats::pnorm(.data$distance, mean = 0,
                                       stats::sd(.data$distance)) * 100) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(hex_color = get_color(.data$angle, .data$percentage)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(x_color = .data$percentage * cos(.data$angle * pi/180),
                    y_color = .data$percentage * sin(.data$angle * pi/180),
                    time = 0) %>%
      dplyr::select(.data$id, .data$x, .data$y,
                    .data$hex_color, .data$percentage, .data$time)
  }

  return(points)
}

#' Internal function to get the points' full paths.
#'
#' This starts points along their paths. Paths are shorten when either the
#' points hit a plateau (vectors are 0 for both x and y) or when filtered by
#' percentage.
#'
#' @param points Current position of the points.
#' @param seeds Values to set the random seeds in \code{gen_simplex}.
#' @importFrom rlang .data
#' @return Dataframe of paths.
get_paths <- function(points, seeds) {
  paths <- purrr::accumulate(.x = rep(list(seeds), 100),
                             .f = move_points,
                             .init = points)
  paths <- dplyr::bind_rows(paths)
  paths <- paths %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(xend = dplyr::lead(.data$x),
                  yend = dplyr::lead(.data$y)) %>%
    dplyr::filter(!is.na(.data$xend)) %>%
    dplyr::filter(.data$time <= .data$percentage)
}

#' Internal function to handle edge case of when points start on a plateau.
#'
#' @param points Current position of the points.
#' @param paths Paths the points take.
#' @importFrom rlang .data
#' @return Dataframe of points with attributes for time 0.
get_point_paths <- function(points, paths) {
  # Handle paths that were dropped because they didn't go anywhere
  point_paths <- points %>%
    dplyr::anti_join(paths, by = "id") %>%
    dplyr::mutate(hex_color = get_color(0, 0),
                  alpha_value = 1)
}
