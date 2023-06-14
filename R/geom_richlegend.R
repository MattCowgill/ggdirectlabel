#' Rich legend
#' @description `geom_richlegend()` draws coloured text in lieu of a legend
#' @param legend.position Either:
#'
#'  - a two-element numeric vector such as `c(0.2, 0.9)`. The first element
#'  denoted the x-position of the data and the second element denotes the
#'  y-position. Each element must be between 0 and 1 (inclusive).
#'
#'  - one of the following strings: "left", "right", "bottom", "top",
#'  "topleft", "topright", "bottomleft", "bottomright".
#'
#'  Default is "topright", which is equivalent to `c(0.975, 0.975)`.
#'
#' @inheritParams ggtext::geom_richtext
#' @examples
#' library(ggplot2)
#'
#' base_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'   geom_point() +
#'   theme(legend.position = "none")
#'
#' # By default, the rich legend is placed at the "topright"
#' base_plot +
#'   geom_richlegend(aes(label = cyl))
#'
#' # You can change the position of the rich legend:
#' base_plot +
#'   geom_richlegend(aes(label = cyl),
#'                   legend.position = "top")
#'
#' # Or you can change the position using a numeric vector:
#' base_plot +
#'   geom_richlegend(aes(label = cyl),
#'                   legend.position = c(0.1, 0.1))
#'
#' # The legend respects facets:
#' base_plot +
#'   geom_richlegend(aes(label = cyl)) +
#'   facet_wrap(~cyl)
#'
#'
#' @import ggplot2
#' @rdname geom_richlegend
#' @export
geom_richlegend <-
  function(mapping = NULL,
           data = NULL,
           legend.position = "topright",
           na.rm = FALSE,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = "identity",
      geom = GeomRichLegend,
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        legend.position = legend.position,
        ...
      )
    )
  }

#' @rdname geom_richlegend
#' @export
GeomRichLegend <- ggplot2::ggproto(
  "GeomRichLegend",
  ggplot2::Geom,
  extra_params = c("na.rm",
                   "legend.position"),
  setup_data = function(data, params) {

    ggtext::GeomRichText$setup_data(data, params) |>
      dplyr::group_by(label, group, PANEL, colour) |>
      dplyr::summarise() |>
      dplyr::ungroup() |>
      dplyr::mutate(legend.position = list(params$legend.position))
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        flipped_aes = FALSE) {

    xy <- legend_pos_to_xy(data$legend.position[[1]],
                           panel_params$x.range,
                           panel_params$y.range)

    richtext_data <- data |>
      dplyr::mutate(
        label = paste0(
          "<span style='color:",
          colour,
          "'>",
          label,
          "</span>"
        ),
        x = xy[1],
        y = xy[2]
      ) |>
      dplyr::group_by(
        PANEL, x, y, size, angle, hjust, vjust,
        alpha, lineheight, family, fontface
      ) |>
      dplyr::summarise(label = paste0(label, collapse = "<br>")) |>
      dplyr::ungroup() |>
      as.data.frame() |>
      dplyr::mutate(colour = "#000000")

    rt_draw_panel(
      richtext_data,
      panel_params,
      coord
    )
  },
  draw_key = ggplot2::draw_key_text,
  required_aes = c(
    "label",
    "colour"
  ),
  default_aes = ggplot2::aes(
    colour = ggtext::GeomRichText$default_aes$colour,
    size = ggtext::GeomRichText$default_aes$size,
    angle = ggtext::GeomRichText$default_aes$angle,
    hjust = 1,
    vjust = 1,
    alpha = ggtext::GeomRichText$default_aes$alpha,
    family = ggtext::GeomRichText$default_aes$family,
    fontface = ggtext::GeomRichText$default_aes$fontface,
    lineheight = 1.2
  )
)

legend_pos_to_xy <- function(legend.position,
                             xrange,
                             yrange) {
  l <- legend.position

  if(is.numeric(l)) {
    stopifnot("Numeric `legend.position` must have length 2" = length(l) == 2)
    stopifnot("Numeric `legend.position` must have values between 0 & 1" =
                min(l) >= 0 || max(l) <= 1)
    l_num <- l
  }

  if (is.character(l)) {
    stopifnot(l %in% c("left",
                       "right",
                       "bottom",
                       "top",
                       "topright",
                       "bottomright",
                       "bottomleft",
                       "topleft"))
    l_num <- switch(
      l,
      "left" = c(0.025, 0.5),
      "right" = c(0.975, 0.5),
      "bottom" = c(0.5, 0.025),
      "top" = c(0.5, 0.975),
      "topright" = c(0.975, 0.975),
      "bottomright" = c(0.975, 0.025),
      "bottomleft" = c(0.025, 0.025),
      "topleft" = c(0.025, 0.975)
    )
  }
    l_x <- l_num[1] * (xrange[2] - xrange[1]) + xrange[1]
    l_y <- l_num[2] * (yrange[2] - yrange[1]) + yrange[1]

    return(c(l_x, l_y))
}

#' Slightly modified from gridtext by Claus O Wilke
#' @keywords internal
#' @noRd
rt_draw_panel <- function(data, panel_params, coord,
                          label.padding = unit(c(
                            0.25,
                            0.25, 0.25, 0.25
                          ), "lines"),
                          label.margin = unit(c(
                            0, 0,
                            0, 0
                          ), "lines"),
                          label.r = unit(0.15, "lines"),
                          na.rm = FALSE) {
  data <- coord$transform(data, panel_params)
  if (is.character(data$vjust)) {
    data$vjust <- compute_just(data$vjust, data$y)
  }
  if (is.character(data$hjust)) {
    data$hjust <- compute_just(data$hjust, data$x)
  }
  gridtext::richtext_grob(data$label, data$x, data$y,
    default.units = "native",
    hjust = data$hjust, vjust = data$vjust, rot = data$angle,
    padding = label.padding, margin = label.margin,
    gp = grid::gpar(
      col = scales::alpha(data$text.colour %||%
        data$colour, data$alpha), fontsize = data$size *
        .pt, fontfamily = data$family, fontface = data$fontface,
      lineheight = data$lineheight
    ),
    r = label.r
  )
}

#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}

# From gridtext by Claus O Wilke
#' @noRd
compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# From gridtext by Claus O Wilke
#' @noRd
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

