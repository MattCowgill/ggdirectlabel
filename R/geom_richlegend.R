geom_richlegend <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = FALSE,
           inherit.aes = TRUE,
           ...) {

    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomRichLegend,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,

        ...
      )
    )
  }

#' @export
#' @rdname geom_finallabel
GeomRichLegend <- ggplot2::ggproto(
  "GeomRichLegend",
  ggplot2::Geom,
  extra_params = c("richlegend_x", "richlegend_y", "na.rm"),
  setup_data = function(data, params) {
    # print(data)
    x <- ggtext::GeomRichText$setup_data(data, params) |>
      dplyr::group_by(label, group, PANEL, colour) |>
      dplyr::summarise() |>
      dplyr::mutate(x = params$richlegend_x,
                    y = params$richlegend_y) |>
      dplyr::ungroup()

    # print(x)
    x
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        flipped_aes = FALSE) {

    richtext_data <- data |>
      dplyr::mutate(
        label = paste0("<span style='color:",
                           colour,
                           "'>",
                           label,
                           "</span>")
      ) |>
      dplyr::group_by(PANEL, x, y, size, angle, hjust, vjust,
                      alpha, lineheight, family, fontface) |>
      dplyr::summarise(label = paste0(label, collapse = "<br>")) |>
      dplyr::ungroup() |>
      as.data.frame() |>
      dplyr::mutate(colour = "#000000")

    print(richtext_data)

    panel_params_env <<- panel_params
    data_env <<- richtext_data

    # ggtext::GeomRichText$draw_panel(
    #   data,
    #   panel_params,
    #   coord
    # )
    rt_draw_panel(
      richtext_data,
      panel_params,
      coord
    )

    # ggplot2::GeomText$draw_panel(
    #   richtext_data,
    #   panel_params,
    #   coord
    # )
  },
  draw_key = ggplot2::draw_key_text,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = ggplot2::GeomText$default_aes$colour,
    size = ggplot2::GeomText$default_aes$size,
    angle = ggplot2::GeomText$default_aes$angle,
    hjust = 0,
    vjust = ggplot2::GeomText$default_aes$vjust,
    alpha = ggplot2::GeomText$default_aes$alpha,
    family = ggplot2::GeomText$default_aes$family,
    fontface = ggplot2::GeomText$default_aes$fontface,
    lineheight = 0.9
  )
)

#' Slightly modified from gridtext by Claus O Wilke
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

`%||%` <- function (a, b)  {
  if (!is.null(a)) {
    a
  } else {
    b
  }
}
