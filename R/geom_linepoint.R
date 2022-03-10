#' Line with a dot on the final observation
#' @description `geom_linepoint()` draws a `ggplot2::geom_line()` and adds a
#' `ggplot2::geom_point()` at the observation with the maximum x value for each
#' group.
#' @inheritParams ggplot2::geom_line
#' @section Aesthetics:
#' \code{geom_linepoint()} understands the following aesthetics (required aesthetics are in bold):
#'   \itemize{
#'     \item \strong{\code{x}}
#'     \item \strong{\code{y}}
#'     \item \code{alpha}
#'     \item \code{colour}
#'     \item \code{group}
#'     \item \code{linetype}
#'     \item \code{pointfill}
#'     \item \code{pointshape}
#'     \item \code{pointsize}
#'     \item \code{pointstroke}
#'     \item \code{pointshape}
#'     \item \code{size}
#'     \item \code{weight}
#'   }
#' The aesthetics that begin with 'point' (eg. `pointfill`) are passed to
#' `geom_point()` - for example `pointfill` is passed to the `fill` aesthetic
#' of `geom_point()`.
#'
#' The `x`, `y`, `alpha`, `colour`, and `group` aesthetics are passed to both
#' `geom_line()` and `geom_point()`.
#'
#' The `linetype`, `size`, and `weight` aesthetics are passed to `geom_line()`.
#'
#' Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
#' @seealso `ggplot2::geom_line`, `ggplot2::geom_point`
#' @export
#' @examples
#' library(ggplot2)
#'
#'ggplot(ggplot2::economics_long, aes(x = date, y = value)) +
#'   geom_linepoint(aes(col = variable)) +
#'   facet_wrap(~variable)
geom_linepoint <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomLinePoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
  }

GeomLinePoint <- ggplot2::ggproto(
  "GeomLinePoint",
  ggplot2::Geom,
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    ggplot2::GeomPoint$setup_data(data, params)
  },
  draw_group = function(data,
                        panel_params,
                        coord,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 10,
                        size = 5,
                        flipped_aes = FALSE) {
    point <- data
    point <- point[point$x == max(point$x), ]
    point$size <- point$pointsize
    point$fill <- point$pointfill
    point$shape <- point$pointshape
    point$stroke <- point$pointstroke
    point$shape <- point$pointshape

    path <- transform(data, alpha = NA)

    grid::gList(
      ggplot2::GeomLine$draw_panel(
        path,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      ),
      ggplot2::GeomPoint$draw_panel(point, panel_params, coord)
    )
  },
  draw_key = ggplot2::draw_key_smooth,
  required_aes = c("x", "y"),
  non_missing_aes = c(
    "size", "shape", "colour", "pointsize",
    "pointstroke", "pointfill", "pointshape"
  ),
  default_aes = ggplot2::aes(
    pointsize = 2.5, pointfill = "white", pointshape = 21,
    shape = 19, colour = "black", size = 1,
    alpha = 1, pointstroke = 1.5, linetype = 1, weight = 1
  )
)
