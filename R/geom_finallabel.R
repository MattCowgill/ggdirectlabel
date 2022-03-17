#' Line with a dot on the final observation
#' @description `geom_finallabel()` draws a `ggplot2::geom_text()`
#' at the observation with the maximum x value for each group.
#' @inheritParams ggplot2::geom_text
#' @section Aesthetics:
#'   \code{geom_text()} understands the following aesthetics (required aesthetics are in bold):
#'   \itemize{
#'     \item \strong{\code{x}}
#'     \item \strong{\code{y}}
#'     \item \strong{\code{label}}
#'     \item \code{alpha}
#'     \item \code{angle}
#'     \item \code{colour}
#'     \item \code{family}
#'     \item \code{fontface}
#'     \item \code{group}
#'     \item \code{hjust}
#'     \item \code{lineheight}
#'     \item \code{size}
#'     \item \code{vjust}
#'   }
#'
#' Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
#' @seealso `ggplot2::geom_text`
#' @export
#' @examples
#' library(ggplot2)
#'
#'ggplot(ggplot2::economics_long, aes(x = date, y = value)) +
#'   geom_linepoint(aes(col = variable)) +
#'   geom_finallabel(aes(label = value)) +
#'   facet_wrap(~variable)
geom_finallabel <-
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
      geom = GeomFinalLabel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
  }

GeomFinalLabel <- ggplot2::ggproto(
  "GeomFinalLabel",
  ggplot2::Geom,
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    ggplot2::GeomText$setup_data(data, params)
  },
  draw_group = function(data,
                        panel_params,
                        coord,
                        flipped_aes = FALSE) {
    data <- data[data$x == max(data$x), ]

    ggplot2::GeomText$draw_panel(
        data,
        panel_params,
        coord
      )
  },
  draw_key = ggplot2::draw_key_text,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = ggplot2::GeomText$default_aes$colour,
    size = ggplot2::GeomText$default_aes$size,
    angle = ggplot2::GeomText$default_aes$angle,
    hjust = -0.12,
    vjust = ggplot2::GeomText$default_aes$vjust,
    alpha = ggplot2::GeomText$default_aes$alpha,
    family = ggplot2::GeomText$default_aes$family,
    fontface = ggplot2::GeomText$default_aes$fontface,
    lineheight = 0.9
  )
)
