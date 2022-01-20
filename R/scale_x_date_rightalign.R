#' Add a ggplot2 date scale with breaks aligned to the end of your data
#'
#' When visualising time series, it's often important to be clear about
#' the date of the latest observation. This scale aligns your axis labels
#' to the most recent observation in your data.
#'
#' @param expand The amount of padding/expansion that should be added at the
#' left and right of the data. Use the `ggplot2::expansion()` function to
#' add padding. By default, 5% padding is added to both right and left.
#' @param num_breaks The (approximate) number of breaks to include on the
#' axis. `base::pretty()` is used to generate these breaks.
#' @param date_labels The format for the date labels on the axis. The default
#' means "day of month, then shortened month, then linebreak, then
#' full year". See `?strptime` for more codes that can be used here.
#' @param ... Arguments passed to `ggplot2::scale_x_date()`.
#' @export
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(ggplot2::economics, aes(date, unemploy)) +
#'     geom_line() +
#'     scale_x_date_rightalign()
#'

scale_x_date_rightalign <- function(expand = expansion(mult = c(0.05, 0.05)),
                                    num_breaks = 5,
                                    date_labels = "%e %b\n%Y",
                                    ...) {
  scale_x_date(
    expand = expand,
    breaks = function(limits,
                      exp = expand,
                      break_num = num_breaks) {
      exp_mult <- exp[c(1, 3)]
      exp_add <- exp[c(2, 4)]

      range_perc_data <- 1 + exp_mult[1] + exp_mult[2]
      data_range <- as.numeric((1 / range_perc_data) *
                                 (limits[2] - limits[1] - exp_add[1] - exp_add[2]))

      data_max <-
        limits[2] - (data_range * (exp_mult[2]))  - exp_add[2]

      data_min <- data_max - data_range

      breaks <- breaks_right(limits = c(data_min, data_max),
                             n_breaks = break_num)

      breaks
    },
    date_labels = date_labels,
    ...
  )
}


#' Generate date breaks that align with the maximum data value
#'
#' This function generates a vector of of 'pretty'
#' breaks (using `scales::breaks_pretty()`) that ends with
#' the upper limit provided and excludes any values that lie
#' outside the limits.
#'
#' @param limits Length-two numeric or date vector
#' @param n_breaks Number of breaks; passed to `scales::breaks_pretty()`
#' @param ... Passed to `scales::breaks_pretty()`
#' @export
#' @seealso scale_x_date_rightalign()
#' @return Vector of breaks, of the same class as `limits`
#' @author Originally written by Matt Cowgill for the `djprtheme` package,
#' in which the Department of Jobs, Precincts and Regions (Victoria) is
#' the copyright holder.
#' @examples
#'# Can be used with numeric vectors
#'breaks_right(c(10, 30))
#'
#'# Or date vectors
#'econ_dates <- range(ggplot2::economics$date)
#'breaks_right(econ_dates)
#'
#'# Can be supplied directly to the `breaks` argument of
#'# `ggplot2::scale_*_continuous()`. Note that the breaks will
#'# be aligned to the right-limit of the plot
#'# (including expansion/padding area).
#'
#'library(ggplot2)
#'
#'ggplot(ggplot2::economics,
#'       aes(x = date, y = unemploy)) +
#'  geom_line() +
#'  scale_x_date(breaks = breaks_right)
#'
#' # To right-align to the data, not including padding, try:
#' ggplot(ggplot2::economics,
#'     aes(x = date, y = unemploy)) +
#'     geom_line() +
#'     scale_x_date(breaks = breaks_right(limits = range(
#'     ggplot2::economics$date)))
#'
#' # Or use `scale_x_date_rightalign()`:
#' ggplot(ggplot2::economics,
#'     aes(x = date, y = unemploy)) +
#'     geom_line() +
#'     scale_x_date_rightalign()
#'
breaks_right <- function(limits,
                         n_breaks = 5,
                         ...) {
  min_date <- limits[1]
  max_date <- limits[2]
  pre_br <- scales::breaks_pretty(n = n_breaks,
                                  ...)(c(min_date, max_date))
  date_adj <- pre_br[length(pre_br)] - max_date
  adj_br <- pre_br - date_adj
  names(adj_br) <- NULL
  out_br <- adj_br[adj_br >= min_date & adj_br <= max_date]
  out_br
}
