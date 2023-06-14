test_that("geom_richlegend produces expected output", {
  library(ggplot2)

  base_plot <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
    geom_point() +
    theme(legend.position = "none")

  # Default rich legend
  def_rich <- base_plot +
    geom_richlegend(aes(label = cyl))

  vdiffr::expect_doppelganger("default richlegend plot", def_rich)

  # Change rich legend position with string
  text_pos <- base_plot +
    geom_richlegend(aes(label = cyl),
                    legend.position = "top")

  vdiffr::expect_doppelganger("string richlegend position", text_pos)

  # Change the position using a numeric vector:
  num_pos <- base_plot +
    geom_richlegend(aes(label = cyl),
                    legend.position = c(0.1, 0.1))

  vdiffr::expect_doppelganger("numeric richlegend position", num_pos)

  # faceted richlegend
  faceted <- base_plot +
    geom_richlegend(aes(label = cyl)) +
    facet_wrap(~cyl)

  vdiffr::expect_doppelganger("faceted richlegend plot", faceted)

})
