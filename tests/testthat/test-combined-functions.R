test_that("core functions work together", {

  p <- ggplot(ggplot2::economics_long,
              aes(x = date, y = value, col = variable)) +
    geom_linepoint() +
    geom_finallabel(aes(label = value)) +
    geom_richlegend(aes(label = variable),
                    legend.position = "topleft",
                    hjust = 0) +
    scale_x_date_rightalign() +
    theme(legend.position = "none")

  expect_s3_class(p, "gg")
  vdiffr::expect_doppelganger("all functions together", p)
})
