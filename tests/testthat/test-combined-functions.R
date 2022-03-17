test_that("core functions work together", {

  p <- ggplot(ggplot2::economics_long,
              aes(x = date, y = value, col = variable)) +
    geom_linepoint() +
    geom_finallabel(aes(label = value)) +
    scale_x_date_rightalign()

  expect_s3_class(p, "gg")
  vdiffr::expect_doppelganger("all functions together", p)
})
