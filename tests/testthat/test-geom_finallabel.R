test_that("geom_finallabel() works", {
  x <- geom_finallabel()
  expect_s3_class(x, "gg")

  vdiffr::expect_doppelganger("Final label",
                              ggplot(ggplot2::economics, aes(date, uempmed)) +
                                geom_line() +
                                geom_finallabel(aes(label = uempmed)))

  vdiffr::expect_doppelganger("nudge_x_perc",
                              ggplot(ggplot2::economics, aes(date, uempmed)) +
                                geom_line() +
                                geom_finallabel(aes(label = uempmed),
                                                nudge_x_perc = 5) +
                                scale_x_date(expand = expansion(mult = c(0, 0.2))))
})


