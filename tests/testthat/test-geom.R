test_that("geom_linepoint() produces expected output", {
  library(ggplot2)

  plot <-  ggplot2::economics_long %>%
      ggplot(aes(x = date, y = value)) +
      geom_linepoint(aes(col = variable)) +
      facet_wrap(~variable)

  expect_s3_class(plot, "gg")
  vdiffr::expect_doppelganger("Faceted plot", plot)

  vdiffr::expect_doppelganger("Numeric axes",
                              iris %>%
    ggplot(aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
    geom_linepoint())

  vdiffr::expect_doppelganger("No colour",
  ggplot2::economics_long %>%
    ggplot(aes(x = date, y = value)) +
    geom_linepoint() +
    facet_wrap(~variable) )

})
