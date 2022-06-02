library(ggplot2)

test_that("geom_linepoint() produces expected output", {

  plot <-  ggplot(ggplot2::economics_long, aes(x = date, y = value)) +
      geom_linepoint(aes(col = variable)) +
      facet_wrap(~variable, scales = "free_y")

  expect_s3_class(plot, "gg")
  vdiffr::expect_doppelganger("Faceted plot", plot)

  vdiffr::expect_doppelganger("Numeric axes",
    ggplot(iris,
           aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
    geom_linepoint())

  vdiffr::expect_doppelganger("No colour",
    ggplot(ggplot2::economics_long,
           aes(x = date, y = value)) +
    geom_linepoint() +
    facet_wrap(~variable) )

})


test_that("non-ordered data displays correctly with geom_linepoint()", {
  plot <- economics %>%
    dplyr::arrange(unemploy) %>%
    ggplot(aes(x = date, y = unemploy)) +
    geom_linepoint()

  vdiffr::expect_doppelganger("Unordered data", plot)
})
