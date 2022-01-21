test_that("scale_x_date_*align() works", {

  lapply(X = list(scale_x_date_rightalign(),
         scale_x_date_leftalign(),
         scale_x_date_bothalign()),
         FUN = function(x) {
           expect_s3_class(x, "ScaleContinuousDate")
         })

  vdiffr::expect_doppelganger(title = "Right-aligned dates",
                              ggplot(ggplot2::economics,
                                     aes(x = date, y = unemploy)) +
                                geom_line() +
                                scale_x_date_rightalign())



  plots <- list(
    "right-aligned" = ggplot(ggplot2::economics,
                             aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_rightalign(),
    "left-aligned" = ggplot(ggplot2::economics,
                            aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_leftalign(),
    "both-aligned" = ggplot(ggplot2::economics,
                            aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_bothalign()
  )

  Map(function(x, i) vdiffr::expect_doppelganger(title = i, fig = x),
      plots, names(plots))


})

test_that("breaks_right() works with numeric vectors", {
  expect_identical(
    breaks_right(c(10, 30)),
    seq(10, 30, 5)
  )

  expect_length(breaks_right(c(10, 30),
                             n = 10),
                11)

  expect_length(breaks_right(c(10, 30),
                             n = 1),
                2)
})

test_that("breaks_right() works with date vectors", {
  econ_dates <- c(min(ggplot2::economics$date),
                  max(ggplot2::economics$date))

  expect_identical(breaks_right(econ_dates),
                   seq.Date(as.Date("1975-04-01"),
                            as.Date("2015-04-01"),
                            by = "10 years"))
})

test_that("breaks_right() works when supplied to scale_x_date()", {
  p <- ggplot2::ggplot(ggplot2::economics,
                       ggplot2::aes(x = date, y = unemploy)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(breaks = breaks_right)

  expect_s3_class(p, "gg")

})
