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

test_that("date_labels behaviour in scale_date_*align() works", {
  plots <- list(
    "year-dates" = ggplot(ggplot2::economics,
                             aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_rightalign(),
    "month-dates" = ggplot(subset(ggplot2::economics, date >= as.Date("2010-01-01")),
                            aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_rightalign(),
    "day-dates" = ggplot(subset(ggplot2::economics, date >= as.Date("2015-01-01")),
                         aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_rightalign(),
    "manual-dates" = ggplot(subset(ggplot2::economics, date >= as.Date("2010-01-01")),
                            aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_rightalign(date_labels = "%Y"),
    "manual-dates-2" = ggplot(subset(ggplot2::economics, date >= as.Date("2010-01-01")),
                              aes(x = date, y = unemploy)) +
      geom_line() +
      scale_x_date_rightalign(date_labels = "%b-%Y")
  )

  Map(function(x, i) vdiffr::expect_doppelganger(title = i, fig = x),
      plots, names(plots))

})

test_that("labels_date_auto() works as expected", {
  my_dates <- ggplot2::economics$date

  cut_dates <- function(x, n) {
    as.Date(
      labels(
        split(x, cut(x, n))
      )
    )
  }

  expect_equal(labels_date_auto(cut_dates(my_dates, 5)),
               "%Y")

  expect_equal(labels_date_auto(cut_dates(my_dates, 100)),
               "%b\n%Y")

  expect_equal(labels_date_auto(cut_dates(my_dates, 500)),
               "%b\n%Y")

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
