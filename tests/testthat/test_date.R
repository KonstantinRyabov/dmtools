id <- c("01", "02", "03")
screen_date_E1 <- c("1991-03-13", "1991-03-07", "1991-03-08")
rand_date_E2 <- c("1991-03-15", "1991-03-11", "1991-03-10")
ph_date_E3 <- c("1991-03-21", "1991-03-16", "1991-03-16")
bio_date_E3 <- c("1991-03-23", "1991-03-16", "1991-03-16")

df <- data.frame(
  id, screen_date_E1, rand_date_E2, ph_date_E3, bio_date_E3,
  stringsAsFactors = F
)

obj_date <- date("dates.xlsx", id, dplyr::contains)
obj_date <- check(obj_date, df)
out_date <- choose_test(obj_date, "out")

test_that("check the number of dates, which is out the timeline", {
  expect_equal(nrow(out_date), 2)
})

test_that("check names of dates, which is out the timeline", {
  expect_equal(out_date$name_item[1], "bio_date_E3")
  expect_equal(out_date$name_item[2], "screen_date_E1")
})

test_that("check the difference of protocol", {
  expect_equal(out_date$out[1], "->2")
  expect_equal(out_date$out[2], "1<-")
})

test_that("check the number of dates, which is no equal", {
  uneq <- choose_test(obj_date, "uneq")
  expect_equal(nrow(uneq), 1)
})

test_that("check the number of dates, which is correct", {
  correct <- choose_test(obj_date, "ok")
  expect_equal(nrow(correct), 13)
})

test_that("check the number of dates, which is skipped", {
  skipped <- choose_test(obj_date, "skip")
  expect_equal(nrow(skipped), 0)
})

test_that("check unknown parameter", {
  expect_error(choose_test(obj_date, "okk"), "uknown parameter okk")
})
