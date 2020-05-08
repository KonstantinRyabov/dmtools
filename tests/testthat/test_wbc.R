id <- c("01", "02", "03")
all_wbc_post <- c(5.6, 7.8, 8.1)
rel_lym_post <- c(21, 25, 30)
abs_lym_post <- c(1.18, 1.95, 2.13)

df <- data.frame(
  id, all_wbc_post, rel_lym_post, abs_lym_post,
  stringsAsFactors = F
)

wbcc <- wbc("wbcc.xlsx", id)
wbcc <- check(wbcc, df)

test_that("check the number of incorrect calculation", {
  incorrect <- choose_test(wbcc, "mis")
  expect_equal(nrow(incorrect), 1)
})

test_that("check the number of correct calculation", {
  correct <- choose_test(wbcc, "ok")
  expect_equal(nrow(correct), 2)
})

test_that("check unknown parameter", {
  expect_error(choose_test(wbcc, "wbc"), "uknown parameter wbc")
})
