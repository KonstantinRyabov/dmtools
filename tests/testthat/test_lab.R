id <- c("01", "02", "03")
site <- c("site 01", "site 02", "site 03")
age <- c("19", "20", "22")
sex <- c("f", "m", "f")
gluc_post <- c(5.5, 4.1, 9.7)
gluc_res_post <- c("norm", "no", "cl")
ast_post <- c("30", "48", "31")
ast_res_post <- c(NA, "norm", "norm")

df <- data.frame(
  id, site, age, sex,
  gluc_post, gluc_res_post,
  ast_post, ast_res_post,
  stringsAsFactors = F
)

s01_lab <- lab("labs_refer.xlsx", id, age, sex, "norm", "no", clsig = "cl", site = "site 01")
s02_lab <- lab("labs_refer.xlsx", id, age, sex, "norm", "no", clsig = "cl", site = "site 02")
s03_lab <- lab("labs_refer.xlsx", id, age, sex, "norm", "no", clsig = "cl", site = "site 03")
all_lab <- check(s01_lab, df)

labs <- list(s01_lab, s02_lab, s03_lab)
labs <- check_sites(labs, df, site)

test_that("check the number of incorrect analyzes in different sites", {
  site_incorrect <- test_sites(labs, func = function(x) choose_test(x, "mis"))
  expect_equal(nrow(site_incorrect), 2)
})

test_that("check the number of skipped analyzes in different sites", {
  site_skip <- test_sites(labs, func = function(x) choose_test(x, "skip"))
  expect_equal(nrow(site_skip), 1)
})

test_that("check the number of incorrect analyzes", {
  incorrect <- choose_test(all_lab, "mis")
  expect_equal(nrow(incorrect), 2)
})

test_that("check the number of correct analyzes", {
  correct <- choose_test(all_lab, "ok")
  expect_equal(nrow(correct), 3)
})

test_that("check the number of skipped analyzes", {
  skipped <- choose_test(all_lab, "skip")
  expect_equal(nrow(skipped), 1)
})

test_that("check the number of null analyzes", {
  null_lab <- choose_test(all_lab, "null")
  expect_equal(nrow(null_lab), 0)
})

test_that("check unknown parameter", {
  expect_error(choose_test(all_lab, "nul"), "uknown parameter nul")
})

test_that("check if analyzes not found", {
  warn_lab <- lab("warning_refer.xlsx", id, age, sex, "norm", "no")
  expect_warning(check(warn_lab, df), "enzyme alt not found")
})

test_that("check if the final result is empty", {
  error_lab <- lab("error_refer.xlsx", id, age, sex, "norm", "no")
  expect_error(check(error_lab, df), "the final result of validation is empty")
})

test_that("check if age_min > age_max", {
  warn_age <- lab("warning_age.xlsx", id, age, sex, "norm", "no")
  expect_warning(check(warn_age, df), "age_low > age_high in enzyme ast")
})

test_that("check if lab_min > lab_max ", {
  warn_lab <- lab("warning_lab.xlsx", id, age, sex, "norm", "no")
  expect_warning(check(warn_lab, df), "lbornrlo > lbornrhi in enzyme ast")
})
