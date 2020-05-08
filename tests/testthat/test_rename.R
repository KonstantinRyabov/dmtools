id <- c("01", "02", "03")
age <- c("19", "20", "22")
sex <- c("f", "m", "f")
screen_date_post <- c("1991-03-13", "1991-03-07", "1991-03-08")
rand_date_post <- c("1991-03-15", "1991-03-11", "1991-03-10")
ph_date_post <- c("1991-03-21", "1991-03-16", "1991-03-16")
bio_date_post <- c("1991-03-23", "1991-03-16", "1991-03-16")
gluc_post <- c("5.5", "4.1", "9.7")
gluc_res_post <- c("norm", "no", "norm")

df <- data.frame(
  id, age, sex,
  screen_date_post, rand_date_post,
  ph_date_post, bio_date_post,
  gluc_post, gluc_res_post,
  stringsAsFactors = F
)

test_that("check names of renamed dataset", {
  rename_ds <- rename_dataset(df, "forms", "old_name", "new_name", 1)
  renamed_names <- c(
    "id", "age", "sex",
    "screening date_post", "randomization date_post", "physical examination date_post",
    "blood biochemistry date_post", "glucose test_post", "result is normal_post"
  )
  expect_named(rename_ds$data, renamed_names)
})
