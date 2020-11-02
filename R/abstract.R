#' Check the dataset
#'
#' @param obj An object for check.
#' @param dataset A dataset, a type is a data frame.
#'
#' @return An object with a check result.
#' @export
#'
#' @examples
#' id <- c("01", "02", "03")
#' screen_date_E1 <- c("1991-03-13", "1991-03-07", "1991-03-08")
#' rand_date_E2 <- c("1991-03-15", "1991-03-11", "1991-03-10")
#' ph_date_E3 <- c("1991-03-21", "1991-03-16", "1991-03-16")
#' bio_date_E3 <- c("1991-03-23", "1991-03-16", "1991-03-16")
#'
#' df <- data.frame(id, screen_date_E1, rand_date_E2, ph_date_E3, bio_date_E3,
#'   stringsAsFactors = FALSE
#' )
#'
#' timeline <- system.file("dates.xlsx", package = "dmtools")
#' obj_date <- date(timeline, id, dplyr::contains)
#'
#' obj_date <- check(obj_date, df)
check <- function(obj, dataset) {
  UseMethod("check")
}

#' Filter the final result
#'
#' @param obj An object for check.
#' @param group_id A logical scalar, default is TRUE.True is grouped by id, otherwise, it isn't grouped.
#' @param test Parameters, which use to filter the final dataset.
#'
#' @return The filtered dataset.
#' @export
#'
#' @examples
#' id <- c("01", "02", "03")
#' screen_date_E1 <- c("1991-03-13", "1991-03-07", "1991-03-08")
#' rand_date_E2 <- c("1991-03-15", "1991-03-11", "1991-03-10")
#' ph_date_E3 <- c("1991-03-21", "1991-03-16", "1991-03-16")
#' bio_date_E3 <- c("1991-03-23", "1991-03-16", "1991-03-16")
#'
#' df <- data.frame(id, screen_date_E1, rand_date_E2, ph_date_E3, bio_date_E3,
#'   stringsAsFactors = FALSE
#' )
#'
#' timeline <- system.file("dates.xlsx", package = "dmtools")
#' obj_date <- date(timeline, id, dplyr::contains)
#'
#' obj_date <- check(obj_date, df)
#' choose_test(obj_date, "out")
choose_test <- function(obj, test, group_id) {
  UseMethod("choose_test")
}

#' Find column names
#'
#' @param obj An object for check.
#' @param dataset A dataset, a type is a data frame.
#' @param row_file A row of the file.
#'
#' @return A data frame. Result of run_tests.
#'
find_colnames <- function(obj, dataset, row_file) {
  UseMethod("find_colnames")
}

#' Reshape the dataset to a long view
#'
#' @param obj An object for check.
#' @param dataset A data frame.
#' @param row_file A data frame. A data frame with parameters.
#' @param part A character scalar. Prefixes or postfixes.
#'
#' @return A data frame. The part of final result.
#'
to_long <- function(obj, dataset, row_file, part) {
  UseMethod("to_long")
}
