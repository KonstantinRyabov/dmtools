#' Create object date
#'
#' @param file A character scalar. Path to the date's parameters in the excel table.
#' @param id  A column name of the subject id in the dataset, without quotes.
#' @param get_visit A function, which select necessary visit or event e.g. dplyr::start_with, dplyr::contains.
#' @param get_date A function, which select dates from necessary visit e.g. dplyr::matches, dplyr::contains, default: dplyr::contains.
#' @param str_date A date's pattern in column names, default: "DAT".
#'
#' @return The object date.
#' @export
#'
#' @examples
#' obj_date <- date("dates.xlsx", id, dplyr::contains)
#' obj_date <- date("dates.xlsx", id, dplyr::contains, "uneq")
date <- function(file, id, get_visit, get_date = dplyr::contains, str_date = "DAT") {
  obj <- list(
    file = file,
    id = dplyr::enquo(id),
    get_visit = get_visit,
    get_date = get_date,
    str_date = str_date
  )

  class(obj) <- "date"
  obj
}

#' Filter final result
#'
#' @param obj An object for calculation. Class date.
#' @param group_id A logical scalar, default is TRUE.True is grouped by id, otherwise, it isn't grouped.
#' @param test A character scalar. Parameters, which use to filter the final dataset, default: "out":
#'                                                   "out" - dates, which are out of the protocol's timeline,
#'                                                   "uneq" - dates, which are unequal,
#'                                                   "ok" - correct dates,
#'                                                   "skip" - empty dates.
#'
#'
#' @return The dataset by a value of \code{test}.
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
choose_test.date <- function(obj, test = "out", group_id = T) {
  result <- obj %>% get_result(group_id)

  # filter final dataset
  if (test == "out") {
    result <-
      result %>%
      dplyr::filter(.data$IS_TIMELINE == F) %>%
      dplyr::select(-.data$IS_TIMELINE, -.data$IS_EQUAL, -.data$EQUALDAT)
  } else if (test == "uneq") {
    result <-
      result %>%
      dplyr::filter(.data$IS_EQUAL == F) %>%
      dplyr::select(
        -.data$IS_EQUAL,
        -.data$PLANDAT,
        -.data$STARTDAT,
        -.data$STARTVISIT,
        -.data$DAYS_OUT
      )
  } else if (test == "ok") {
    result <-
      result %>%
      dplyr::filter(.data$IS_EQUAL == T & .data$IS_TIMELINE == T) %>%
      dplyr::select(-.data$IS_TIMELINE, -.data$IS_EQUAL, -.data$DAYS_OUT)
  } else if (test == "skip") {
    result <-
      result %>%
      dplyr::filter(is.na(.data$VISDAT)) %>%
      dplyr::select(-.data$DAYS_OUT, -.data$PLANDAT, -.data$IS_TIMELINE, -.data$IS_EQUAL, -.data$EQUALDAT)
  } else {
    stop("uknown parameter ", test)
  }

  result
}

#' Find column names with dates
#'
#' @param dataset A data frame. Class date.
#' @param obj An object for validation.
#' @param row_file A data frame. A data frame with analysis parameters.
#'
#' @return A data frame. Visit's dates.
#'
find_colnames.date <- function(obj, dataset, row_file) {
  str_visit <- row_file$VISITNUM
  string_date <- obj[["str_date"]]
  get_visit <- obj[["get_visit"]]
  get_date <- obj[["get_date"]]

  # select visit
  visit <- dataset %>% dplyr::select(get_visit(str_visit))

  # select dates from visit
  dates <- visit %>%
    dplyr::select(get_date(string_date)) %>%
    colnames()

  do.call(rbind, lapply(
    dates,
    function(date) {
      run_tests(obj, dataset, row_file, date)
    }
  ))
}

#' Run test
#'
#' @param dataset A data frame. Class date.
#' @param obj An object for validation.
#' @param row_file A data frame. A data frame with analysis parameters.
#' @param date A column name with dates.
#'
#' @return A data frame. Result of the date's validation.
#'
run_tests.date <- function(obj, dataset, row_file, date) {
  id <- obj[["id"]]

  # params of visit
  minus <- as.integer(row_file$MINUS)
  plus <- as.integer(row_file$PLUS)
  shift <- as.integer(row_file$VISITDY)
  name_visit <- row_file$VISIT
  st_date <- row_file$STARTDAT
  st_name <- row_file$STARTVISIT

  # params for check if dates are equal
  check_equal <- as.logical(row_file$IS_EQUAL)
  equal_date <- ifelse(check_equal, row_file$EQUALDAT, date)

  # check dates
  dataset %>%
    dplyr::mutate(
      STARTVISIT = st_name,
      STARTDAT = lubridate::as_date(.data[[st_date]]),
      VISIT = name_visit,
      TERM = date,
      VISDAT = lubridate::as_date(.data[[date]]),
      EQUALDAT = lubridate::as_date(.data[[equal_date]])
    ) %>%
    dplyr::mutate(PLANDAT = lubridate::interval(
      .data$STARTDAT - lubridate::days(minus),
      .data$STARTDAT + lubridate::days(plus)
    ) %>% lubridate::int_shift(lubridate::duration(days = shift))) %>%
    dplyr::mutate(IS_TIMELINE = .data$VISDAT %within% .data$PLANDAT) %>%
    dplyr::mutate(IS_EQUAL = .data$VISDAT == .data$EQUALDAT) %>%
    dplyr::mutate(DAYS_OUT = ifelse(.data$IS_TIMELINE, 0, calc_diff(.data$PLANDAT, .data$VISDAT))) %>%
    dplyr::select(
      !!id,
      .data$STARTVISIT,
      .data$STARTDAT,
      .data$VISIT,
      .data$TERM,
      .data$VISDAT,
      .data$PLANDAT,
      .data$EQUALDAT,
      .data$IS_TIMELINE,
      .data$IS_EQUAL,
      .data$DAYS_OUT
    )
}

#' Function for calculating the difference between two dates
#'
#' @param st_inter An interval. An object of interval.
#' @param dt_item A date item. An object of date.
#'
#' @return An integer scalar. Differences between the two dates.
#'
calc_diff <- function(st_inter, dt_item) {
  start_item <- dt_item - lubridate::as_date(lubridate::int_start(st_inter))
  end_item <- dt_item - lubridate::as_date(lubridate::int_end(st_inter))
  pmin(abs(start_item), abs(end_item))
}
