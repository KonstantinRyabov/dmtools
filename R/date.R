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
#'
date <- function(file, id, get_visit, get_date = dplyr::contains, str_date = "DAT") {
  id <- dplyr::enquo(id)

  obj <- list(
    file = file,
    id = id,
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
#' stringsAsFactors = FALSE)
#'
#' timeline <- system.file("dates.xlsx", package = "dmtools")
#' obj_date <- date(timeline, id, dplyr::contains)
#'
#' obj_date <- check(obj_date, df)
#' choose_test(obj_date, "out")
#'
choose_test.date <- function(obj, test = "out", group_id = T) {

  result <- obj %>% get_result(group_id)

  # filter final dataset
  if (test == "out") {
    result <-
      result %>%
      dplyr::filter(.data$is_in_timeline == F) %>%
      dplyr::select(-.data$is_in_timeline, -.data$is_equal, -.data$stand_equal)
  } else if (test == "uneq") {
    result <-
      result %>%
      dplyr::filter(.data$is_equal == F) %>%
      dplyr::select(
        -.data$is_equal,
        -.data$standard_interval,
        -.data$standard_date,
        -.data$standard_name,
        -.data$out
      )
  } else if (test == "ok") {
    result <-
      result %>%
      dplyr::filter(.data$is_equal == T & .data$is_in_timeline == T) %>%
      dplyr::select(-.data$is_in_timeline, -.data$is_equal, -.data$out)
  } else if (test == "skip") {
    result <-
      result %>%
      dplyr::filter(is.na(.data$date_item)) %>%
      dplyr::select(-.data$out, -.data$standard_interval, -.data$is_in_timeline, -.data$is_equal, -.data$stand_equal)
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
  str_visit <- row_file$num_visit
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
  minus <- as.integer(row_file$minus)
  plus <- as.integer(row_file$plus)
  shift <- as.integer(row_file$shift)
  name_visit <- row_file$name_visit
  st_date <- row_file$standard_date
  st_name <- row_file$standard_name

  # params for check if dates are equal
  check_equal <- as.logical(row_file$check_equal)
  equal_date <- ifelse(check_equal, row_file$equal_date, date)

  # check dates
  dataset %>%
    dplyr::mutate(
      standard_name = st_name,
      standard_date = lubridate::as_date(.data[[st_date]]),
      name_event = name_visit,
      name_item = date,
      date_item = lubridate::as_date(.data[[date]]),
      stand_equal = lubridate::as_date(.data[[equal_date]])
    ) %>%
    dplyr::mutate(standard_interval = lubridate::interval(
      .data$standard_date - lubridate::days(minus),
      .data$standard_date + lubridate::days(plus)
    ) %>% lubridate::int_shift(lubridate::duration(days = shift))) %>%
    dplyr::mutate(is_in_timeline = .data$date_item %within% .data$standard_interval) %>%
    dplyr::mutate(is_equal = .data$date_item == .data$stand_equal) %>%
    dplyr::mutate(out = ifelse(.data$is_in_timeline, 0, calc_diff(.data$standard_interval, .data$date_item))) %>%
    dplyr::select(
      !!id,
      .data$standard_name,
      .data$standard_date,
      .data$name_event,
      .data$name_item,
      .data$date_item,
      .data$standard_interval,
      .data$stand_equal,
      .data$is_in_timeline,
      .data$is_equal,
      .data$out
    )
}

#' Function for calculating the difference between two dates
#'
#' @param st_inter An interval. An Object of interval.
#' @param dt_item A date item. An Object of date.
#'
#' @return An integer scalar. Differences between the two dates.
#'
calc_diff <- function(st_inter, dt_item) {
  start_item <- dt_item - lubridate::as_date(lubridate::int_start(st_inter))
  end_item <- dt_item - lubridate::as_date(lubridate::int_end(st_inter))
  minimum <- pmin(abs(start_item), abs(end_item))
  ifelse(minimum == abs(start_item), paste0(minimum, "<-"), paste0("->", minimum))
}
