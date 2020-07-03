#' Create object wbc
#'
#' @param file A character scalar. Path to the laboratory's reference in the excel table.
#' @param id A column name of the subject id in the dataset, without quotes.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#' @param site A site number, default: NA.
#' @param name_to_find A character scalar. For search prefixes or postfixes, default is "relative".
#'
#' @return The object wbc.
#' @export
#'
wbc <- function(file, id, is_post = T, site = NA, name_to_find = "relative") {
  obj <- list(
    file = file,
    id = dplyr::enquo(id),
    is_post = is_post,
    site = site,
    name_to_find = name_to_find,
    bond = "_"
  )

  class(obj) <- "wbc"
  obj
}

#' Filter final result
#'
#' @param obj An object. Class wbc.
#' @param group_id A logical scalar, default is TRUE.True is grouped by id, otherwise, it isn't grouped.
#' @param test A character scalar. Parameters, which use to filter the final dataset, default: "mis":
#'                                                   "ok" - wbc, which is calculated correct,
#'                                                   "mis" - wbc, which is calculated incorrect.
#'
#'
#' @return The dataset by a value of \code{test}.
#' @export
#'
#' @examples
#' id <- c("01", "02", "03")
#' wbc_post <- c(5.6, 7.8, 8.1)
#' lym_rel_post <- c(21, 25, 30)
#' lym_abs_post <- c(1.18, 1.95, 2.13)
#'
#' df <- data.frame(
#'   id, wbc_post, lym_rel_post, lym_abs_post,
#'   stringsAsFactors = FALSE
#' )
#'
#' wbcc <- system.file("wbcc.xlsx", package = "dmtools")
#' obj_wbc <- wbc(wbcc, id)
#'
#' obj_wbc <- check(obj_wbc, df)
#' choose_test(obj_wbc, "mis")
choose_test.wbc <- function(obj, test = "mis", group_id = T) {
  result <- obj %>% get_result(group_id)

  # filter final dataset
  if (test == "mis") {
    result <-
      result %>%
      dplyr::filter(.data$is_right == F) %>%
      dplyr::select(-.data$is_right)
  } else if (test == "ok") {
    result <-
      result %>%
      dplyr::filter(.data$is_right == T) %>%
      dplyr::select(-.data$is_right)
  } else {
    stop("uknown parameter ", test)
  }

  result
}

#' Run tests
#'
#' @param dataset A data frame.
#' @param row_file A data frame. A data frame with parameters.
#' @param part A character scalar. Prefixes or postfixes.
#' @param obj An object. Class wbc.
#'
#' @return A data frame. The part of the final result.
#'
run_tests.wbc <- function(obj, dataset, row_file, part) {
  id <- obj[["id"]]
  is_post <- obj[["is_post"]]

  # wbc's parameters
  relative <- row_file$relative
  absolute <- row_file$absolute
  all <- row_file$all
  human_name <- row_file$human_name

  # wbc's parameter with prefix or postfix
  rel <- ifelse(is_post, paste0(relative, part), paste0(part, relative))
  abs <- ifelse(is_post, paste0(absolute, part), paste0(part, absolute))
  all <- ifelse(is_post, paste0(all, part), paste0(part, all))

  vars_rename <- c("rel" = rel, "abs" = abs, "all" = all)

  # calculate
  dataset %>%
    dplyr::mutate(human_name = human_name, lab_name = abs) %>%
    dplyr::select(!!id, .data$human_name, .data$lab_name, !!rel, !!all, !!abs) %>%
    dplyr::mutate(!!rel := to_dbl(.data[[rel]]), !!all := to_dbl(.data[[all]]), !!abs := to_dbl(.data[[abs]])) %>%
    dplyr::mutate(auto_abs = round((.data[[rel]] * .data[[all]]) / 100, 2)) %>%
    dplyr::mutate(is_right = dplyr::near(.data$auto_abs, .data[[abs]])) %>%
    dplyr::rename(!!vars_rename)
}
