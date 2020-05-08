#' Create object short
#'
#' @param file A character scalar. Path to the excel table.
#' @param id A column name of the subject id in the dataset, without quotes.
#' @param name_to_find A character scalar. For search prefixes or postfixes.
#' @param extra A character scalar. For additional information.
#' @param common_cols A character vector. A column names in the dataset, which common for all events.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#'
#' @return The object short.
#' @export
#'
#' @examples
#' obj_short <- short("preg.xlsx", id,"res", c("site", "sex"))
#' obj_short <- short("labs.xlsx", id,"name_labs", c("site"), "human_name")
#'
short <- function(file, id, name_to_find, common_cols = c(), extra = NA, is_post = T) {

  id <- dplyr::enquo(id)

  obj <- list(
    file = file,
    id = id,
    common_cols = common_cols,
    is_post = is_post,
    name_to_find = name_to_find,
    extra = extra,
    bond = "_"
  )

  class(obj) <- "short"
  obj
}

#' Run tests
#'
#' @param dataset A data frame.
#' @param obj An object. Class short.
#' @param row_file A data frame. A data frame with parameters.
#' @param part A character scalar. Prefixes or postfixes.
#'
#' @return A data frame. The part of the final result.
#'
run_tests.short <- function(obj, dataset, row_file, part) {

  id <- obj[["id"]]
  is_pst <- obj[["is_post"]]
  name_to_find <- obj[["name_to_find"]]
  common_cols <- obj[["common_cols"]]
  extra <- obj[["extra"]]

  temp_row <- data.frame()
  if (!is.na(extra)) {
    temp_row <- row_file %>% dplyr::select(-!!extra)
  } else {
    temp_row <- row_file
  }

  logics <- rep(is_pst, ncol(temp_row))
  col_names <- names(temp_row)
  ds_names <- unlist(temp_row)
  ds_names <- ifelse(logics, paste0(ds_names, part), paste0(part, ds_names))
  names(ds_names) <- col_names

  name_to_find <- row_file[[name_to_find]]
  name_to_find <- ifelse(is_pst, paste0(name_to_find, part), paste0(part, name_to_find))

  result <- data.frame()
  if (length(common_cols) == 0) {
    result <- dataset %>%
      dplyr::select(!!id, !!ds_names) %>%
      dplyr::mutate(name_to_find = name_to_find)
  } else {
    result <- dataset %>%
      dplyr::select(!!id, !!common_cols, !!ds_names) %>%
      dplyr::mutate(name_to_find = name_to_find)
  }

  if (!is.na(extra)) {
    result <- result %>% dplyr::mutate(!!extra := row_file[[extra]])
  }

  result
}


