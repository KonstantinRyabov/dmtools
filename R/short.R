#' Create object short
#'
#' @param file A character scalar. Path to the excel table.
#' @param id A column name of the subject id in the dataset, without quotes.
#' @param name_to_find A character scalar. For search prefixes or postfixes.
#' @param extra A character scalar. For additional information.
#' @param common_cols A character vector. A column names in the dataset, which common for all events.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#' @param is_add_cols A logical scalar, default is FALSE. If necessary add columns.
#'
#' @return The object short.
#' @export
#'
#' @examples
#' obj_short <- short("preg.xlsx", id, "res", c("site", "sex"))
#' obj_short <- short("labs.xlsx", id, "name_labs", c("site"), "human_name")
short <- function(file,
                  id,
                  name_to_find,
                  common_cols = NULL,
                  extra = NULL,
                  is_post = T,
                  is_add_cols = F) {
  obj <- list(
    file = file,
    id = dplyr::enquo(id),
    common_cols = common_cols,
    is_post = is_post,
    name_to_find = name_to_find,
    extra = extra,
    is_add_cols = is_add_cols,
    bond = "_"
  )

  class(obj) <- "short"
  obj
}

#' Reshape the dataset to a long view
#'
#' @param dataset A data frame.
#' @param obj An object. Class short.
#' @param row_file A data frame. A data frame with parameters.
#' @param part A character scalar. Prefixes or postfixes.
#'
#' @return A data frame. The part of the final result.
#'
to_long.short <- function(obj, dataset, row_file, part) {
  id <- obj[["id"]]
  is_pst <- obj[["is_post"]]
  name_to_find <- obj[["name_to_find"]]
  common_cols <- obj[["common_cols"]]
  extra <- obj[["extra"]]
  is_add_cols <- obj[["is_add_cols"]]

  temp_row <- data.frame()
  if (!is.null(extra)) {
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

  if (is_add_cols) {
    dataset <- add_cols(dataset, part, ds_names)
  }

  result <- data.frame()
  if (is.null(common_cols)) {
    result <- dataset %>%
      dplyr::select(!!id, !!ds_names) %>%
      dplyr::mutate(IDVAR = name_to_find, VISIT = part)
  } else {
    result <- dataset %>%
      dplyr::select(!!id, !!common_cols, !!ds_names) %>%
      dplyr::mutate(IDVAR = name_to_find, VISIT = part)
  }

  if (!is.null(extra)) {
    result <- result %>% dplyr::mutate(!!extra := row_file[[extra]])
  }

  result
}
