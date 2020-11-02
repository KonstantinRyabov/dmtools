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
check.default <- function(obj, dataset) {

  # mutate empty strings in NA
  dataset <- dataset %>% dplyr::mutate(dplyr::across(.fns = ~ ifelse(. == "", NA, .)))

  if (class(obj) == "lab") {
    subjs <- dataset %>%
      dplyr::filter(is.na(!!obj[["age"]]) | is.na(!!obj[["sex"]])) %>%
      dplyr::select(!!obj[["id"]]) %>%
      unlist() %>%
      unique()

    if (length(subjs) != 0) {
      warning("problem with age or sex: ", paste(subjs, collapse = ","))
    }
  }

  # load file
  file <- obj[["file"]]
  labs <- readxl::read_xlsx(file)

  # create final dataset
  rs <- do.call(rbind, lapply(
    seq_len(nrow(labs)),
    function(n) {
      find_colnames(obj, dataset, labs[n, ])
    }
  ))

  # if dataset is empty
  if (nrow(rs) == 0) {
    stop("the final result of validation is empty")
  } else {
    obj[["result"]] <- rs
  }

  obj
}


#' Find column names
#'
#' @param obj An object for validation.
#' @param dataset A dataset, a type is a data frame.
#' @param row_file A row of the file.
#'
#' @return A data frame. Result of run_tests.
#'
find_colnames.default <- function(obj, dataset, row_file) {

  # name to find
  name_to_find <- obj[["name_to_find"]]
  name <- row_file[[name_to_find]]
  # all names of the dataset
  dset_colnames <- names(dataset)

  # names from one row
  is_post <- obj[["is_post"]]
  bond <- obj[["bond"]]
  name_find <- ifelse(is_post, paste0("^", name, bond), paste0(bond, name, "$"))
  result_find <- grepl(name_find, dset_colnames)

  if (is.null(name)) {
    stop("name_to_find is wrong")
  }

  # if not found
  if (!any(result_find)) {
    warning(name, " not found")
  }

  names <- dset_colnames[result_find]

  # prefixes or postfixes
  parts <- unique(gsub(name, "", names))

  # execute tests
  do.call(
    rbind,
    lapply(parts, function(part) {
      tryCatch(to_long(obj, dataset, row_file, part),
        error = function(e) {
          warning(part, " can't bind, because ", e)
          data.frame()
        }
      )
    })
  )
}

#' Get the final result of the check
#'
#' @param obj An object. Can be all classes: short, lab, date.
#' @param group_id A logical scalar, default is TRUE.True is grouped by id, otherwise, it isn't grouped.
#'
#' @return A data frame. The final result.
#' @export
#'
#' @examples
#' id <- c("01", "02", "03")
#' site <- c("site 01", "site 02", "site 03")
#' sex <- c("f", "m", "f")
#' preg_yn_e2 <- c("y", "y", "y")
#' preg_res_e2 <- c("neg", "neg", "neg")
#' preg_yn_e3 <- c("y", "y", "n")
#' preg_res_e3 <- c("neg", "pos", "unnes")
#'
#' df <- data.frame(
#'   id, site, sex,
#'   preg_yn_e2, preg_res_e2,
#'   preg_yn_e3, preg_res_e3,
#'   stringsAsFactors = FALSE
#' )
#'
#' preg <- system.file("preg.xlsx", package = "dmtools")
#' obj_short <- short(preg, id, "LBORRES", c("site", "sex"))
#'
#' obj_short <- check(obj_short, df)
#' get_result(obj_short)
get_result <- function(obj, group_id = T) {
  result <- obj[["result"]]

  if (group_id) {
    id <- obj[["id"]]
    result <- result %>%
      dplyr::arrange(!!id)
  }

  result
}


#' Cast to double type
#'
#' @param vals A character or double vector.
#'
#' @return A double vector.
#'
to_dbl <- function(vals) {
  if (is.numeric(vals)) {
    vals
  } else {
    with_point <- gsub(",", ".", vals)
    rs <- ifelse(grepl("^[0-9.]+$", with_point), with_point, "Inf")
    as.double(rs)
  }
}

#' Add columns if columns don't exist
#'
#' @param dset A data frame. The dataset.
#' @param ds_part A character scalar. Prefix or postfix.
#' @param target_cols A character vector with necessary columns.
#'
#' @return A data frame. The dataset.
#
add_cols <- function(dset, ds_part, target_cols) {
  part_dset <- dset %>% dplyr::select(dplyr::contains(ds_part))
  part_cols <- names(part_dset)
  diff <- dplyr::setdiff(target_cols, part_cols)
  NAs <- rep(NA, nrow(dset))

  for (col in diff) {
    dset[col] <- NAs
  }

  dset
}
