#' Create object lab
#'
#' @param file A character scalar. Path to the laboratory's reference in the excel table.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#' @param id A column name of the subject id in the dataset, without quotes.
#' @param age A column name of the subject age in the dataset, without quotes.
#' @param sex A column name of the subject sex in the dataset, without quotes.
#' @param norm A normal estimate, for example, "NORMAL".
#' @param no_norm An abnormal estimate, for example, "ABNORMAL".
#' @param cl_sign A clinical significant estimate, for example, "CLISIG".
#' @param site A site number, default: NA.
#' @param name_to_find A character scalar. For search prefixes or postfixes, default is "name_is_norm".
#'
#' @return The object lab.
#' @export
#'
#' @examples
#' obj_lab <- lab("lab_refer.xlsx", id, age, sex, 1, 2)
#' obj_lab <- lab("lab_refer.xlsx", id, age, sex, "NORMAL", "NOCLISIG", cl_sign = "CLISIG")
#' obj_lab <- lab("lab_refer.xlsx", id, age, sex, "norm", "no", FALSE)
lab <- function(file,
                id,
                age,
                sex,
                norm,
                no_norm,
                is_post = T,
                cl_sign = NULL,
                site = NA,
                name_to_find = "name_is_norm") {
  id <- dplyr::enquo(id)
  age <- dplyr::enquo(age)
  sex <- dplyr::enquo(sex)

  obj <- list(
    file = file,
    id = id,
    age = age,
    sex = sex,
    norm = norm,
    no_norm = no_norm,
    is_post = is_post,
    cl_sign = cl_sign,
    site = site,
    name_to_find = name_to_find,
    bond = "_"
  )

  class(obj) <- "lab"
  obj
}

#' Filter final result
#'
#' @param obj An object. Class lab.
#' @param group_id A logical scalar, default is TRUE.True is grouped by id, otherwise, it isn't grouped.
#' @param test A character scalar. Parameters, which use to filter the final dataset, default: "mis":
#'                                      "ok" - analysis, which has a correct estimate of the result,
#'                                      "mis" - analysis, which has an incorrect estimate of the result,
#'                                      "skip" - analysis, which has an empty value of the estimate,
#'                                      "null" - analysis, which has an empty result and value of the estimate.
#'
#'
#' @return The filtered dataset by a value of \code{test}.
#' @export
#'
#' @examples
#' id <- c("01", "02", "03")
#' site <- c("site 01", "site 02", "site 03")
#' age <- c("19", "20", "22")
#' sex <- c("f", "m", "f")
#' gluc_post <- c(5.5, 4.1, 9.7)
#' gluc_res_post <- c("norm", "no", "cl")
#' ast_post <- c("30", "48", "31")
#' ast_res_post <- c(NA, "norm", "norm")
#'
#' df <- data.frame(
#'   id, site, age, sex,
#'   gluc_post, gluc_res_post,
#'   ast_post, ast_res_post,
#'   stringsAsFactors = FALSE
#' )
#'
#' refs <- system.file("labs_refer.xlsx", package = "dmtools")
#' obj_lab <- lab(refs, id, age, sex, "norm", "no")
#'
#' obj_lab <- check(obj_lab, df)
#' choose_test(obj_lab, "mis")
choose_test.lab <- function(obj, test = "mis", group_id = T) {
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
  } else if (test == "skip") {
    result <-
      result %>%
      dplyr::filter(!is.na(.data$lab_vals) &
        is.na(.data$is_norm)) %>%
      dplyr::select(-.data$is_right)
  } else if (test == "null") {
    result <-
      result %>%
      dplyr::filter(is.na(.data$lab_vals) &
        is.na(.data$is_norm)) %>%
      dplyr::select(-.data$is_right)
  } else {
    stop("uknown parameter ", test)
  }

  result
}

#' Run tests
#'
#' @param dataset A data frame.
#' @param obj An object. Class lab.
#' @param row_file A data frame. A data frame with parameters.
#' @param part A character scalar. Prefixes or postfixes.
#'
#' @return A data frame. The part of the final result.
#'
run_tests.lab <- function(obj, dataset, row_file, part) {
  # object's parameters
  id <- obj[["id"]]
  age <- obj[["age"]]
  sex <- obj[["sex"]]
  norm <- obj[["norm"]]
  no_norm <- obj[["no_norm"]]
  is_post <- obj[["is_post"]]
  obj_cl <- obj[["cl_sign"]]
  cl_sign <- ifelse(is.null(obj_cl), no_norm, obj_cl)

  # laboratory's parameters
  human_name <- row_file$human_name
  lab_vals <- row_file$name_lab_vals
  is_norm <- row_file$name_is_norm
  lab_min <- as.double(row_file$lab_vals_min)
  lab_max <- as.double(row_file$lab_vals_max)
  age_min <- as.double(row_file$age_min)
  age_max <- as.double(row_file$age_max)
  pattern_sex <- paste0("^", row_file$sex, "$")

  if (age_min > age_max) {
    warning("age_min > age_max in ", human_name)
  }

  if (lab_min > lab_max) {
    warning("lab_min > lab_max in ", human_name)
  }

  # laboratory's parameter with prefix or postfix
  lab_vals <- ifelse(is_post, paste0(lab_vals, part), paste0(part, lab_vals))
  is_norm <- ifelse(is_post, paste0(is_norm, part), paste0(part, is_norm))

  vars_rename <- c("lab_vals" = lab_vals, "is_norm" = is_norm)

  # filter by age and sex
  by_age_sex <- dataset %>%
    dplyr::mutate(!!age := as.double(!!age)) %>%
    dplyr::filter(dplyr::between(!!age, age_min, age_max), grepl(pattern_sex, !!sex))

  # validate by reference values
  result <- by_age_sex %>%
    dplyr::mutate(name_lab = lab_vals, human_lab = human_name, refs = paste(lab_min, "-", lab_max)) %>%
    dplyr::select(!!id, !!age, !!sex, .data$human_lab, .data$name_lab, .data$refs, !!lab_vals, !!is_norm) %>%
    dplyr::mutate(vals_to_dbl = to_dbl(.data[[lab_vals]])) %>%
    dplyr::mutate(auto_norm = create_norm(.data$vals_to_dbl, lab_min, lab_max, .data[[is_norm]], norm, no_norm, cl_sign)) %>%
    dplyr::mutate(is_right = .data$auto_norm == .data[[is_norm]]) %>%
    dplyr::rename(!!vars_rename)

  result
}

#' Estimating laboratory values
#'
#' @param vals A double vector. The laboratory values.
#' @param left_bound A double scalar. The minimum.
#' @param right_bound A double scalar. The maximum.
#' @param ds_norm An estimate of the laboratory values from the dataset.
#' @param normal An option for the normal estimate, for example, "NORMAL".
#' @param abnormal An option for the abnormal estimate, for example, "ABNORMAL".
#' @param clsign An option for the clinical significant estimate, for example, "CLISIG".
#'
#' @return A vector with the auto estimate.
#'
create_norm <- function(vals, left_bound, right_bound, ds_norm, normal, abnormal, clsign) {
  temp_norm <- ifelse(dplyr::between(vals, left_bound, right_bound), normal, abnormal)
  temp_cl <- ifelse(dplyr::between(vals, left_bound, right_bound), normal, clsign)
  ifelse(temp_cl == ds_norm, temp_cl, temp_norm)
}
