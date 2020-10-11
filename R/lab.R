#' Create object lab
#'
#' @param file A character scalar. Path to the laboratory's reference in the excel table.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#' @param id A column name of the subject id in the dataset, without quotes.
#' @param age A column name of the subject age in the dataset, without quotes.
#' @param sex A column name of the subject sex in the dataset, without quotes.
#' @param normal A normal estimate, for example, "NORMAL".
#' @param abnormal An abnormal estimate, for example, "ABNORMAL".
#' @param clsig A clinical significant estimate, for example, "CLISIG".
#' @param site A site number, default: NA.
#' @param name_to_find A character scalar. For search prefixes or postfixes, default is "LBNDIND".
#'
#' @return The object lab.
#' @export
#'
#' @examples
#' obj_lab <- lab("lab_refer.xlsx", ID, AGE, SEX, 1, 2)
#' obj_lab <- lab("lab_refer.xlsx", ID, AGE, SEX, "NORMAL", "NOCLISIG", clsig = "CLISIG")
#' obj_lab <- lab("lab_refer.xlsx", ID, AGE, SEX, "norm", "no", FALSE)
lab <- function(file,
                id,
                age,
                sex,
                normal,
                abnormal,
                is_post = T,
                clsig = NULL,
                site = NA,
                name_to_find = "LBNDIND") {
  obj <- list(
    file = file,
    id = dplyr::enquo(id),
    age = dplyr::enquo(age),
    sex = dplyr::enquo(sex),
    normal = normal,
    abnormal = abnormal,
    is_post = is_post,
    clsig = clsig,
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
#' ID <- c("01", "02", "03")
#' SITE <- c("site 01", "site 02", "site 03")
#' AGE <- c("19", "20", "22")
#' SEX <- c("f", "m", "f")
#' GLUC_V1 <- c(5.5, 4.1, 9.7)
#' GLUC_IND_V1 <- c("norm", "no", "cl")
#' AST_V2 <- c("30", "48", "31")
#' AST_IND_V2 <- c(NA, "norm", "norm")
#'
#' df <- data.frame(
#'   ID, SITE, AGE, SEX,
#'   GLUC_V1, GLUC_IND_V1,
#'   AST_V2, AST_IND_V2,
#'   stringsAsFactors = FALSE
#' )
#'
#' refs <- system.file("labs_refer.xlsx", package = "dmtools")
#' obj_lab <- lab(refs, ID, AGE, SEX, "norm", "no")
#'
#' obj_lab <- check(obj_lab, df)
#' choose_test(obj_lab, "mis")
choose_test.lab <- function(obj, test = "mis", group_id = T) {
  result <- obj %>% get_result(group_id)

  # filter final dataset
  if (test == "mis") {
    result <-
      result %>%
      dplyr::filter(.data$IS_RIGHT == F) %>%
      dplyr::select(-.data$IS_RIGHT)
  } else if (test == "ok") {
    result <-
      result %>%
      dplyr::filter(.data$IS_RIGHT == T) %>%
      dplyr::select(-.data$IS_RIGHT)
  } else if (test == "skip") {
    result <-
      result %>%
      dplyr::filter(!is.na(.data$LBORRES) &
        is.na(.data$LBNRIND)) %>%
      dplyr::select(-.data$IS_RIGHT)
  } else if (test == "null") {
    result <-
      result %>%
      dplyr::filter(is.na(.data$LBORRES) &
        is.na(.data$LBNRIND)) %>%
      dplyr::select(-.data$IS_RIGHT)
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
  normal <- obj[["normal"]]
  abnormal <- obj[["abnormal"]]
  is_post <- obj[["is_post"]]
  obj_cl <- obj[["clsig"]]
  lbclsig <- ifelse(is.null(obj_cl), abnormal, obj_cl)

  # laboratory's parameters
  lbtest <- row_file$LBTEST
  lbtestcd <- row_file$LBORRES
  lbnrind <- row_file$LBNDIND
  lbornrlo <- as.double(row_file$LBORNRLO)
  lbornrhi <- as.double(row_file$LBORNRHI)
  age_low <- as.double(row_file$AGELOW)
  age_high <- as.double(row_file$AGEHIGH)
  pattern_sex <- paste0("^", row_file$SEX, "$")

  if (age_low > age_high) {
    warning("AGELOW > AGEHIGH in ", lbtest)
  }

  if (lbornrlo > lbornrhi) {
    warning("LBORNRLO > LBORNRHI in ", lbtest)
  }

  # laboratory's parameter with prefix or postfix
  lborres <- ifelse(is_post, paste0(lbtestcd, part), paste0(part, lbtestcd))
  lbnrind <- ifelse(is_post, paste0(lbnrind, part), paste0(part, lbnrind))

  vars_rename <- c("LBORRES" = lborres, "LBNRIND" = lbnrind)

  # filter by age and sex
  by_age_sex <- dataset %>%
    dplyr::mutate(!!age := as.double(!!age)) %>%
    dplyr::filter(dplyr::between(!!age, age_low, age_high), grepl(pattern_sex, !!sex))

  # validate by reference values
  result <- by_age_sex %>%
    dplyr::mutate(LBTESCD = lbtestcd, LBTEST = lbtest, VISIT = part, LBORNRLO = lbornrlo, LBORNRHI = lbornrhi) %>%
    dplyr::select(!!id, !!age, !!sex, .data$LBTEST, .data$LBTESCD, .data$VISIT, .data$LBORNRLO, .data$LBORNRHI, !!lborres, !!lbnrind) %>%
    dplyr::mutate(RES_TYPE_NUM = to_dbl(.data[[lborres]])) %>%
    dplyr::mutate(IND_EXPECTED = create_norm(.data$RES_TYPE_NUM, lbornrlo, lbornrhi, .data[[lbnrind]], normal, abnormal, lbclsig)) %>%
    dplyr::mutate(IS_RIGHT = .data$IND_EXPECTED == .data[[lbnrind]]) %>%
    dplyr::rename(!!vars_rename)

  result
}

#' Estimating laboratory values
#'
#' @param vals A double vector. The laboratory values.
#' @param low A double scalar. The minimum.
#' @param high double scalar. The maximum.
#' @param ds_norm An estimate of the laboratory values from the dataset.
#' @param normal An option for the normal estimate, for example, "NORMAL".
#' @param abnormal An option for the abnormal estimate, for example, "ABNORMAL".
#' @param clsig An option for the clinical significant estimate, for example, "CLISIG".
#'
#' @return A vector with the auto estimate.
#'
create_norm <- function(vals, low, high, ds_norm, normal, abnormal, clsig) {
  temp_norm <- ifelse(dplyr::between(vals, low, high), normal, abnormal)
  temp_cl <- ifelse(dplyr::between(vals, low, high), normal, clsig)
  ifelse(temp_cl == ds_norm, temp_cl, temp_norm)
}
