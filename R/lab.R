#' Create object lab
#'
#' @param file A character scalar. Path to the laboratory's reference in the excel table.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#' @param id A column name of the subject id in the dataset, without quotes.
#' @param age A column name of the subject age in the dataset, without quotes.
#' @param sex A column name of the subject sex in the dataset, without quotes.
#' @param normal A normal estimate, for example, "NORMAL".
#' @param abnormal An abnormal estimate, for example, "ABNORMAL".
#' @param name_to_find A character scalar. For search prefixes or postfixes, default is "LBNRIND".
#'
#' @return The object lab.
#' @export
#'
#' @examples
#' obj_lab <- lab("lab_refer.xlsx", ID, AGE, SEX, 1, 2)
#' obj_lab <- lab("lab_refer.xlsx", ID, AGE, SEX, "NORMAL", "ABNORMAL")
#' obj_lab <- lab("lab_refer.xlsx", ID, AGE, SEX, "norm", "no", FALSE)
lab <- function(file,
                id,
                age,
                sex,
                normal,
                abnormal,
                is_post = T,
                name_to_find = "LBNRIND") {
  obj <- list(
    file = file,
    id = dplyr::enquo(id),
    age = dplyr::enquo(age),
    sex = dplyr::enquo(sex),
    normal = normal,
    abnormal = abnormal,
    is_post = is_post,
    name_to_find = name_to_find,
    bond = "_"
  )

  class(obj) <- "lab"
  obj
}

#' Filter the final result of the object lab
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

#' Reshape the dataset to a long view
#'
#' @param dataset A data frame.
#' @param obj An object. Class lab.
#' @param row_file A data frame. A data frame with parameters.
#' @param part A character scalar. Prefixes or postfixes.
#'
#' @return A data frame. The part of the final result.
#'
to_long.lab <- function(obj, dataset, row_file, part) {
  # object's parameters
  id <- obj[["id"]]
  age <- obj[["age"]]
  sex <- obj[["sex"]]
  normal <- obj[["normal"]]
  abnormal <- obj[["abnormal"]]
  is_post <- obj[["is_post"]]
  obj_cl <- obj[["clsig"]]

  # laboratory's parameters
  lbtest <- row_file$LBTEST
  lbtestcd <- row_file$LBORRES
  lbnrind <- row_file$LBNRIND
  lbornrlo <- as.double(row_file$LBORNRLO)
  lbornrhi <- as.double(row_file$LBORNRHI)
  age_low <- as.double(row_file$AGELOW)
  age_high <- as.double(row_file$AGEHIGH)
  pattern_sex <- paste0("^", row_file$SEX, "$")

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
    dplyr::mutate(LBTESTCD = lbtestcd, LBTEST = lbtest, VISIT = part, LBORNRLO = lbornrlo, LBORNRHI = lbornrhi) %>%
    dplyr::select(!!id, !!age, !!sex, .data$LBTEST, .data$LBTESTCD, .data$VISIT, .data$LBORNRLO, .data$LBORNRHI, !!lborres, !!lbnrind) %>%
    dplyr::mutate(RES_TYPE_NUM = to_dbl(.data[[lborres]])) %>%
    dplyr::mutate(IND_EXPECTED = ifelse(dplyr::between(.data$RES_TYPE_NUM, lbornrlo, lbornrhi), normal, abnormal)) %>%
    dplyr::mutate(IS_RIGHT = .data$IND_EXPECTED == .data[[lbnrind]]) %>%
    dplyr::rename(!!vars_rename)

  result
}
