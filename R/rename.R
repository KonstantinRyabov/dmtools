#' For rename dataset
#'
#' @param dataset A dataset, a type is a data frame.
#' @param is_post A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#' @param path_crfs A character scalar. Path to the specification files the in excel table.
#' @param no_readable_name A character scalar. A column name of no_readable values.
#' @param readable_name A character scalar. A column name of readable values.
#' @param num_sheet An integer scalar, default is the first sheet. A position of a sheet in the excel document.
#'
#' @return The list with two values: data - renamed dataset, spec - common specification.
#'         The common specification is data frame of two values: no_readable_var, readable_var.
#'
#' @export
#' @examples
#' id <- c("01", "02", "03")
#' age <- c("19", "20", "22")
#' sex <- c("f", "m", "f")
#' bio_date_post <- c("1991-03-23", "1991-03-16", "1991-03-16")
#' gluc_post <- c("5.5", "4.1", "9.7")
#' gluc_res_post <- c("norm", "no", "norm")
#'
#'
#' df <- data.frame(
#'   id, age, sex,
#'   bio_date_post,
#'   gluc_post, gluc_res_post,
#'   stringsAsFactors = FALSE
#' )
#'
#' crfs <- system.file("forms", package = "dmtools")
#'
#' result <- rename_dataset(df, crfs, "old_name", "new_name")
#' result[["data"]]
rename_dataset <- function(dataset, path_crfs, no_readable_name, readable_name, num_sheet = 1, is_post = T) {
  # all names of the dataset
  df_colname <- names(dataset)

  # files of specification
  files <- list.files(path = path_crfs, pattern = "*.xlsx")

  # create the common specification
  all_spec <- do.call(rbind, lapply(files, function(file) {
    # load a file of specification
    vars <- c(no_readable_var = no_readable_name, readable_var = readable_name)
    file <- file.path(path_crfs, file)
    spec <- readxl::read_xlsx(file, sheet = num_sheet) %>%
      dplyr::rename(!!vars)

    if (length(spec$no_readable_var) == 0) {
      return()
    }
    # pattern for find
    name <- spec$no_readable_var[1]
    # names of crf's values
    name_find <- ifelse(is_post, paste0("^", name, "_"), paste0("_", name, "$"))
    spec_names <- df_colname[grepl(name_find, df_colname)]

    # parts
    parts <- unique(gsub(name, "", spec_names))

    # create a specification of one crf with a different prefix
    do.call(rbind, lapply(parts, function(part) {
      create_spec(spec, df_colname, part, is_post)
    }))
  }))

  # rename the dataset
  dset <- dataset %>% dplyr::rename(!!purrr::set_names(all_spec$no_readable_var, all_spec$readable_var))
  list(data = dset, spec = all_spec)
}

#' For creating part of the specification
#'
#' @param df_spec A dataset, a type is a data frame.
#' @param all_colname A character vector with all names in the dataset.
#' @param part_spec A character scalar. Prefixes or postfixes.
#' @param is_pst A logical scalar, default is TRUE. True is postfix, otherwise, prefix.
#'
#' @return A data frame. Part of the specification.
#'
create_spec <- function(df_spec, all_colname, part_spec, is_pst) {
  logics <- rep(is_pst, nrow(df_spec))
  # colomn names in readable format with prefix or postfix
  new_names <- ifelse(logics, paste0(df_spec$readable_var, part_spec), paste0(part_spec, df_spec$readable_var))
  # colomn names with prefix or postfix
  old_names <- ifelse(logics, paste0(df_spec$no_readable_var, part_spec), paste0(part_spec, df_spec$no_readable_var))
  # index of neccessary colomn
  index <- old_names %in% all_colname
  # change colomn in a specification
  df_spec <- df_spec %>% dplyr::mutate(readable_var = new_names, no_readable_var = old_names)
  # filter dataset
  df_spec[index, c("no_readable_var", "readable_var")]
}
