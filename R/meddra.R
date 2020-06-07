#' Get meddra token
#'
#' @param target_url The url for authenticate.
#' @param meddra_id The user's meddra id.
#' @param api_key The users's api key.
#'
#' @return A string scalar. The user's token.
#' @export
#'
#' @examples
#' \dontrun{
#' meddra_auth(url, id, key)
#' }
meddra_auth <- function(target_url, meddra_id, api_key) {
  response_ok <- 200

  post_res <- httr::POST(
    url = target_url,
    body = list(grant_type = "password", username = meddra_id, password = api_key, scope = "meddraapi"),
    httr::authenticate("mspclient", "clientsecret"),
    encode = "form"
  )

  if (post_res$status_code != response_ok) {
    stop("response has status ", post_res$status_code)
  }

  post_res <- httr::content(post_res)
  paste(post_res$token_type, post_res$access_token)
}

#' Create the post query
#'
#' @param target_url The url for a post query.
#' @param json  A string scalar or a list. The json query.
#' @param token The user's token.
#'
#' @return
#' A list. The result of query.
#' @export
#'
#' @examples
#' \dontrun{
#' meddra_post(url, json_body, token)
#' }
meddra_post <- function(target_url, json, token) {
  response_ok <- 200

  post_res <- httr::POST(
    url = target_url,
    body = json,
    httr::add_headers(Authorization = token), httr::content_type_json()
  )

  if (post_res$status_code != response_ok) {
    stop("response has status ", post_res$status_code)
  }

  httr::content(post_res)
}

#' A list to a tibble.
#'
#' @param to_tibble A list with nested lists.
#'
#' @return
#' A tibble.
#' @export
#'
#' @examples
#' temp_list <- list(list(1,3), list(4,5))
#' list_parse(temp_list)
list_parse <- function(to_tibble) {

  tibble::tibble(result = to_tibble) %>%
    tidyr::unnest_wider(col = .data[["result"]])
}
