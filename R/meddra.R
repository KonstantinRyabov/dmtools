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

  post_res <- httr::POST(
    url = target_url,
    body = list(grant_type = "password", username = meddra_id, password = api_key, scope = "meddraapi"),
    httr::authenticate("mspclient", "clientsecret"),
    encode = "form"
  )

  post_res <- httr::content(post_res)
  token <- paste(post_res$token_type, post_res$access_token)
  token
}

#' Create the post query
#'
#' @param target_url The url for a post query.
#' @param json  A string scalar. The json query.
#' @param token The user's token.
#' @param encoding The json encoding. The default is utf-8.
#'
#' @return
#' A data frame. The result of query.
#' @export
#'
#' @examples
#' \dontrun{
#' meddra_post(url, json_body, token)
#' }
meddra_post <- function(target_url, json, token, encoding = "UTF-8") {

  post_res <- httr::POST(
    url = target_url,
    body = iconv(json, to = encoding),
    httr::add_headers(Authorization = token), httr::accept_json(), httr::content_type_json()
  )

  post_res <- httr::content(post_res)
  tibble::tibble(result = post_res) %>% tidyr::unnest_wider(col = .data[["result"]])
}
