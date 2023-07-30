github_api_repo_prs = function(repo, state) {
  arg_is_chr_scalar(repo, state)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/pulls",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    state = state
  )
}


#' @rdname repo_details
#'
#' @param state Character. Pull request state.
#' @param tz Character. A time zone for PR Date/Time conversion. See
#'   `lubridate::force_tz()` for  alternatives (`tz = "UTC"` would be natural).
#' @export
#'
repo_prs = function(repo, state = c("open","closed","all"),
                    tz = "America/Vancouver") {
  state = match.arg(state)
  arg_is_chr(repo)
  arg_is_chr_scalar(state)

  # TODO - check on id?

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_prs)(repo, state)

      if (empty_result(res)) {
        tibble::tibble(
          repo = character(),
          pr = integer(),
          id = integer(),
          title = character(),
          created = lubridate::POSIXct(),
          state = character(),
          base_ref = character(),
          head_ref = character()
        )
      } else {
        tibble::tibble(
          repo = repo,
          pr = purrr::map_int(result(res), "number", .default = NA),
          id = purrr::map_int(result(res), "id", .default = NA),
          title = purrr::map_chr(result(res), "title", .default = NA),
          created = lubridate::ymd_hms(
            purrr::map_chr(result(res), "created_at", .default = NA), tz = tz),
          state = purrr::map_chr(result(res), "state", .default = NA),
          base_ref = purrr::map_chr(result(res), c("base","ref"), .default = NA),
          head_ref = purrr::map_chr(result(res), c("head","ref"), .default = NA)
        )
      }
    }
  )
}

github_api_pr_comment = function(repo, pr_number, body) {
  arg_is_chr(repo, body)
  arg_is_pos_int(pr_number)

  ghclass_api_v3_req(
    endpoint = "POST /repos/:owner/:repo/issues/:issue_number/comments",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    issue_number = pr_number,
    body = body,
    .send_headers = c(Accept = "application/vnd.github.shadow-cat-preview+json")
  )
}

#' @rdname pr
#'
#' @param pr_number Integer. The number of the pull request.
#' @param body Character. Text to be added in the comment.
#' @export
#'
pr_comment = function(repo, pr_number, body = "") {

  arg_is_chr(repo, body)
  arg_is_pos_int(pr_number)

  res = purrr::pmap(
    list(repo, pr_number, body),
    function(repo, pr_number, body) {
      res = purrr::safely(github_api_pr_comment)(
        repo, pr_number = pr_number, body = body
      )


      status_msg(
        res,
        "Commented on pull request number {pr_number} in {repo}.",
        "Failed to comment on pull request {pr_number} in {repo}."
      )
    }
  )

  invisible(res)
}

