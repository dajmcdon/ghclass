github_api_repo_pr_reviewers = function(repo, pr_number) {
  arg_is_chr_scalar(repo)
  arg_is_pos_int(pr_number)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/pulls/:pull_number/requested_reviewers",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    pull_number = pr_number
  )
}

#' @rdname repo_details
#'
#' @param pr_number Integer. Pull request number.
#' @export
#'
repo_pr_reviewers = function(repo, pr_number) {
  arg_is_chr(repo)
  arg_is_pos_int(pr_number)

  purrr::map2_dfr(
    repo, pr_number,
    function(repo, pr_num) {
      res = purrr::safely(github_api_repo_pr_reviewers)(repo, pr_num)

      if (empty_result(res)) {
        tibble::tibble(
          repo = character(),
          pr = integer(),
          reviewer = character()
        )
      } else {
        tibble::tibble(
          repo = repo,
          pr = pr_num,
          reviewer = purrr::map_chr(result(res)$users, "login", .default = NA)
        )
      }
    }
  )
}
