github_api_branch_protect_with_review <-function(repo, branch,
                                                 n_approvals,
                                                 code_owners,
                                                 enforce_admins) {
  body <- paste0(
    '{',
    '"required_status_checks":null,',
    str_glue('"enforce_admins":{enforce_admins},'),
    '"required_pull_request_reviews"',
    ':{',
    '"dismiss_stale_reviews":false,',
    str_glue('"require_code_owner_reviews":{code_owners},'),
    str_glue('"required_approving_review_count":{n_approvals}'),
    '},',
    '"restrictions":null',
    '}'
  )

  ghclass_api_v3_req(
    "PUT /repos/:owner/:repo/branches/:branch/protection",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    branch = branch,
    charToRaw(body),
    .send_headers = c(
      Accept = "application/vnd.github.luke-cage-preview+json",
      "Content-Type" = "application/json"
    )
  )
}



#' Branch protection
#'
#' Adds or removes branch protection on the repo. Specifically,
#' added branch protection enforces 1 approving
#' PR review from Codeowners before allowing a merge.
#' This is typically coupled with a `.github/CODEOWNERS`
#' file in the repo root (on `main/master`).
#'
#' @param repo Github repository address in `owner/repo` format.
#' @param branch Repository branch to use.
#' @param n_approvals Integer. Number of required PR reviews. Rounds to nearest
#'  integer if necessary.
#' @param code_owners Logical. Must CODEOWNERS reviews be required.
#' @param enforce_admins Logical. Does protection apply to admins.
#'
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test", "protect_me", auto_init=TRUE)
#'
#' repo_put_file("ghclass-test/protect_me", ".github/CODEOWNERS",
#'   content = "* @dajmcdon")
#' branch_protect_with_review("ghclass-test/protect_me", "main")
#' }
branch_protect_with_review <- function(repo, branch,
                                       n_approvals = 1L,
                                       code_owners = FALSE,
                                       enforce_admins = FALSE) {
  arg_is_chr(repo, branch)
  n_approvals <- as.character(round(n_approvals))
  code_owners <- tolower(as.character(code_owners))
  enforce_admins <- tolower(as.character(enforce_admins))

  purrr::walk2(
    repo, branch,
    function(repo, branch) {
      res = purrr::safely(github_api_branch_protect_with_review)(
        repo, branch, n_approvals, code_owners, enforce_admins)

      repo_fmt = format_repo(repo, branch)

      status_msg(
        res,
        "Protecting branch {.val {repo_fmt}}, and requiring 1 review from CODEOWNERS.",
        "Failed to protect branch {.val {repo_fmt}}."
      )
    }
  )
}
