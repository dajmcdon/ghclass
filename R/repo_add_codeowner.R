#' Add CODEOWNERS to repo
#'
#' Add / replace `.github/CODEOWNERS` files in a GitHub repository.
#' Note that due to delays in caching, files that have been added
#' very recently might not yet be displayed as existing and might accidentally
#' be overwritten. The `owner` will be responsible for all files in the repo.
#'
#' @param repo  Character. Address of repository in `owner/name` format.
#' @param owner Character. Username of the owner. Without any prefix.
#' @param message Character. Commit message.
#' @param overwrite Logical. Should any existing CODEOWNERS file be replaced?
#'
#' @return Called for its side effects. invisibly return a list containing the
#'   results of the relevant GitHub API calls.
#' @export
#'
#' @examples
#' \dontrun{
#' repo_add_codeowner("orgname/example-repo", "dajmcdon")
#' }
repo_add_codeowner <- function(repo, owner, message = NULL, overwrite = FALSE) {
  arg_is_chr(repo)
  arg_is_chr(owner, allow_na = TRUE)
  arg_is_chr_scalar(message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  purrr::walk2(repo, owner, function(repo, owner) {
    if (!is.na(owner)) {
      if (!file_exists(repo, ".github/CODEOWNERS") | overwrite) {
        repo_put_file(
          repo = repo, path = ".github/CODEOWNERS",
          content = paste0("* @", owner),
          message = message,
          verbose = TRUE)
      } else {
        cli::cli_alert_danger(
          c("Failed to add CODEOWNERS to repo {.val {repo}}: file exists. ",
            "To force add this file, re-run with {.code overwrite = TRUE}."),
          wrap = TRUE
        )
      }
    } else {
      cli::cli_alert_danger(
        "Failed to add file CODEOWNERS to repo {.val {repo}}, because the owner was {.code NA}."
      )
    }
  })

}
