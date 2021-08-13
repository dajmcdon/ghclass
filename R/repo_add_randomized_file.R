#' Add randomized versions of a file (unique) to each repo
#'
#' @param message Character. Commit message.
#' @param repo_folder Character. Name of folder on repository to save the file(s) to. If the folder does not exist on the repository, it will be created.
#' @param branch Character. Name of branch to use.
#' @param preserve_path Logical. Should the local relative path be preserved.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#'
#' @name repo_add_randomized
#' @rdname repo_add_randomized
NULL


#' @rdname repo_add_randomized
#' @param repo Character. Address of repository in `owner/name` format.
#' @param file Character. Local file path(s) of file or files to be added.
#' @param nreplace Integer. Number of lines to replace.
#' @param replace_str Character. String to replace previously existing lines.
#'
#' @description `repo_add_randomized_text_file()` takes in text files (R code or a markdown document for example)
#'   and randomly replaces some lines with a text string (only those lines which had text to begin with).
#'   The result is uploaded to an individual repo. Thus, different repos will receive
#'   different versions of the file.
#'
#' @export
repo_add_randomized_text_file = function(repo, file, nreplace = 2, replace_str = "",
                                         message = NULL, repo_folder = NULL,
                                         branch = NULL, preserve_path = FALSE, overwrite = FALSE) {

  arg_is_chr(repo, file)
  arg_is_pos_int_scalar(ndump)
  arg_is_chr(branch, allow_null=TRUE)
  arg_is_chr_scalar(repo_folder, message, allow_null = TRUE)
  arg_is_lgl_scalar(preserve_path, overwrite)

  missing_files = file[!fs::file_exists(file)]
  if (length(missing_files) != 0)
    cli_stop("Unable to locate the following file{?s}: {.val {missing_files}}")

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  if (is.null(branch))
    branch = list(NULL)

  purrr::pwalk(
    list(repo, file, branch),
    function(repo, file, branch) {
      purrr::walk(
        file,
        function(file) {
          gh_path = file
          if (!preserve_path)
            gh_path = fs::path_file(file)

          # ext <- tools::file_ext(file)
          # tmp <- tempfile(fileext = ext)

          dat = readLines(file)
          has_text <- which(! grepl("^\\s$|^#", b))
          # should ignore empty lines `^\\s$` and lines starting with `#`
          dump <- sample(has_text, n, replace = FALSE)
          dat[dump] <- replace_str
          dat <- paste0(dat, collapse = "\n")
          # writeLines(dat, tmp)

          if(!is.null(repo_folder))
            gh_path = fs::path(repo_folder, gh_path)

          if (!file_exists(repo, gh_path, branch) | overwrite) {
            repo_put_file(
              repo = repo,
              path = gh_path,
              content = dat,
              message = message,
              branch = branch,
              verbose = TRUE
            )
            unlink(tmp)
          } else {
            unlink(tmp)
            cli::cli_alert_danger( c(
              "Failed to add file {.val {gh_path}} to repo {.val {repo}}, this file already exists. ",
              "If you want to force add this file, re-run the command with {.code overwrite = TRUE}."
            ) )
          }
        }
      )
    }
  )
}

#' @rdname repo_add_randomized
#'
#' @param n number of items to sample
#' @param replace do we sample with replacement?
#'
#' @description `repo_add_sampled_data_file` takes an RDS file (or set of such) and resamples it
#'   (optionally with replacement) before pushing to a repo.
#'
#' @export
repo_add_sampled_data_file = function(repo, file, n, replace = FALSE,
                                      message = NULL, repo_folder = NULL,
                                      branch = NULL, preserve_path = FALSE,
                                      overwrite = FALSE) {


  arg_is_chr(repo, file)
  arg_is_pos_int_scalar(n)
  arg_is_chr(branch, allow_null=TRUE)
  arg_is_chr_scalar(repo_folder, message, allow_null = TRUE)
  arg_is_lgl_scalar(replace, preserve_path, overwrite)

  missing_files = file[!fs::file_exists(file)]
  if (length(missing_files) != 0)
    cli_stop("Unable to locate the following file{?s}: {.val {missing_files}}")

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  exts <- tolower(sapply(file, tools::file_ext))
  if (any(exts != "rds"))
    cli_stop("All data files must be .Rds (or .rds)")

  if (is.null(branch))
    branch = list(NULL)

  purrr::pwalk(
    list(repo, file, branch),
    function(repo, file, branch) {
      purrr::walk(
        file,
        function(file) {
          gh_path = file
          if (!preserve_path)
            gh_path = fs::path_file(file)

          tmp <- tempfile(fileext = "rds")
          dat <- readRDS(file)
          if (is.data.frame(dat) || is.matrix(dat)) {
            dump <- sample.int(nrow(dat), n, replace = replace)
            dat <- dat[!dump,]
          } else if (is.vector(dat)) {
            dat <- sample(dat, n, replace = replace)
          } else {
            cli_stop("Data type is not supported")
          }

          saveRDS(dat, tmp)

          if(!is.null(repo_folder))
            gh_path = fs::path(repo_folder, gh_path)

          if (!file_exists(repo, gh_path, branch) | overwrite) {
            repo_put_file(
              repo = repo,
              path = gh_path,
              content = read_bin_file(tmp),
              message = message,
              branch = branch,
              verbose = TRUE
            )
          } else {
            cli::cli_alert_danger( c(
              "Failed to add file {.val {gh_path}} to repo {.val {repo}}, this file already exists. ",
              "If you want to force add this file, re-run the command with {.code overwrite = TRUE}."
            ) )
          }
          unlink(tmp)
        }
      )
    }
  )
}
