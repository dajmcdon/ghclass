
# ghclass <img src='man/figures/logo.png' align="right" height="140" />

<!-- badges: start -->

[![R build
status](https://github.com/rundel/ghclass/workflows/R-CMD-check/badge.svg)](https://github.com/rundel/ghclass/actions?query=workflow%3AR-CMD-check)
<!-- badges: end -->

## Tools for managing GitHub class organization accounts

This R package is designed to enable instructors to efficiently manage
their courses on GitHub. It has a wide range of functionality for
managing organizations, teams, repositories, and users on GitHub and
helps automate most of the tedious and repetitive tasks around creating
and distributing assignments.

Install ghclass from CRAN:

``` r
install.packages("ghclass")
```

Install the development version package from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("dajmcdon/ghclass")
```

See package
[vignette](https://rundel.github.io/ghclass/articles/articles/ghclass.html)
for details on how to use the package.

## GitHub & default branches

Recently, GitHub has announced that they will be changing the default
branch for all new repositories on their platform to `main` from
`master`. Details on this change and the timeline for implementation are
available [here](https://github.com/github/renaming). In anticipation of
these changes we have updated `ghclass` to support alternative default
branch names across the entire package. See the FAQ in the Getting
Started vignette for more details.

## dajmcdon updates

This version differs slightly from the released/development versions
maintained by Colin Rundel. Mainly, we want functionality for a large
class with individual (private) repos. Modifications

-   `branch_protect_with_review()` adds protection to a branch in all
    repos matching the pattern. The idea is to prevent pushes to master
    and require a TA-approved PR (for grading).
-   `branch_unprotect()` removes the protection. Is commented out in the
    crundel source.
-   `repo_add_randomized_text_file()` similar to `repo_add_file()` but
    randomly replaces a few lines of text (ignoring empty lines and
    those beginning with `#`) differently for each repo.
-   `repo_add_sampled_data_file()` similar to `repo_add_file()` but
    expects the input to be a `.rds` containing a `data.frame`, `tibble`
    or `vector`. It samples the data randomly for each repo. Good for
    giving different training sets to different students.
-   `repo_prs()` added a column to record the time that the PR was made.
