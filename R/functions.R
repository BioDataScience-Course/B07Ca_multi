# Functions to caculate scores for multivariate analyses
# Copyright (c) 2024 Ph. Grosjean (phgrosjean@sciviews.org) & Guyliann Engels

repository <- sub("\\.git$", "", basename(usethis::git_remotes()$origin))
svMisc::assign_temp("scores", numeric(10L), replace.existing = TRUE)
reference_dir <- fs::path("figures")
output_dir <- fs::path("challenge_multi_files", "figure-html")
results_dir <- fs::path("results")
unlink(dir(output_dir, pattern = "\\.png$"))

#' Calculate the score for one chart
#'
#' @param multi The number from 1 to 10 of the multivariate analysis to score
#' @param ref_dir The directory containing the reference image
#' @param out_dir The directory containing the produced chart
#'
#' @return A score from 0 to 1
#' @export
#'
#' @examples
#' score_multi(1) # Get score for first multivariate analysis
score_multi <- function(multi, ref_dir = reference_dir, out_dir = output_dir) {
  multi <- as.integer(multi)[1]
  if (multi < 1 | multi > 10)
    stop("argument multi = must be an integer between 1 and 10")
  # Get vector of scores
  scores <- svMisc::get_temp("scores", default = NULL)
  if (is.null(scores))
    stop("scores not found")
  # Filename
  multifile <- sprintf("multi%02.0f-1.png", multi)
  # Read reference multi
  reffile <- file.path(ref_dir, multifile)
  if (!file.exists(reffile))
    stop("The reference image for multi ", multi, " is not found")
  ref <- png::readPNG(reffile)[, , 1:3] # Do not use alpha channel
  # Is a multivariate analysis produced?
  outfile <- file.path(out_dir, multifile)
  if (file.exists(outfile)) {
    out <- png::readPNG(outfile)[, , 1:3] # Do not use alpha channel
    score <- try(cor(ref, out), silent = TRUE)
    if (inherits(score, "try-error")) {
      score <- 0
    } else {# If score is too low, we consider it is a different item
      if (score < 0.8)
        score <- 0
    }
  } else {
    score <- 0
  }
  # Record the score and return it
  scores[multi] <- score
  svMisc::assign_temp("scores", scores, replace.existing = TRUE)
  score
}


#' Calculate the global score for all multivariate analyses
#'
#' @param res_dir The directory where to place the results
#' @param repos The current GitHub repository
#'
#' @return
#' @export
#'
#' @examples
score_all_multi <- function(res_dir = results_dir, repos = repository) {
  scores <- svMisc::get_temp("scores", default = NULL)
  if (is.null(scores))
    stop("scores not found")
  # Get an id and file name, according to current files in results
  all_res <- dir(res_dir, full.names = FALSE,
    pattern = paste0("^", repos, "__[0-9]{3}\\.rds$"))
  if (!length(all_res)) {
    id <- "001"
  } else {
    last_res <- sort(all_res, decreasing = TRUE)[1]
    last_id <-
      sub("^.+__([0-9]{3})\\.rds$", "\\1", last_res)
    id <- sprintf("%03.0f", as.integer(last_id) + 1)
  }
  file <- glue::glue("{repos}__{id}.rds")
  attr(scores, "id") <- id
  attr(scores, "file") <- file
  # Save the results file (only if score > 0)
  if (sum(scores) > 0) {
    resfile <- file.path(res_dir, file)
    write$rds(scores, file = resfile)
  }
  scores
}
