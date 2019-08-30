#' Tidy REF results and filter by Unit of Assessment
#'
#' This is a convenience function for filtering REF2014 results from a
#' particular Unit of Assessment (UoA; i.e. subject area) by its numeric ID,
#' by exact name or by a partial regular expression.
#'
#' As well as filtering the rows of the \code{\link{results}} dataset, this
#' function also selects only the most useful columns and renames the column to
#' `snake_case`, which is slightly more convenient for programming.
#'
#' @param number an integer from 1 to 36. See \code{\link{UoA}} for a key
#' @param name a string. See \code{\link{UoA}} for possible names
#' @param grep a regular expression representing (part of) a UoA name
#' @param ... arguments passed to \code{\link{grepl}}
#'
#' @examples
#' # All of the following are equivalent:
#' tidy_results(18)
#' tidy_results(name = 'Economics & Econometrics')
#' tidy_results(grep = 'con')
#'
#' @note
#' This function does not check how many UoAs are selected. It could be zero,
#' one or all of them.
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing the following columns:
#' \itemize{
#' \item name of institution
#' \item UoA name
#' \item UoA number
#' \item type of profile (Overall, Environment, Impact, Outputs)
#' \item number of full-time equivalent (FTE) staff in the submission
#' \item percentage of submission rated 4\*, 3\*, 2\*, 1\* or unclassified
#' }
#'
#' @importFrom dplyr %>% transmute filter
#'
#' @export
tidy_results <- function(number, name, grep, ...) {
  ref2014::results %>%
    transmute(institution = `Institution name`,
              uoa_name = `Unit of assessment name`,
              uoa_number = `Unit of assessment number`,
              profile = Profile,
              fte = `FTE Category A staff submitted`,
              `4*`, `3*`, `2*`, `1*`, unclassified) %>%
    dplyr::filter(if (!missing(number)) uoa_number == number else TRUE,
                  if (!missing(name)) uoa_name == name else TRUE,
                  if (!missing(grep)) grepl(grep, uoa_name, ...))
}

#' List the most popular journals
#'
#' Enumerate the journals, books and reports with the most REF submissions.
#' Any volumes with fewer than `min_articles` submissions will be aggregated
#' into a category called 'Other'.
#'
#' @param outputs data from \code{\link{cluster_outputs_by_journals}}
#' @param min_articles 'top' journals must have at least this many articles each
#'
#' @import dplyr
#'
#' @examples
#' math_outputs <- subset(tidy_outputs(), uoa_name == 'Mathematical Sciences')
#' math_outputs <- cluster_outputs_by_journals(math_outputs)
#' get_top_journals(math_outputs)
#'
#' @export
get_top_journals <- function(outputs, min_articles = 20) {
  na_journal_ids <- filter(outputs, is.na(volume_std)) %>%
    distinct(journal_id)

  journal_names <- outputs %>%
    dplyr::select(journal_id, volume_title) %>%
    distinct(journal_id, .keep_all = TRUE)

  outputs %>%
    count(journal_id) %>%
    anti_join(na_journal_ids, by = 'journal_id') %>%
    arrange(desc(n)) %>%
    dplyr::filter(n >= min_articles) %>%
    left_join(journal_names, by = 'journal_id')
}
