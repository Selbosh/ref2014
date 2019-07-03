#' Group
#'
#' @examples
#' # Not done yet. Need to reconcile column names.
#'
#' @note
#' The current implementation is not 100% safe, as there is the possibility that
#' row IDs become large enough to be confused with ISSNs. We can fix this with
#' the addition of a non-numeric prefix to row ids.
#'
#' @importFrom dplyr %>% filter mutate rename row_number select
#' @importFrom igraph graph_from_data_frame clusters membership
#' @importFrom tidyr gather
#'
#' @return
#' The input data frame, with an added column \code{journal_id} assigning each
#' submission to a group representing a journal (or book). Any submissions that
#' could not be assigned to a group will have \code{journal_id = NA}.
#'
#' @export
cluster_outputs_by_journals <- function(data) {
  data <- dplyr::mutate(data, row_id = row_number())

  long_tbl <- data %>%
    dplyr::select(row_id, doi, volume_std, issn, isbn) %>%
    tidyr::gather('identifier', 'value', -row_id) %>%
    dplyr::filter(!is.na(value))

  connected_components <- long_tbl %>%
    dplyr::select(-identifier) %>%
    igraph::graph_from_data_frame() %>%
    igraph::clusters() %>% igraph::membership() %>% stack %>%
    dplyr::rename(journal_id = values, row_id = ind)

  merge(data, connected_components, by = 'row_id', all.x = TRUE)
}

#' Convert a title to lower case, and remove spaces, punctuation and accents
#' @param title A character vector of volume titles
#' @return A character vector, the same length as the input
#' @importFrom dplyr %>%
standardise_volume_title <- function(title) {
  title %>%
    tolower %>%
    gsub(pattern = '[^[:alnum:]]', replacement = '') %>%
    gsub(pattern = '^the', replacement = '') %>%
    iconv(to = 'ASCII//TRANSLIT')
}
