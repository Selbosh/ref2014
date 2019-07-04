#' Units of Assessment
#'
#' The names of the subject-based units of assessment (UoAs) in the 2014 REF.
#' \enumerate{
#' \item Clinical Medicine
#' \item Public Health, Health Services and Primary Care
#' \item Allied Health Professions, Dentistry, Nursing and Pharmacy
#' \item Psychology, Psychiatry and Neuroscience
#' \item Biological Sciences
#' \item Agriculture, Veterinary and Food Science
#' \item Earth Systems and Environmental Sciences
#' \item Chemistry
#' \item Physics
#' \item Mathematical Sciences
#' \item Computer Science and Informatics
#' \item Aeronautical, Mechanical, Chemical and Manufacturing Engineering
#' \item Electrical and Electronic Engineering, Metallurgy and Materials
#' \item Civil and Construction Engineering
#' \item General Engineering
#' \item Architecture, Built Environment and Planning
#' \item Geography, Environmental Studies and Archaeology
#' \item Economics and Econometrics
#' \item Business and Management Studies
#' \item Law
#' \item Politics and International Studies
#' \item Social Work and Social Policy
#' \item Sociology
#' \item Anthropology and Development Studies
#' \item Education
#' \item Sport and Exercise Sciences, Leisure and Tourism
#' \item Area Studies
#' \item Modern Languages and Linguistics
#' \item English Language and Literature
#' \item History
#' \item Classics
#' \item Philosophy
#' \item Theology and Religious Studies
#' \item Art and Design: History, Practice and Theory
#' \item Music, Drama, Dance and Performing Arts
#' \item Communication, Cultural and Media Studies, Library and Information Management
#' }
#'
#' @format A character vector with 36 elements, giving the names of the UoAs
#' @source \url{https://www.ref.ac.uk/2014/}
'UoA'

#' Research Excellence Framework 2014 results
#'
#' This is a large data set and could possibly do with slimming down
#' @format A tibble with 190,962 rows and 31 columns
#' @source \url{https://results.ref.ac.uk/DownloadResults}
'outputs'

#' Research output (REF2) submission data
#' @format A tibble with 7,644 rows and 16 columns
#' @source \url{https://results.ref.ac.uk/DownloadSubmissions}
'results'

#' Tidy the output data
#'
#' This is a utility function that performs three useful tasks on a REF outputs
#' dataset, making it more amenable to later analysis.
#' \itemize{
#' \item coerce column names to snake case (replacing spaces with underscores)
#' \item standardize ISBNs and ISSNs by removing hyphens and spaces
#' \item add a \code{volume_std} column representing a standardised journal/volume title
#' }
#'
#' @param outputs A dataset of REF outputs. Defaults to whole REF2014 dataset
#'
#' @return
#' The \code{outputs} data with column names and serial numbers sanitized
#'
#' @examples
#' # Extract Economics submissions data
#' econ <- grep('^Economics', UoA)
#' subset(tidy_outputs(outputs), uoa_number == econ)
#'
#' @seealso \code{\link{standardise_volume_title}}
#'
#' @importFrom dplyr %>% transmute
#'
#' @export
tidy_outputs <- function(outputs = ref2014::outputs) {
  ref2014::outputs %>%
    transmute(institution = `Institution name`,
              uoa_name = `Unit of assessment name`,
              uoa_number = `Unit of assessment number`,
              output_type = `Output type`,
              title = Title, doi = DOI,
              volume_title = `Volume title`,
              volume_std = ref2014:::standardise_volume_title(volume_title),
              issn = gsub('[^[:alnum:]]', '', ISSN),
              isbn = gsub('[^[:alnum:]]', '', ISBN),
              research_group = `Research group`)
}
