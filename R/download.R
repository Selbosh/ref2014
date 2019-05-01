#' Download data from the REF 2014 web site
#'
#' Use this function to download an Excel spreadsheet from the REF 2014 web site
#' describing institutional submissions and results for a particular unit of
#' assessment (UoA). See Details.
#'
#' A unit of assessment is an academic subject area, enumerated as follows.
#' See \href{https://results.ref.ac.uk/DownloadSubmissions/SelectUoa}{online}
#' for more information.
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
#' @param uoa integer vector of unit(s) of assessment, from 1--36. Downloads all UoAs by default
#' @param destination directory in which to store the Excel spreadsheet(s).
#' Uses the current working directory by default
#'
#' @return
#' A character vector of file paths to the downloaded \code{.xlsx} spreadsheets
#' @export
download_uoa <- function(uoa = 1:36, destination = '.') {
  if (any(uoa < 1 | uoa > 36 | round(uoa) != uoa))
    stop('Units of assessment (UoAs) must be integers between 1 and 36')
  if (any(duplicated(uoa)))
    warning('Ignoring duplicate UoAs: ', paste(uoa[duplicated(uoa)]))
  uoa <- unique(uoa)
  file_list <- vector(length = length(uoas))
  for (i in uoa)
    file_list[i] <- download_single_uoa(i, destination)
  file_list
}

download_single_uoa <- function(uoa, destination = tempdir()) {
  temp <- tempfile(fileext = '.zip')
  on.exit(unlink(temp))
  url <- 'https://results.ref.ac.uk/DownloadFile/UnitOfAssessment'
  download.file(paste(url, uoa, 'excelwithnames', sep = '/'), temp, mode = 'wb')
  xls_filename <- grep('\\.xlsx$', unzip(temp, list = TRUE)$Name, value = TRUE)
  unzip(temp, files = xls_filename, exdir = destination)
}


#' Import REF data from an Excel spreadsheet
#'
#' Extract data on institutional outputs and results from files downloaded from
#' the REF 2014 web site. Each file should correspond to a particular unit of
#' assessment (UoA). If you don't have the files already on your computer, you
#' can download them using \code{\link{download_uoa}}.
#'
#' This is a light wrapper around \code{\link[readxl]{read_excel}}.
#'
#' @param filename Path to the Excel spreadsheet
#'
#' @seealso \code{\link{download_uoa}}, to download the spreadsheet files
#'
#' @importFrom readxl read_excel
#'
#' @export
read_outputs <- function(filename) {
  readxl::read_excel(filename, sheet = 'Outputs', skip = 4)
}

#' @rdname read_outputs
read_profiles <- function(filename) {
  readxl::read_excel(filename, sheet = 'Profiles', skip = 4)
}
