#' Download data from the REF 2014 web site
#'
#' This helper function will download and unzip an Excel (\code{.xlsx}) file
#' containing either the full results or the \emph{Research outputs (REF2)}
#' submission data for every institution and every unit of assessment in the
#' Research Excellence Framework 2014.
#'
#' Alternatively you can download the data yourself manually from the
#' \href{https://results.ref.ac.uk/Results}{REF 2014 web site}.
#' Once downloaded, call \code{\link[readxl]{read_excel}} to import the data
#' from the spreadsheet into R.
#'
#' @note
#' Unfortunately there is no way to read Excel files from an \code{\link{unz}}
#' connection, so it is necessary to save the file to disk. However, if you
#' leave the \code{destination} to the default \code{tempdir()} then you should
#' not need to worry too much about cleaning up the \code{.xlsx} files
#' afterwards.
#'
#' @param type Either 'outputs' or 'results'
#' @param destination Optional directory in which to store the downloaded
#' \code{.xlsx} file. If not supplied, it will be saved to a temporary directory
#'
#' @return Path to the downloaded/unzipped \code{.xlsx} file of REF outputs
#'
#' @seealso \code{\link[readxl]{read_excel}}, to import downloaded data into R
#'
#' @export
download_REF <- function(type = c('results', 'outputs'),
                         destination = tempdir()) {
  type <- match.arg(type)
  if (type == 'results') {
    url <- 'https://results.ref.ac.uk/DownloadFile/AllResults/xlsx'
    temp <- tempfile(fileext = '.xlsx', tmpdir = destination)
    download.file(url, temp, mode = 'wb')
    return(temp)
  }
  url <- 'https://results.ref.ac.uk/DownloadFile/Form/REF2/excelwithnames'
  temp <- tempfile(fileext = '.zip')
  on.exit(unlink(temp))
  download.file(url, temp, mode = 'wb')
  unzip(temp, exdir = destination)
}

#' Download submissions data from the REF 2014 web site
#'
#' Use this function to download an Excel spreadsheet from the REF 2014 web site
#' describing institutional submissions and results for a particular unit of
#' assessment (UoA). See Details. You probably don't want this function---it's
#' more efficient to use \code{\link{download_REF}('outputs')} instead.
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
download_submissions <- function(uoa = 1:36, destination = '.') {
  if (any(uoa < 1 | uoa > 36 | round(uoa) != uoa))
    stop('Units of assessment (UoAs) must be integers between 1 and 36')
  if (any(duplicated(uoa)))
    warning('Ignoring duplicate UoAs: ', paste(uoa[duplicated(uoa)]))
  uoa <- unique(uoa)
  file_list <- vector(length = length(uoas))
  for (i in uoa)
    file_list[i] <- download_UoA(i, destination)
  file_list
}

download_UoA <- function(uoa, destination = tempdir()) {
  temp <- tempfile(fileext = '.zip')
  on.exit(unlink(temp))
  url <- 'https://results.ref.ac.uk/DownloadFile/UnitOfAssessment'
  download.file(paste(url, uoa, 'excelwithnames', sep = '/'), temp, mode = 'wb')
  xls_filename <- grep('\\.xlsx$', unzip(temp, list = TRUE)$Name, value = TRUE)
  unzip(temp, files = xls_filename, exdir = destination)
}
