#' Normalize a string
#'
#' Normalizes a string by setting to lowercase, filtering out punctation and digits
#' @param x string to normalize
#' @return normalized string
#' @export
normalize_string = function(x) {
    tolower(gsub("[[:punct:][:blank:][:digit:]]+", " ", x))
}

#' Check to see if a string is potentially a valid DOI
#'
#' @param x string input to see if doi
#' @return bool
#' @export
check_doi = function(x) {
    grepl('/^10.\\d{4,9}/[-._;()/:A-Z0-9]+$/i', x)
}
