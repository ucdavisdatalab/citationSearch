#' Normalize a string
#'
#' Normalizes a string by setting to lowercase, filtering out punctation and digits
#' @param x string to normalize
#' @return normalized string
#' @export
normalize_string = function(x) {
    if (is.null(x)) {
	return (NULL)
    }
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


#' Check to see if user has the number of matching columns specified
#' 
#' 
#' @param cols char colnames to check
#' @param expected char names to check against
#' @param return bool
validate_columns = function (cols, expected) {
    ms = setdiff(expected, tolower(cols))
    if (length(ms) > 0) {
        stop(paste("missing the following columns:", paste(ms, collapse=", ")))
    }
    return (TRUE)
}
