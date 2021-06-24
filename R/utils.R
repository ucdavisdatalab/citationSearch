normalize_string = function(x) {
    tolower(gsub("[[:punct:][:blank:][:digit:]]+", " ", x))
}

check_doi = function(x) {
grepl('/^10.\\d{4,9}/[-._;()/:A-Z0-9]+$/i', x)
}
