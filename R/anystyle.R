# still struggles with particles, and NA fields...
unique_authors = function(authors_df) {
    if (is.null(authors_df)) {
	return (NULL)
    }

    if (length(authors_df) == 0) {
	return (NULL)
    }

    if (min(dim(authors_df)) > 1) {
	combined = apply( authors_df[ , colnames(authors_df)]  , 1 , paste , collapse = " ")
	combined = unique(combined)
	paste(combined, collapse=",")
    }
}
