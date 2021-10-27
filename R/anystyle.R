# still struggles with particles, and NA fields...
unique_authors = function(authors_df) {
    if (is.null(authors_df) | length(authors_df) == 0){ 
	return (NULL)
    }

    # combine across the rows
    if (ncol(authors_df) > 1)  {
	combined = apply(authors_df[ ,colnames(authors_df)], 1, paste, 
			 collapse = " ")
    } else {
	combined = authors_df[,1]
    } # ncol(authors_df) == 1

    # get only unique combined rows
    combined = unique(combined)

    # paste together rows
    return (paste(combined, collapse=" "))
}

#' Convert anystyle data to the input format expected for searches
#'
#' @param anystyle_entry dataframe
#' @return dataframe
parse_anystyle = function(anystyle_df) {
    # fix columns
    a = anystyle_df[, c("title", "author", "date", "publisher", 
			"collection-title", "doi")]
    colnames(a)[[2]] = "authors"
    colnames(a)[[3]] = "year"
    colnames(a)[[5]] = "journal_title"

    # For now using only the first entry in each field
    # anystyle seems to split the fields into multiple entries based on
    # formatting on the pdf itself. Could mean that we change this in the 
    # future if we notice that the fields should be combined
    a[-c(2)] = lapply(a[-c(2)], function(x) {as.character(lapply(x, "[[", 1))})
    a$authors = as.character(lapply(a$authors, unique_authors))

    a
}
