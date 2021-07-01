# Exported functions 
# search_construct_query
# search_collection 

preprocess_anystyle_entry = function(anystyle_entry) {

    normalize_string = function(string) {
	# remove all " \r \n \t
	string = stringr::str_remove_all(string, '"')
	string = stringr::str_remove_all(string, '\r')
	string = stringr::str_remove_all(string, '\n')
	string = stringr::str_remove_all(string, '\t')
    }

    anystyle_entry = anystyle_entry[ , c("author", "date", "title", "publisher", "doi" )]

    # for each field except author take only the first entry
    anystyle_entry$date = anystyle_entry$date[[1]][1]
    anystyle_entry$publisher = anystyle_entry$publisher[[1]][1]
    anystyle_entry$doi = anystyle_entry$doi[[1]][1]
    anystyle_entry$title = anystyle_entry$title[[1]][1]

    # for author get only the unique rows from the dataframe
    unique_authors = unique(anystyle_entry$author[[1]])
    anystyle_entry$author = list(unique_authors)

    # remove potentially problematic characters
    anystyle_entry$title = normalize_string(anystyle_entry$title)
    anystyle_entry$publisher = normalize_string(anystyle_entry$publisher)

    # convert to year naively
    anystyle_entry$date = substr(anystyle_entry$date, 1, 4)
    

    anystyle_entry
}


#' Convert Anystyle item to a search query string
#' takes row, returns string
#'
#' @param anystyle_entry single row from anystyle output
#' @return string suitable for solr search
#' @import stringr
#' @export
search_construct_query = function(anystyle_entry) {

    add_field_identifier = function (field, fieldname) {
	if (is.null(field) | any(is.na(field))) {
	    return("")
	}

	field = trimws(field)

	if (nchar(field) == 0) {
	    return("")
	}

	field = strsplit(field, " ")[[1]]
	field = na.omit(field)
	field = paste0(fieldname, field)
	paste0(field, collapse=" ")
    }

    query_string = ""

    anystyle_entry = preprocess_anystyle_entry(anystyle_entry)

    authors = unlist(anystyle_entry$author)
    if (length(authors) > 0) {
	authors = paste0("Authors:", authors)
	authors = paste0(authors, collapse=" ")
    } else {
	authors = ""
    }

    publisher = add_field_identifier(anystyle_entry$publisher, "Publisher:")
    title = add_field_identifier(anystyle_entry$title, "Title:")
    year = add_field_identifier(anystyle_entry$date, "Year:")
    doi = add_field_identifier(anystyle_entry$doi, "DOI:")

    query_string = paste(authors, year, title, doi, publisher)
}


#' Given Anystyle row, get top matches
#'
#' @param collection_name one of blm,treesearch,nfsl
#' @param query_string created with construct_query function
#' @param conn optional SolrClient object
#' @return dataframe of top 5 results
#' @import solrium
#' @export
#'
search_collection = function(collection_name, query_string, conn=NULL) {
    if (is.null(conn)) {
	conn = solrium::SolrClient$new()
    }

    results = solrium::solr_search(
				   conn,
				   name= collection_name,
				   params = list(
						 q = query_string,
						 fl = c("Title", "Authors", "Publisher", "Year", "DOI", "score"),
						 rows = 5)
    )
}
