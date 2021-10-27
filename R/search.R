# Exported functions 
# search_collection 

#' Given search query, get top matches
#'
#' @param collection_name string to search against
#' @param query_string created with construct_query function
#' @param conn optional SolrClient object
#' @return dataframe of top 5 results
#' @importFrom solrium SolrClient solr_search
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
						 fl = c("Title", "Authors", 
							"Publisher", "Year", 
							"DOI", "Source",
							"score"),
						 rows = 5)
    )
}

#' Convert dataframe of citations into search queries
#'
#'
#' @return search queries
create_queries = function(citations) {

    # check input data
    valid = validate_columns(colnames(citations),
		     expected=c("title", "authors", "year", "publisher", 
				"doi", "journal_title"))

    # for each string field:
    # normalize string
    sfn = c("title", "authors", "publisher", "doi", "journal_title")
    citations[sfn] = lapply(citations[sfn], normalize_string)

    # for year field
    # confirm either NA or 4 digit year
    # throw error
    citations["year"] = as.numeric(citations["year"]) 
    if (any(citations["year"] < 1800 || citations["year"] > 2022)) {
	stop()
    }

    # for title, add phrase

    # for rest add field identifiers

    # paste together

    # return just queries
}


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

#' Convert Anystyle item to a search query string
#' takes row, returns string
#'
#' @param anystyle_entry single row from anystyle output
#' @return string suitable for solr search
#' @export
search_construct_query = function(anystyle_entry) {

    query_string = ""

    anystyle_entry = preprocess_anystyle_entry(anystyle_entry)

    authors = unlist(anystyle_entry$author)
    if (length(authors) > 0) {
    	authors = paste0("Authors:", authors)
    	authors = paste0(authors, collapse=" ")
    } else {
	    authors = ""
    }
    
    journalTitle = add_field_identifier(anystyle_entry$`container-title`, 'Journal:')
    publisher = add_field_identifier(anystyle_entry$publisher, "Publisher:")
    # Want phrases for the title instead of word
    title = ifelse(is.null(t1$title[[1]]), "Title:", paste0("Title:",t1$title))
    year = add_field_identifier(anystyle_entry$date, "Year:")
    doi = add_field_identifier(anystyle_entry$doi, "DOI:")

    query_string = paste(authors, year, title, doi, publisher, journalTitle)
}
