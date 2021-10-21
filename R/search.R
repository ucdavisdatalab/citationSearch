# Exported functions 
# search_construct_query
# search_collection 

#' Preprocess an anystyle entry so that it can be searched with
#'
#' @param anystyle_entry dataframe
#' @return dataframe
#' @importFrom stringr str_remove_all
preprocess_anystyle_entry = function(anystyle_entry) {

    normalize_string = function(string) {
      	# remove all " \r \n \t
    	string = stringr::str_remove_all(string, '"')
    	string = stringr::str_remove_all(string, '\r')
    	string = stringr::str_remove_all(string, '\n')
    	string = stringr::str_remove_all(string, '\t')
  
  	  # remove punctuation
  	  string = stringr::str_remove_all(string, "[[:punct:]]")
    }

    anystyle_entry = anystyle_entry[ , c("author", "date", "title", "publisher", "container-title", "doi" )]

    # for each field except author take only the first entry
    anystyle_entry$date = anystyle_entry$date[[1]][1]
    anystyle_entry$publisher = anystyle_entry$publisher[[1]][1]
    anystyle_entry$doi = anystyle_entry$doi[[1]][1]
    anystyle_entry$title = anystyle_entry$title[[1]][1]
    anystyle_entry$`container-title` = anystyle_entry$`container-title`[[1]][1]

    # for author get only the unique rows from the dataframe
    unique_authors = unique(anystyle_entry$author[[1]])
    anystyle_entry$author = list(unique_authors)

    # remove potentially problematic characters
    anystyle_entry$title = normalize_string(anystyle_entry$title)
    anystyle_entry$publisher = normalize_string(anystyle_entry$publisher)
    anystyle_entry$`container-title` = normalize_string(anystyle_entry$`container-title`)

    # convert to year naively
    anystyle_entry$date = substr(anystyle_entry$date, 1, 4)
    
    anystyle_entry
}


#' Convert Anystyle item to a search query string
#' takes row, returns string
#'
#' @param anystyle_entry single row from anystyle output
#' @return string suitable for solr search
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
    
    journalTitle = add_field_identifier(anystyle_entry$`container-title`, 'Journal:')
    publisher = add_field_identifier(anystyle_entry$publisher, "Publisher:")
    # Want phrases for the title instead of word
    title = ifelse(is.null(t1$title[[1]]), "Title:", paste0("Title:",t1$title))
    year = add_field_identifier(anystyle_entry$date, "Year:")
    doi = add_field_identifier(anystyle_entry$doi, "DOI:")

    query_string = paste(authors, year, title, doi, publisher, journalTitle)
}


#' Given Anystyle row, get top matches
#'
#' @param collection_name one of blm,treesearch,nfsl
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
