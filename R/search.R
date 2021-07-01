# Exported functions 
# search_construct_query
# search_collection 


#' Convert Anystyle item to a search query string
#' takes row, returns string
#'
#' @param anystyle_entry single row from anystyle output
#' @return string suitable for solr search
#' @import stringr
#' @export
search_construct_query = function(anystyle_entry) {

    # simplify
    anystyle_entry = anystyle_entry[ , c("author", "date", "title", "publisher", "doi" )]

    normalize_year = function(anystyle_date) {
	y = unlist(anystyle_date)
	y = unlist(lapply(y, strsplit, "-"))
	y = y[nchar(y) == 4]
	ux <- unique(y)
	ux[which.max(tabulate(match(y, ux)))]
    }

    normalize_string = function(string) {
	# remove all " \r \n \t
	string = stringr::str_remove_all(string, '"')
	string = stringr::str_remove_all(string, '\r')
	string = stringr::str_remove_all(string, '\n')
	string = stringr::str_remove_all(string, '\t')
    }

    query_string = ""

    # author -> Authors
    authors = unlist(anystyle_entry$author)
    authors = authors[!is.na(authors)]
    if (length(authors) > 0) {
    authors = paste0("Authors:", authors)
    authors = paste0(authors, collapse=" ")
    } else {
	authors = ""
    }

    # date -> Year
    year = as.numeric(normalize_year(anystyle_entry$date))
    if (is.null(year)) {
	year = ""
    } else {
        year = paste0("Year:", year)
    }

    # title -> Title
    title = unlist(anystyle_entry$title) 
    if ( nchar(trimws(title)) > 0) {
    title = paste0(title, collapse=" ")
    title = strsplit(title, " ")[[1]]
    title = paste0("Title:", title)
    title = paste0(title, collapse=" ")
    } else {
	title = ""
    }

    # doi -> DOI
    doi = unlist(anystyle_entry$doi)
    doi = doi[[1]]
    if (is.null(doi)) {
	doi = ""
    } else {
	doi = paste0("DOI", publisher)
    }

    # publisher -> Publisher
    publisher = unlist(anystyle_entry$publisher)
    publisher = paste0(publisher, collapse=" ")

    if ( nchar(trimws(publisher)) > 0) {
        publisher = strsplit(publisher, " ")[[1]]
        publisher = paste0("Publisher:", publisher)
        publisher = paste0(publisher, collapse=" ")
    } else {
	publisher =""
    }

    # query string
    query_string = paste(authors, year, title, doi, publisher)
    query_string = normalize_string(query_string)
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
