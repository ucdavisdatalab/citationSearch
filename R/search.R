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
