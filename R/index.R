# Exported Functions:
# index_nfsl_data
# index_blm_data
# index_treesearch_data
#
# FIELDS IN AN INDEX:
# Authors, Year, Title, DOI, LCCN, Publisher, Keywords, Description, Combined, MiscID
# 
#

#' Create index from nfsl dataset
#'
#'
#' @param data optional dataframe of nfsl data
#' @param conn optional solrclient connection object
#' @export
#' @import solrium
index_nfsl_data = function(data = NULL, conn = NULL) {

    if (is.null(data)) {
	data = nfsl # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "nfsl")) {
	warning("'nfsl' Collection already exits, not overwriting\n")
	return()
    }

    # remove unneded columns and rename to match index fields
    data = data[ ,c("title", "authors", "year", "description", "publisher", "nfsl_id") ]
    colnames(data) = c("Title", "Authors", "Year", "Description", "Publisher", "MiscID")

    # preprocess data
    data$Year = substr(data$Year, 1,4)
    data$Year[data$Year=="NULL"] <- "0"
    data$Year = as.numeric(data$Year)
    data$Year[data$Year>=2025] <- 0
    data$Year[data$Year<=1800] <- 0

    # add missing fields (Combined, Keywords, DOI, LCCN)
    data$Combined = paste(data$Authors, data$Title, data$Publisher)
    data$Keywords = NA
    data$DOI = NA
    data$LCCN = NA

    # create collection (also called index)
    collection_create(conn, name="nfsl", numShards = 1)

    # add documents
    conn$add(data, "nfsl")

    return()
}

#' Create index from blm dataset
#'
#'
#' @param data optional dataframe of nfsl data
#' @param conn optional solrclient connection object
#' @export
#' @import solrium
index_blm_data = function(data = NULL, conn = NULL) {

    if (is.null(data)) {
	data = blm # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "blm")) {
	warning("'treesearch' Collection already exits, not overwriting\n")
	return()
    }

    # remove unneded columns and rename to match index fields
    data = data[ ,c("title", "authors", "year", "topics", "publisher", "identifier", "lccn") ]
    colnames(data) = c("Title", "Authors", "Year", "Keywords", "Publisher", "MiscID", "LCCN")

    # add missing fields (Combined, Description, DOI)
    data$Combined = paste(data$Authors, data$Title, data$Publisher)
    data$Description = NA
    data$DOI = NA


    # create collection (also called index)
    collection_create(conn, name="blm", numShards = 1)

    # add documents
    conn$add(data, "blm")

    return()
}

#' Create index from Treesearch dataset
#'
#'
#' @param data optional dataframe of treesearch data
#' @param conn optional solrclient connection object
#' @export
#' @import solrium
index_treesearch_data = function(data = NULL, conn = NULL) {

    if (is.null(data)) {
	data = treesearch # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "treesearch")) {
	warning("'treesearch' Collection already exits, not overwriting\n")
	return()
    }

    data = data[ ,c("title", "authors", "year", "keywords", "description", "source", "treesearch_id", "doi") ]
    colnames(data) = c("Title", "Authors", "Year", "Keywords", "Description", "Publisher", "MiscID", "DOI")

    # add missing fields (Combined, LCCN)
    data$Combined = paste(data$Authors, data$Title, data$Publisher)
    data$LCCN = NA

    # create collection (also called index)
    collection_create(conn, name="treesearch", numShards = 1)

    # add documents
    conn$add(data, "treesearch")

    return()
}
