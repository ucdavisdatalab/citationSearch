# Exported Functions:
# index_nfsl_data
# index_blm_data
# index_treesearch_data
# index_all
#
# FIELDS IN AN INDEX:
# Authors, Year, Title, DOI, Publisher, Combined (Authors + Publisher), 
# MiscID, Source
# 
#

#' Create Combined column for authors and publishers
#'
#'
#' @param authors character vector
#' @param publisher character vector
create_combined = function(authors, publishers) {
    authors[authors == "NULL"] <- ""
    publishers[publishers == "NULL"] <- ""
    combined = paste(authors, publishers)
    combined = trimws(combined)
}


#' Preprocess NFSL data
#'
#' @param nfsl_data dataframe of nfsl record metadata
preprocess_nfsl_data = function(nfsl_data) {
    # remove unneded columns and rename to match index fields
    nfsl_data = nfsl_data[ ,c("title", "authors", "year",  
			      "publisher", "nfsl_id") ]
    colnames(nfsl_data) = c("Title", "Authors", "Year",
			    "Publisher", "MiscID")

    # preprocess nfsl_data
    nfsl_data$Year = substr(nfsl_data$Year, 1,4)
    nfsl_data$Year[nfsl_data$Year=="NULL"] <- "0"
    nfsl_data$Year = as.numeric(nfsl_data$Year)
    nfsl_data$Year[nfsl_data$Year>=2025] <- 0
    nfsl_data$Year[nfsl_data$Year<=1800] <- 0
    nfsl_data$Year = trimws(nfsl_data$Year)
    nfsl_data$Year = as.numeric(nfsl_data$Year)

    # add missing fields (Combined, DOI) make sure to not add NULL somehow
    nfsl_data$Combined = create_combined(nfsl_data$Authors, nfsl_data$Publisher)
    nfsl_data$DOI = NA
    nfsl_data$Source = "NFSL"
    return (nfsl_data)
}

#' Create index from nfsl dataset
#'
#'
#' @param data optional dataframe of nfsl data
#' @param conn optional solrclient connection object
#' @param overwrite optional bool delete and recreate collection if exists 
#' @export
#' @import solrium
index_nfsl_data = function(data = NULL, conn = NULL, overwrite=FALSE) {

    if (is.null(data)) {
	data = nfsl # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "nfsl")) { 
	if (overwrite) {
	    collection_delete(conn, name="nfsl")
	} else {
	    warning("'nfsl' Collection already exits, not overwriting\n")
	    return()
	}
    }

    data = preprocess_nfsl_data(data)

    # create collection (also called index)
    collection_create(conn, name="nfsl", numShards = 1)

    # add documents
    conn$add(data, "nfsl")

    return()
}

#' Preprocess BLM data
#'
#' @param blm_data dataframe of blm record metadata
preprocess_blm_data = function(blm_data) {
    # remove unneded columns and rename to match index fields
    blm_data = blm_data[ ,c("title", "authors", "year",  
			    "publisher", "identifier") ]
    colnames(blm_data) = c("Title", "Authors", "Year", 
			   "Publisher", "MiscID")

    blm_data$Year = substr(blm_data$Year, 0, 4)
    blm_data$Year = as.numeric(blm_data$Year)

    # add missing fields (Combined, DOI)
    blm_data$Combined = create_combined(blm_data$Authors, blm_data$Publisher)
    blm_data$DOI = NA
    blm_data$Source ="BLM"
    return(blm_data)
}

#' Create index from blm dataset
#'
#'
#' @param data optional dataframe of nfsl data
#' @param conn optional solrclient connection object
#' @param conn overwrite bool delete and recreate collection if exists
#' @export
#' @import solrium
index_blm_data = function(data = NULL, conn = NULL, overwrite=FALSE) {

    if (is.null(data)) {
	data = blm # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "blm")) { 
	if (overwrite) {
	    collection_delete(conn, name="blm")
	} else {
	    warning("'blm' Collection already exits, not overwriting\n")
	    return()
	}
    }

    data = preprocess_blm_data(data)

    # create collection (also called index)
    collection_create(conn, name="blm", numShards = 1)

    # add documents
    conn$add(data, "blm")

    return()
}

#' Preprocess Treesearch data
#'
#' @param treesearch_data dataframe of nfsl record metadata
preprocess_treesearch_data = function(treesearch_data) {

    treesearch_data = treesearch_data[ ,c("title", "authors", "year", 
					  "source", "treesearch_id", "doi") ]

    colnames(treesearch_data) = c("Title", "Authors", "Year", 
				  "Publisher", "MiscID", "DOI")

    # add missing fields (Combined)
    treesearch_data$Combined = create_combined(treesearch_data$Authors, 
					       treesearch_data$Publisher)
    treesearch_data$Source = "Treesearch"

    return(treesearch_data)
}

#' Create index from Treesearch dataset
#'
#'
#' @param data optional dataframe of treesearch data
#' @param conn optional solrclient connection object
#' @param overwrite optional bool delete and recreate collection if exists
#' @export
#' @import solrium
index_treesearch_data = function(data = NULL, conn = NULL, overwrite=FALSE) {

    if (is.null(data)) {
	data = treesearch # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "treesearch")) { 
	if (overwrite) {
	    collection_delete(conn, name="treesearch")
	} else {
	    warning("'treesearch' Collection already exits, not overwriting\n")
	    return()
	}
    }

    data = preprocess_treesearch_data(data)

    # create collection (also called index)
    collection_create(conn, name="treesearch", numShards = 1)

    # add documents
    conn$add(data, "treesearch")

    return()
}

#' Create collection containing nfsl, blm and treesearch datasets
#'
#' @param nfsl optional dataframe of nfsl record metadata
#' @param blm optional dataframe of blm record metadata
#' @param treesearch optional dataframe of treesearch record metadata
#' @param conn optional solrclient connection object
#' @param overwrite optional bool delete and recreate collection if exists
#' @export 
#' @import solrium
index_all = function(nfsl_data= NULL, blm_data=NULL, treesearch_data=NULL,
		     conn=NULL, overwrite=FALSE) {

    if (is.null(nfsl_data)) {
	nfsl_data = nfsl # loads from package data
    }

    if (is.null(blm_data)) {
	blm_data = blm # loads from package data
    }

    if (is.null(treesearch_data)) {
	treesearch_data = treesearch # loads from package data
    }

    if (is.null(conn)) {
	# use the default connection info
	conn = solrium::SolrClient$new()
    }

    if (collection_exists(conn, "all")) { 
	if (overwrite) {
	    collection_delete(conn, name="all")
	} else {
	    warning("'all' Collection already exits, not overwriting\n")
	    return()
	}
    }

    nfsl_data = preprocess_nfsl_data(nfsl_data)
    blm_data = preprocess_blm_data(blm_data)
    treesearch_data = preprocess_treesearch_data(treesearch_data)
    all_data = rbind(nfsl_data, blm_data, treesearch_data)

    # create collection (also called index)
    collection_create(conn, name="all", numShards = 1)

    # add documents
    conn$add(all_data, "all")

    return()
}
