# Exported Functions 
# nfsl_query
# nfsl_parse
# nfsl_get_metadata_all()

#' Querying the National Forest Service Library and parsing the search results.
#' 
#' @param title string
#' @param creators string
#' @param publisher string
#' @param contributors string
#' @param date string
#' @param identifier string
#' @param source string
#' @return dataframe of title and id links for top results (or all if fields blank). 
#' @examples
#' nfsl_query(creators = c("arno", "tomback"))
#'                  
#' @import data.table
#' @import jsonlite
#' @export

## A script for querying the National Forest Service Library and parsing
## the search results.

# Query NFSL with the given search parameters
# Accepts: title, creators, publisher, contributors, date, identifier, source
#   Arguments can be character string or vector of character strings
# Returns df of titles and links
# If no args returns df of all 12339 docs in the library
nfsl_query <- function(title = NULL, creators = NULL, publisher = NULL, 
                       contributors = NULL, date = NULL, identifier = NULL, 
                       source = NULL) {
  
  # Given an api request url make request and return json
  nfsl_get_page <- function(url) {
    resp <- httr::RETRY("GET", url = url) # retry with exponential backoff
    json <- httr::content(resp, as = "text", encoding = "UTF-8")
    jsonlite::parse_json(json)
  }
  
  # Given an api request url make request and return ids of search results
  nfsl_get_results <- function(url) {
    num_results <- nfsl_get_page(url)$totalResults
    num_pages <- ceiling(num_results / 10) # ten results per page
    
    get_page <- function(n) {
      # read json
      doc <- nfsl_get_page(paste0(url, "/page/", n))
      
      # list of identifier links and titles
      links <- sapply(doc$items, function(x) x$itemLink)
      links <- sapply(strsplit(links, split = "collection/"), "[[", 2)
      titles <- sapply(doc$items, function(x) x$metadataFields[[1]]$value)
      
      # return df of search results
      data.frame(
        id = links,
        title = titles,
        stringsAsFactors = FALSE
      )
    }
    
    data.frame(do.call("rbind", lapply(1:num_pages, get_page)))
  }
  
  # get function arguments
  fields <- names(formals(sys.function(sys.parent(n = 1))))
  
  # construct a list for each type of url segment
  searchterm_list <- unlist(lapply(fields, function(x) get(x)))
  field_list <- unlist(lapply(fields, function(x) rep(strtrim(x, 6), 
                                                      length(get(x)))))
  
  get_type_list <- function(val)
    type_list <- paste(rep(val, length(searchterm_list)), collapse = "!")
  mode_list <- get_type_list("all")
  conn_list <- get_type_list("and")
  
  # paste lists into url
  construct_seg <- function(name, seg_list)
    paste0(name, "/", paste(seg_list, collapse = "!"), "/")
  url <- URLencode(paste0(
    nfsl_api_url = "https://nfsl.contentdm.oclc.org/digital/api/search/",
    construct_seg("searchterm", searchterm_list),
    construct_seg("field", field_list),
    construct_seg("mode", mode_list),
    construct_seg("conn", conn_list),
    "order/nosort/ad/asc"
  ))
  
  nfsl_get_results(url)
}


#' Given NFSL ID return metadata dataframe
#' 
#' @param nfsl_id string nfsl id link
#' @param nfsl_doc_url string optional argument
#' @return metadata dataframe
#' @examples
#' nfsl_parse("p17053coll2/id/310050")
#' @export
#' 
# Given a nfsl id string return a metadata dataframe
nfsl_parse <- function(nfsl_id, 
                       nfsl_doc_url = "https://nfsl.contentdm.oclc.org/digital/api/collections/") {
  # get json data
  resp <- httr::RETRY("GET", 
                url = paste0(nfsl_doc_url, gsub("id", "items", nfsl_id), 
                             "/false"))
  cont <- httr::content(resp, encoding = "UTF-8")
  
  fields <- cont$parent$fields
  if (!is.list(fields))
    return(list())
  
  # combine in a data.table where key is field name
  meta <- data.table::data.table(do.call("rbind", fields))
  meta <- meta[, key := as.character(key)]
  data.table::setkey(meta, "key")
  get_field <- function(x) as.character(meta[.(x)]$value)
  
  data.frame(
    nfsl_id = nfsl_id,
    title = get_field("title"),
    authors = get_field("creato"),
    year = get_field("date"),
    description = get_field("descri"),
    publisher = get_field("publis"),
    place = get_field("place"),
    forest = get_field("forest"),
    reportnumber = get_field("report"),
    contributors = get_field("contri"),
    datecreated = get_field("dmcreated"),
    stringsAsFactors = FALSE
  )
}

#' Returns dataframe containing metadata for all items in NFSL catalog
#
#' @export
nfsl_get_metadata_all = function() {
  res <- nfsl_query()
  docs <- lapply(res$id, nfsl_parse)
  nfsl_all <- data.frame(do.call("rbind", docs))
}

