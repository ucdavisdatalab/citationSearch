# Exported functions
# blm_query
# blm_parse
# blm_get_metadata 
# blm_get_metadata_all

# A script for querying the Bureau of Land Management Library and parsing the search results.

#' Query BLM library with the given search parameters
#'
#' @param query string, no vector
#' @param years string
#' @param mediatypes string
#' @param subjects string
#' @param creators string
#' @param languages string
#'
#' @return dataframe title, link
#' @export
#'
#' @examples
#' blm_query(query = "wildlife", subjects = c("Wildlife conservation", "land"))
blm_query <- function(query = NULL, years = NULL, mediatypes = NULL, 
                      subjects = NULL, creators = NULL, languages = NULL) {
  
  # Request page and parse out reference links
  get_page <- function(url) {
    print(url)
    # get page
    resp <- httr::RETRY("GET", url = url)
    cont <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_html(cont)
    nodes <- rvest::html_nodes(doc, ".C234 a")
    
    # parse page
    get_info <- function(node) {
      list(
        link = rvest::html_attr(node, "href"),
        title = rvest::html_attr(node, "title")
      )
    }
    
    res <- lapply(nodes, get_info)
    as.data.frame(do.call("rbind", res))
  }
  
  # Construct Bureau of Land Management api request url from search params
  blm_url <- function(call) {
    # construct each segment of the url
    construct_seg <- function(field) {
      if (field == "query")
        return(sprintf("%s&sin=", call[[field]]))
      
      tmp <- sapply(eval(call[[field]]), function(x) {
        sprintf("%s:\"%s\"", substr(field, 1, nchar(field)-1), x)
      })
      paste(tmp, collapse = "&and[]=")
    }
    
    # return url
    tmp <- lapply(names(call), construct_seg)
    url <- paste0(c("https://archive.org/details/blmlibrary?", unlist(tmp)), collapse = "&and[]=")
    gsub(" ", "+", url)
  }
  
  # build request url
  call <- as.list(match.call())
  
  # get info for all 8532 docs in the Bureau of Land Management Library
  #if (length(call) == 1) {
  #    urls <- paste0("https://archive.org/details/blmlibrary?&sort=-downloads&page=", 1:BLM_TOTAL_PAGES)
  #    res <- lapply(urls, get_page)
  #    df <- do.call("rbind", res)
  #    return(df)
  #}
  
  call[[1]] <- NULL
  url <- blm_url(call)
  
  # return df of search results
  get_page(url)
}

#' Given a BLM ID string return a metadata dataframe row
#'
#' @param string 
#'
#' @return
#' @importFrom stringr %>%
#' @export
blm_parse <- function(row) {
  print(row[["link"]])
  
  # get html
  resp <- httr::RETRY("GET", url = paste0("https://archive.org", row[["link"]]))
  doc <- httr::content(resp, encoding = "UTF-8")
  nodes <- rvest::html_nodes(doc, ".metadata-definition")
  
  # parse all available fields into a list
  res <- list()
  res[["title"]] <- row[["title"]]
  get_field <- function(node) {
    field <- rvest::html_nodes(node, "dt") %>% rvest::html_text()
    field <- gsub("-|_|\\s", ".", tolower(field))
    
    if (field == "by")
      field <- "authors"
    
    val <- rvest::html_nodes(node, "dd") %>% rvest::html_text()
    val <- stringr::str_trim(val)  # trim extra whitespace
    
    res[[field]] <<- val
  }
  invisible(lapply(nodes, get_field))
  
  data.frame(res, stringsAsFactors = FALSE)
}

#' Returns dataframe containing metadata for all items in BLM catalog
#'  
#' @export
blm_get_metadata_all <- function() {
  isCursor = TRUE
  url = "https://archive.org/services/search/v1/scrape?fields=title&q=collection%3Ablmlibrary"
  df = data.frame(identifier=character(),
                  title=character(),
                  stringsAsFactors=FALSE)
  
  while (isCursor) {
    response = httr::GET(url)
    data = httr::content(response, "text")
    json = jsonlite::fromJSON(data)
    results = json$items
    df = rbind(df, results)
    
    test = json$previous
    if (length(test) != 0) {
      isCursor = FALSE
      break
    } else {
      cursor = json$cursor
      url = paste0(
        "https://archive.org/services/search/v1/scrape?fields=title&q=collection%3Ablmlibrary&cursor=",
        cursor)
    }
  }
  names(df)[names(df) == 'identifier'] <- 'link'
  df$link = paste0('/details/', df$link)
  docs = apply(df, 1, blm_parse)
  blm_all = data.frame(plyr::rbind.fill(docs))
  
  return(blm_all)
}


