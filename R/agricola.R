# Exported Functions 
# agricola_query
# agricola_parse

#' Querying both NAL Agricola databases (journal articles and other materials) and parsing the search results.
#' 
#' @param rem_dr remote driver
#' @param title string
#' @param author string
#' @param keyword string
#' @param isbn string
#' @param language string
#' @param pub string
#' @param series string
#' @param subject string
#' @param source string
#' @param agricola_search_url string optional argument
#' @return dataframe of title and id links for top results (or all if fields blank). 
#' @examples
#' 
#' library(RSelenium)
#' rem_dr <- remoteDriver(port = 4445L)
#' rem_dr$open(silent = TRUE)
#' 
#' agricola_query(rem_dr, title = "Biological soil crusts: Ecology and management", author = "Jayne Belnap")
#'
#' @export
#' 
## A script for querying both NAL Agricola databases (journal articles and 
## other materials) and parsing the search results.

# Query agricola with the given search parameters
# Takes in an RSelenium remote driver object
# Accepts: title, author, keyword, isbn, language, publisher, series,
#          subject, source
# Note: The search can use at most three fields. If more are supplied, the
# function will ignore them.
# Returns df of titles and links
# If no args returns df of all 8532 docs in the library
agricola_query <- function(rem_dr, title = NULL, author = NULL, keyword = NULL,
                           isbn = NULL, language = NULL, pub = NULL,
                           series = NULL, subject = NULL, source = NULL, 
                           agricola_search_url = "https://agricola.nal.usda.gov/vwebv/search") {
  
  # Construct agricola api request url from search parameters
  agricola_url <- function(call) {
    normalize_string = function(x) {
      tolower(gsub("[[:punct:][:blank:][:digit:]]+", " ", x))
    }
    
    # get the first three function arguments and clean
    clean <- function(x) gsub(" ", "+", normalize_string(x))
    params <- lapply(call[2:4], clean)
    
    # search codes for constructing query strings
    codes <- list(
      title = "TKEY",
      author = "NKEY",
      keyword = "GKEY",
      isbn = "NUMS",
      language = "546A",
      pub = "946A",
      series = "SERI",
      subject = "SKEY",
      source = "773",
      default = "NKEY"
    )
    
    # construct url
    httr::modify_url(agricola_search_url,
               query = list(
                 searchArg1 = I(params[[1]]),
                 argType1 = "all",
                 searchCode1 = codes[[names(params)[1]]],
                 combine2 = "and",
                 searchArg2 = I(params[[2]]),
                 argType2 = "all",
                 searchCode2 = codes[[names(params)[2]]],
                 combine3 = "and",
                 searchArg3 = I(params[[3]]),
                 argType3 = "all",
                 searchCode3 = codes[[names(params)[3]]],
                 recCount = 10000,
                 searchType = 2,
                 page.search.search.button = "Search"
               ))
  }
  
  # just pass search params to build url
  call <- match.call()
  call[[1]] <- NULL
  # call[[2]] <- NULL 
  url <- agricola_url(call)
  cat("querying with url: ", url)
  
  # search both citation and cataloging dbs
  rem_dr$navigate("https://agricola.nal.usda.gov/vwebv/selectDatabase.do?dbCode=AGRI2DB&dbCode=LOCAL")
  rem_dr$navigate(url = url)
  
  Sys.sleep(1)
  
  # if survey pops up continue
  try({
    elem <- rem_dr$findElement("css", "#cfi_btnNoThanks")
    elem$clickElement()
  }, silent = TRUE) 
  
  Sys.sleep(15)
  
  # parse search page
  res <-tryCatch({
    # multiple search results
    elems <- rem_dr$findElements("css", "a[href^=\"holdingsInfo\"")
    links <- unlist(lapply(elems, 
                           function(x) x$getElementAttribute("href")))
    
    # throw an error if only one search result
    if (length(links) < 1)
      stop()
    
    links <- gsub("holdingsInfo", "fullHoldingsInfo", links)
    elems <- rem_dr$findElements("css", ".line1Link > a")
    titles <- unlist(lapply(elems, 
        function(x) x$getElementText()))
    data.frame(links, titles, stringsAsFactors = FALSE)
    },
    error = function(e) {
    # # one search result (redirected to brief record page)
      elem <- rem_dr$findElement("css", "a[href^=\"fullHoldingsInfo\"")
      link <- elem$getElementAttribute("href")
      elem <- rem_dr$findElement("css", ".subfieldData")
      title <- elem$getElementText()
      data.frame(link, title, stringsAsFactors = FALSE)
    })
  
  # return df of search results
  colnames(res) <- c("link", "title")
  res
}


#' Given agricola link return metadata dataframe
#' 
#' @param agricola_link string
#' @return metadata dataframe
#' @examples 
#' res <- agricola_query(title = "Biological soil crusts: Ecology and management", author = "Jayne Belnap")
#' lapply(res$link, agricola_parse)
#'
#' @import dplyr
#' @export

# Given an agricola link return a metadata list
agricola_parse <- function(agricola_link) {
  # get html
  rem_dr$navigate(url = agricola_link)
  doc <- xml2::read_html(as.character(rem_dr$getPageSource()))
  nodes <- rvest::html_nodes(doc, 
                      "ul[title='Bibliographic Record Display'] > .bibTag")
  nodes <- nodes[1:length(nodes)-1] # exclude holdings information
  
  # parse all available fields into a list
  res <- list()
  get_field <- function(node) {
    field <- rvest::html_nodes(node, ".fieldLabelSpan") %>% rvest::html_text()
    field <- gsub("[[:punct:]]", "", field)
    field <- gsub("-|_|\\s", ".", tolower(field))
    val <- rvest::html_nodes(node, ".subfieldData") %>% rvest::html_text()
    val <- gsub(".\n\\s+", "; ", val)
    res[[field]] <<- val
  }
  invisible(lapply(nodes, get_field))
  
  res
}

#' Returns dataframe containing metadata for items in Agricola catalog
#' 
#' @param res agricola query 
#' @return metadata dataframe
#' 
#' @export
agricola_get_metadata = function(res) {
  docs <- lapply(res$link, agricola_parse)
  agricola_all <- data.frame(do.call("rbind", docs))
}
