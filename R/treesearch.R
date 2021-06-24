# Exported Functions 
# treesearch_web_query
# treesearch_get_metadata_one
# treesearch_get_metadata_all

#' Perform a search on Treesearch's online interface
#' 
#' @param keywords string
#' @param authorlastname string
#' @param yearfrom string
#' @param yearto string
#' @param station string
#' @param series string
#' @param volume string
#' @param treesearch_url string optional argument
#' @return dataframe title,id,text for top 20 results (or all if fields blank). 
#' @examples
#' treesearch_web_query(keywords = "increment-borer methods fire history", 
#'                  authorlastname <- "Arno")
#' @export
treesearch_web_query <- function(keywords = "", authorlastname = "", yearfrom = "", 
                             yearto = "", station = "", series = "", 
                             volume = "", treesearch_url = 
				 "https://www.fs.usda.gov/treesearch/search?"){
    
    # Return dataframe of search results (id, title, text)
    parse_result <- function(result_div) {
        title <-  rvest::html_node(result_div, ".title") %>% rvest::html_text()
        text <-  rvest::html_text(result_div)
        
        get_treesearchid = function(result_div) { 
            title_div <- rvest::html_node(result_div, ".title")
            treesearchid <- strsplit(rvest::html_attr(rvest::html_node(title_div, "a"), 
                                               "href"), "/")[[1]][2]
        }
        
        # get treesearch id from html node
        treesearchid <- get_treesearchid(result_div)
        
        data.frame(
            title = title, 
            treesearchid = treesearchid, 
            text = text, 
            stringsAsFactors = FALSE
        )
    }
    
    # Construct url for nth page of search results
    get_page <- function(page_num) {
        # construct url
        cat("getting page", page_num, "\n")
        resp <- httr::RETRY("GET", url = treesearch_url,
                      query = list(
                          keywords = keywords,
                          authorlastname = authorlastname,
                          yearfrom = yearfrom,
                          yearto = yearto,
                          station = station,
                          series = series,
                          volume = volume,
                          page = page_num
                      )
        )
        
        # parse results
        content <- xml2::read_html(httr::content(resp, as="text", encoding="UTF-8"))
        results <- rvest::html_nodes(content, ".search-result")
        parsed <- lapply(results, parse_result)
        
        # return dataframe
        do.call(rbind, parsed)
    }
    

    # if no query fields get info for all docs in treesearch
    fields <- c(keywords, authorlastname, yearfrom, yearto, station, 
                series, volume)
    if (all(fields == "")) {
      
      # get the total number of pages on the site
      total_page <- function(){
        resp <- httr::RETRY("GET", url = treesearch_url, 
                      query = list(
                        keywords = keywords,
                        authorlastname = authorlastname,
                        yearfrom = yearfrom,
                        yearto = yearto,
                        station = station,
                        series = series,
                        volume = volume
                      )
        )
        
        # parse results
        content <- xml2::read_html(httr::content(resp, as="text", encoding="UTF-8"))
        pager <- rvest::html_nodes(content, ".pager")
        page_div <- rvest::html_node(pager, ".pager-last")
        pageid <- strsplit(rvest::html_attr(rvest::html_node(page_div, "a"), "href"), "&page=")[[1]][2]
        
        # return the number of pages
        return(as.numeric(pageid))
      }
      
          res <- lapply(1:(total_page()), get_page) # hardcoded for now
        df <- do.call("rbind", res)
        return(df)
    }
    
    get_page(1)
}

#' Given Treesearch ID return metadata dataframe
#' 
#' @param treesearch_id treesearch id number
#' @param treesearch_doc_url string optional argument
#' @examples
#' treesearch_get_metadata_one(28951)
#' @export
treesearch_get_metadata_one <- function(treesearch_id,
treesearch_doc_url = "https://data.fs.usda.gov/research/westernstubservices/productsservicesentry?"
			     ) {
    # Given a treesearch id returns response content
    get_treesearch_document <- function(treesearch_id) {
        resp <- httr::RETRY("GET", url = treesearch_doc_url,
                      query = list(
                          product_id = "",
                          treesearch_id = treesearch_id
                      ))
        httr::content(resp, as = "text", encoding = "UTF-8")
    }
    
    # Returns treesearch metadata for an xml2 document object
    parse_treesearch_xml <- function(response) {
        doc <- tryCatch({
          xml2::read_xml(response)
        }, error = function(e) return(NULL)) 
        
        
        get_field <- function(xpath) {
          tryCatch({xml2::xml_text(xml2::xml_find_all(doc, xpath))
          }, error = function(e) {})
        }
        
        tryCatch({
          data.frame(
            treesearch_id = get_field("//treesearch_pub_id")[1],
            title = get_field("//title")[1],
            authors = get_field("//authors_listing"),
            year = get_field("//product_year"),
            source = get_field("//pub_publication"),
            pub_series = get_field("//pub_type_desc"),
            doi = get_field("//doi"),
            description = get_field("//abstract"),
            citation = get_field("//citation"),
            keywords = get_field("//keywords"),
            stringsAsFactors = FALSE
          )},
          error = function(e) {})
    }
    response <- get_treesearch_document(treesearch_id)
    parse_treesearch_xml(response)
}

#' Returns dataframe containing metadata for all items in treesearch catalog
#
#' @export
treesearch_get_metadata_all = function() {
    res <- treesearch_web_query() # blank fields returns all ids
    docs <- lapply(res$treesearchid, treesearch_get_metadata_one)
    treesearch_all <- data.frame(do.call("rbind", docs))
}
