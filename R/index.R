
#' Helper function to detect if two sets of string vectors EQUAL to each other
#'
#'
#' @param string a set of strings 
#' @param pattern the format to search against the string
#' @return a list of bool 
#' @importFrom purrr map
rdetect = function(string, pattern){
  string %>% purrr::map(function(s){
    s == pattern
  })
}

#' Check to see if user has the number of matching columns specified
#' 
#' 
#' @param df dataframe to validate if it has expected column names
#' @importFrom purrr map flatten_lgl
isValidColumns = function (df){
  format = c("title", "authors", "year", "publisher", "source", "misc", "journal title", "doi")
  return (colnames(df) %>% tolower() %>% rdetect(., format) %>% 
            purrr::map(any) %>% purrr::flatten_lgl() %>% sum == length(format))
}


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

#' Preprocess a collection 
#' 
#' 
#' @param collection a dataframe used for indexing
#' @return tibble of the processed data
preprocess_data = function (collection){
  colnames(collection) = tolower(colnames(collection))
  collection = collection[ , c("title", "authors", "year", "publisher", "source", "misc", "journal title", "doi")]
  colnames(collection) = c("Title", "Authors", "Year", 
                           "Publisher", "Source", "Journal", "MiscID", "DOI")
  
  collection = collection %>% tibble()
  #parse year
  collection$Year = substr(anytime::anydate(collection$Year), 0, 4) %>% 
    replace_na("0") %>% as.numeric()
  
  collection$Year[collection$Year>=2025] <- 0
  collection$Year[collection$Year<=1800] <- 0
  
  #Combine Athours and Publisher as an additional column
  collection$Combined = create_combined(collection$Authors, collection$Publisher)
  return (collection)
}

#' Create collection for index
#'
#'
#' @param collection a dataframe used for indexing
#' @description The collection passed in must have the following specified columns:
#' Title, Authors, Year, Publisher", Source, Misc, Journal Title, DOI
#' @export 
#' @importFrom solrium SolrClient collection_exists collection_create

index_collection = function (collection){
  if (!isValidColumns(collection)){
    warning("'Some columns did not match\n")
    return(-1)
  }
  conn = solrium::SolrClient$new()
  
  #make the name of the dataframe be the collection name
  collection_name = paste0(substitute(collection))
  
  if (solrium::collection_exists(conn, collection_name)){
    warning("\"", collection_name, "\"", " collection already exits, not overwriting\n")
    return (-1)
  }
  data = preprocess_data(collection)
  collection_create(conn, name = collection_name , numShards = 1)
  conn$add(data, collection_name)
}
