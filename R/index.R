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
#' @importFrom tibble tibble
#' @importFrom anytime anydate
preprocess_data = function (collection){
  colnames(collection) = tolower(colnames(collection))
  collection = collection[ , c("title", "authors", "year", "publisher", "source", "misc", "journal title", "doi")]
  colnames(collection) = c("Title", "Authors", "Year", 
                           "Publisher", "Source", "Journal", "MiscID", "DOI")
  
  collection = collection %>% tibble::tibble()
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
#' @param records a dataframe to be indexed
#' @description The collection passed in must have the following specified columns:
#' Title, Authors, Year, Publisher", Source, Misc, Journal Title, DOI
#' @export 
#' @importFrom solrium SolrClient collection_exists collection_create
index_records = function (records, name=paste0(substitute(records))){
  validate_columns(colnames(collection), c("title", "authors", "year", "publisher", "doi", "journal_title", "source", "miscid"))
  conn = solrium::SolrClient$new()
  
  if (solrium::collection_exists(conn, collection_name)){
    warning("\"", collection_name, "\"", " collection already exits, not overwriting\n")
    return (-1)
  }
  data = preprocess_data(records)
  collection_create(conn, name = collection_name , numShards = 1)
  conn$add(data, collection_name)
}
