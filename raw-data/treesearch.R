library(citationSearch)
library(usethis)

treesearch = citationSearch::treesearch_get_metadata_all()

write.csv(treesearch, "treesearch.csv")
usethis::use_data(treesearch, overwrite=TRUE) 
