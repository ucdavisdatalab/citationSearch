library(citationSearch)
library(usethis)

# takes about 2 hours
nfsl = citationSearch::nfsl_get_metadata_all()

write.csv(nfsl, "nfsl.csv")
usethis::use_data(nfsl, overwrite=TRUE) 
