library(citationSearch)
library(usethis)

blm = citationSearch::blm_get_metadata_all()

write.csv(blm, "blm.csv")
usethis::use_data(blm, overwrite=TRUE) 
