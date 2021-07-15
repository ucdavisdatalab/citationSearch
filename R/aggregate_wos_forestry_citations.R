##### NOTE THAT THE WEB OF SCIENCE API STUFF IS ALL SCREWED UP
##### AND THE MICROSOFT ACADEMIC KNOWLEDGE SYSTEM HAS A QUERY LIMIT AND NO EASY PAY-FOR-PLAY

### SO, THIS IS BRUTE FORCE -- I DOWNLOADED TOP JOURNAL RECORDS FROM THE WEB OF SCIENCE ONLINE PORTAL
### THIS READS IT IN, AND AGGREGATES

require(data.table)
cit_list = lapply(list.files('~/Downloads/',pattern = 'savedrecs.*xls',full.names = T),readxl::read_excel)
cit_dt = rbindlist(cit_list,fill = T,use.names = T)
cit_dt = cit_dt[,.(DOI,`Publication Year`,Title,Authors,`Article Title`,`Source Title`,Volume,Issue,`Start Page`,`End Page`)]

cit_dt = cit_dt[!is.na(DOI),]
cit_dt = cit_dt[!duplicated(cit_dt),]
saveRDS(cit_dt,'data/wos_forestry.rds')




# 
# source('~/Documents/microsoft_key')
# Sys.setenv(MICROSOFT_ACADEMIC_KEY = MICROSOFT_ACADEMIC_KEY)
# qu = "And(Y>=2005,DJN == 'Journal of Forestry'"
# qu = "J.JN = 'Journal of Forestry'"
# ma_search(query = qu,10)
# 
# 
# library(scholar)
# library(rvest)
# forest_journals = 'https://scholar.google.com/citations?view_op=top_venues&hl=en&vq=bio_forestsforestry'
# 
# journal_names = read_html(forest_journals) %>% html_nodes('td.gsc_mvt_t') %>% html_text(trim = T)
# journal_names = tolower(journal_names)
# 
# search_grid = expand.grid(journal = journal_names,year = year)
# 
# call_ms_query = function(journal,year){
#   microdemic::ma_evaluate(query = paste0("And(Y=",year,",Composite(J.JN=='",journal,"'))"),count = 500,atts = c('DOI','Y','AA.DAuN','DN','JJN','V','I','FP'))
# }
# 
# res = mapply(FUN = call_ms_query,journal = search_grid$journal,year = search_grid$year) 
# 
# 
# all_res = rbindlist(res,use.names= T,fill = T)
