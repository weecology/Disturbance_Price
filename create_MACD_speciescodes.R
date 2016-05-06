library(DBI)
library(RSQLite)
library(dplyr)

query_MACD = function(query){
  MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query_output <- dbGetQuery(conn, query)
}

make_species_table_from_MACD = function(){
  genus_species_col = "SELECT DISTINCT family, genus, species FROM community_analysis_data"
  unique_species = query_MACD(genus_species_col)
  clean_whitespace = as.data.frame(apply(unique_species,2,function(x)gsub('\\s+', '',x)),
                                   stringsAsFactors=FALSE)
  number_species = nrow(unique_species)
  species_codes = c(1:number_species)
  unique_species = cbind(unique_species, species_codes)
  unique_species = unique_species %>% distinct(genus, species)
  return(unique_species)
}

make_AMNIOTE_species_table = function(){
  AMNIOTE_DB = read.csv("Data/Amniote_Database_Aug_2015.csv", stringsAsFactors = FALSE)
  AMNIOTE_DB = AMNIOTE_DB %>% select(class, family, genus, species, adult_body_mass_g)
  AMNIOTE_DB = filter(AMNIOTE_DB, adult_body_mass_g > 0)
  clean_AMNIOTE = as.data.frame(apply(AMNIOTE_DB,2,function(x)gsub('\\s+', '',x)), stringsAsFactors = FALSE)
}

merge_AMNIOTE_MACD = function(AMNIOTE, MACD){
  unique_species_wgt = left_join(MACD, AMNIOTE, by = c("genus","species"))
  unique_species_wgt = unique_species_wgt[c(5,6,1,2,3,4,7)]
  return(unique_species_wgt)
}

MACD_species = make_species_table_from_MACD()
AMNIOTE_species = make_AMNIOTE_species_table()
MACD_AMNIOTE_rawmerge = merge_AMNIOTE_MACD(AMNIOTE_species, MACD_species)

unique_species_wgt = left_join(MACD_species, AMNIOTE_species, by = c("genus","species"))
unique_species_wgt = unique_species_wgt[c(5,6,1,2,3,4,7)]

for (row in 1:nrow(changes.file)) {
  
if is.na(unique_species_wgt$family.x)