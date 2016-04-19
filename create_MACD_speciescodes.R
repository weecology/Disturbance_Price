library(DBI)
library(RSQLite)

query_MACD = function(query){
  MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query_output <- dbGetQuery(conn, query)
}

make_species_table_from_MACD = function(){
  genus_species_col = "SELECT DISTINCT family, genus, species FROM community_analysis_data"
  unique_species = query_MACD(genus_species_col)
  
  number_species = nrow(unique_species)
  species_codes = c(1:number_species)
  unique_species = cbind(unique_species, species_codes)
  return(unique_species)
}

MACD_species = make_species_table_from_MACD()




