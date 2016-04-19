library(DBI)
library(RSQLite)

query_MACD = function(query){
  MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query_output <- dbGetQuery(conn, query)
}

genus_species_col = "SELECT genus, species FROM community_analysis_data"
all_species_names = query_MACD(genus_species_col)

