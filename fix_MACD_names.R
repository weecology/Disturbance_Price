####  FIX SPECIES NAMES TYPOS IN MACD SQLITE DATABASE   ####

library(DBI)
library(RSQLite)

query_MACD = function(query){
  MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query_output <- dbGetQuery(conn, query)
}

query1 = "update community_analysis_data set species = LTRIM(RTRIM(species))"
query2 = "update community_analysis_data set genus = LTRIM(RTRIM(genus))"
query3= "UPDATE community_analysis_data SET species='cooperii' WHERE 
species= 'cooperi'"
query_MACD(query1)
query_MACD(query2)
query_MACD(query3)
