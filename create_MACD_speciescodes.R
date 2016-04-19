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
  
  number_species = nrow(unique_species)
  species_codes = c(1:number_species)
  unique_species = cbind(unique_species, species_codes)
  return(unique_species)
}

MACD_species = make_species_table_from_MACD()

AMNIOTE_DB = read.csv("Data/Amniote_Database_Aug_2015.csv")
AMNIOTE_DB[AMNIOTE_DB == -999] <- NA
AMNIOTE_DB = filter(AMNIOTE_DB, adult_body_mass_g > 0)

MACD_AMNIOTE_common_rows <- (AMNIOTE_DB$genus %in% MACD_species$genus 
                                & AMNIOTE_DB$species %in% MACD_species$species)
MACD_AMNIOTE_common_species <- AMNIOTE_DB[MACD_AMNIOTE_common_rows,]


