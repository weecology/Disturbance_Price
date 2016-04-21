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
  return(unique_species)
}

make_AMNIOTE_species_table = function(){
  AMNIOTE_DB = read.csv("Data/Amniote_Database_Aug_2015.csv", stringsAsFactors = FALSE)
  AMNIOTE_DB = AMNIOTE_DB %>% select(genus, species, adult_body_mass_g)
  AMNIOTE_DB = filter(AMNIOTE_DB, adult_body_mass_g > 0)
  clean_AMNIOTE = as.data.frame(apply(AMNIOTE_DB,2,function(x)gsub('\\s+', '',x)), stringsAsFactors = FALSE)
  }

MACD_species = make_species_table_from_MACD()
AMNIOTE_species = make_AMNIOTE_species_table()

MACD_AMNIOTE_wgt = left_join(MACD_species, AMNIOTE_species, by = c("genus","species"))
Missing_wgts = MACD_AMNIOTE_wgt[is.na(MACD_AMNIOTE_wgt$adult_body_mass_g),]

family_data = read.csv("Merged_AMNIOTE_MACD.csv")
Missing.w.class = left_join(Missing_wgts, family_data, by = "species_codes")
Missing.w.class = Missing.w.class %>% select(Class, family.x, genus.x,
                                             species.x, species_codes,
                                             adult_body_mass_g.x)
names(Missing.w.class) = c("Class", "Family", "Genus", "Species", 
                           "Species_codes", "Adult_mass_g")