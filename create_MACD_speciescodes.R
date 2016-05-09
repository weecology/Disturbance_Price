# Create a species table with taxonomic information and body size for all species
# in the MACD database

library(DBI)
library(RSQLite)
library(dplyr)

query_MACD = function(query){
  #  Submits query to SQLITE database
  #  
  #  Args:
  #    query: string containing SQL query
  #
  #  Returns:
  #    query_output: result from SQL query of database
  #
  #  Notes: Database is fixed to MACD.sqlite because it is the only SQLite
  #    database being used in this project. Will need to change if another
  #    SQLite database is addded.
  
  MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query_output <- dbGetQuery(conn, query)
}

make_species_table_from_MACD = function(){
  #  Extracts list of unique species from MACD
  #  
  #  Args:
  #    none passed to function
  #    
  #  Important Internal Variables:
  #      genus_species_col: string constaining SQL query
  #
  #  Returns:
  #    unique_species: dataframe containing taxonomic info for each unique species
  

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
  #  Extracts taxonomic and weigt info from AMNIOTE for all species in AMNIOTE
  #  
  #  Args:
  #    none passed to function
  #    
  #  Important Internal Variables:
  #      filename and path for AMNIOTE are included in read.csv call
  #
  #  Returns:
  #    clean_AMNIOTE: dataframe containing a stripped down version of AMNIOTE
  
  AMNIOTE_DB = read.csv("Data/Amniote_Database_Aug_2015.csv", stringsAsFactors = FALSE)
  AMNIOTE_DB = AMNIOTE_DB %>% select(class, family, genus, species, adult_body_mass_g)
  AMNIOTE_DB = filter(AMNIOTE_DB, adult_body_mass_g > 0)
  clean_AMNIOTE = as.data.frame(apply(AMNIOTE_DB,2,function(x)gsub('\\s+', '',x)), stringsAsFactors = FALSE)
}

merge_AMNIOTE_MACD = function(AMNIOTE, MACD){
  #  Merges AMNIOTE and MACD species info
  #  
  #  Args:
  #    AMNIOTE: dataframe generated from make_AMNIOTE_species_table function
  #    MACD: dataframe generated from make_species_table_from_MACD function
  #    
  #  Returns:
  #    unique_species_wgt: dataframe containing data from both AMNIOTE and MACD for
  #       each unique species in MACD. This datafarme contains 2 family columns (one
  #       from AMNIOTE and one from MACD)
  unique_species_wgt = left_join(MACD, AMNIOTE, by = c("genus","species"))
  unique_species_wgt = unique_species_wgt[c(5,6,1,2,3,4,7)]
  return(unique_species_wgt)
}

make_merged_family_column = function(data){
  # reduces two family columns to one, with preference for AMNIOTE family
  #
  #  Args:
  #    data: dataframe generated from merge_AMNIOTE_MACD
  #    
  #  Returns:
  #    merged_family: dataframe containing data from both AMNIOTE and MACD for
  #       each unique species in MACD. Only 1 family column in this version.
  merged_family = c()
  for (row in 1:nrow(data)) {
    if ((is.na(data$family.y[row])) & 
        (data$family.x[row] == "NULL")){
      value = as.character("No FAMILY")
    } else if ((is.na(data$family.y[row]))){
      value = as.character(data$family.x[row])
    } else {
      value = as.character(data$family.y[row])
    }
    merged_family = rbind(merged_family, value)
  }
  return(merged_family)
}

add_class_info = function(data){
  # Add taxonomic class to species table
  #
  #  Args:
  #    AMNIOTE: dataframe generated from make_AMNIOTE_species_table function
  #    MACD: dataframe generated from make_species_table_from_MACD function
  #    
  #  Returns:
  #    MACD_AMNIOTE_rawmerge: dataframe containing taxonomic info and weights 
  #       from AMNIOTE and family from either MACD or AMNIOTE if present for
  #       each unique species in MACD
  changes.file = read.csv("Data/MACD_namechanges.csv", stringsAsFactors = FALSE)
  
  for (row in 1:nrow(changes.file)) {
    if (changes.file$New_type[row] == "class"){
      data$class = replace(data$class, data$Family == changes.file$Old_genus[row], 
                                    changes.file$New_name1[row])
    }
  }
  return(data)
}

make_species_table = function(AMNIOTE, MACD){
  # Master function making baseline species table with taxonomic info and weight
  #
  #  Args:
  #    AMNIOTE: dataframe generated from make_AMNIOTE_species_table function
  #    MACD: dataframe generated from make_species_table_from_MACD function
  #    
  #  Returns:
  #    MACD_AMNIOTE_rawmerge: dataframe containing taxonomic info and weights 
  #       from AMNIOTE and family from either MACD or AMNIOTE if present for
  #       each unique species in MACD
  MACD_AMNIOTE_rawmerge = merge_AMNIOTE_MACD(AMNIOTE, MACD)
  MACD_AMNIOTE_rawmerge$Family = make_merged_family_column(MACD_AMNIOTE_rawmerge)
  MACD_AMNIOTE_rawmerge = select(MACD_AMNIOTE_rawmerge, c(-family.x, -family.y))
  MACD_AMNIOTE_rawmerge = MACD_AMNIOTE_rawmerge[c(1,6,2:5)]
  MACD_AMNIOTE_rawmerge = add_class_info(MACD_AMNIOTE_rawmerge)
  return(MACD_AMNIOTE_rawmerge)
}

median_weights = function(){
  
  missing_wgts = read.csv("Data/Missing_species_weights.csv", stringsAsFactors = FALSE)
  grouped_species = group_by(missing_wgts, Genus, Species)
  species_medians = summarise(grouped_species, median=median(Avg_Est))
  return(species_medians)
}


MACD_species = make_species_table_from_MACD()
AMNIOTE_species = make_AMNIOTE_species_table()
species_table = make_species_table(AMNIOTE_species, MACD_species)
new_weights = median_weights()







