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
  dbDisconnect(conn)
  return(query_output)
}

make_MACD_species_table = function(){
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
      value = as.character("NO VALUE")
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

combine_AMNIOTE_MACD_tables = function(AMNIOTE, MACD){
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
  MACD_AMNIOTE_rawmerge$Family = as.character(make_merged_family_column(MACD_AMNIOTE_rawmerge))
  MACD_AMNIOTE_rawmerge = select(MACD_AMNIOTE_rawmerge, c(-family.x, -family.y))
  MACD_AMNIOTE_rawmerge = MACD_AMNIOTE_rawmerge[c(1,6,2:5)]
  MACD_AMNIOTE_rawmerge = add_class_info(MACD_AMNIOTE_rawmerge)
  return(MACD_AMNIOTE_rawmerge)
}

median_weights = function(){
  # Opens Missing_species_weights.csv and calculates each species median weight
  #
  #  Args:
  #    None
  #
  #  Important Internal Variables:
  #      filename and path for Missing_species_weights.csv are embedded in function
  #
  #  Returns:
  #    MACD_AMNIOTE_rawmerge: dataframe containing taxonomic info and weights 
  #       from AMNIOTE and family from either MACD or AMNIOTE if present for
  #       each unique species in MACD
  missing_wgts = read.csv("Data/Missing_species_weights.csv", stringsAsFactors = FALSE)
  output = missing_wgts %>% group_by(genus, species) %>%
    summarise(median=median(adult_body_mass_g)) %>%
    as.data.frame()
  return(output)
}

add_missing_masses_to_species_table = function(old_masses){
  # Adds weights for species that did not have weights in AMNIOTE
  #
  #  Args:
  #    old_masses: dataframe containing the merge from AMNIOTE and MACD
  #
  #  Returns:
  #    data: dataframe of species names and weights with weights from AMNIOTE and
  #          additional weights for species not found in AMNIOTE added from the 
  #          file Missing_species_weights.csv
  new_masses = median_weights()
  data = left_join(old_masses, new_masses, by = c("genus","species"))
  data = data %>% 
          mutate(mass = ifelse(!is.na(adult_body_mass_g), adult_body_mass_g, median)) %>% 
          select(c(-adult_body_mass_g, -median)) 
  return(data)
}

insert_species_table_into_MACD = function(new_table){
  conn <- dbConnect(SQLite(), "MACD.sqlite")
  dbWriteTable(conn, 'species_table', new_table, overwrite = TRUE)
  dbDisconnect(conn)
}


MACD_species = make_MACD_species_table()
AMNIOTE_species = make_AMNIOTE_species_table()
species_table = combine_AMNIOTE_MACD_tables(AMNIOTE_species, MACD_species)
species_table = add_missing_masses_to_species_table(species_table)
insert_species_table_into_MACD(species_table)

