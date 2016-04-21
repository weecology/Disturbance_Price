####  FIX SPECIES NAMES TYPOS IN MACD SQLITE DATABASE   ####

library(DBI)
library(RSQLite)

query_MACD = function(query){
  
  MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query_output <- dbGetQuery(conn, query)
}

remove_whitespace = function(){
  query1 = "update community_analysis_data set species = LTRIM(RTRIM(species))"
  query2 = "update community_analysis_data set genus = LTRIM(RTRIM(genus))"
  query_MACD(query1)
  query_MACD(query2)
}

correct_names = function(){
  
changes_file = read.csv("Data/MACD_namechanges.csv", stringsAsFactors = FALSE)

for (row in 1:nrow(changes_file)){
  if (changes_file$New_type[row] == "genus"){
    make_query = paste0("UPDATE community_analysis_data SET genus='",changes_file$New_name1[row],
                        "' WHERE genus='", changes_file$Old_genus[row],
                        "' AND species='", changes_file$Old_species[row], "'")
  }
   else if (changes_file$New_type[row] == "species"){
    make_query = paste0("UPDATE community_analysis_data SET species='",changes_file$New_name1[row],
                        "' WHERE genus='", changes_file$Old_genus[row],
                        "' AND species='", changes_file$Old_species[row], "'")
   }
  else {
    make_query = paste0("UPDATE community_analysis_data SET genus='",changes_file$New_name1[row],
                        "', species='", changes_file$NewName2[row],
                        "' WHERE genus='", changes_file$Old_genus[row],
                        "' AND species='", changes_file$Old_species[row], "'")
  }
  query_MACD(make_query)
}
}

remove_whitespace()
correct_names()

