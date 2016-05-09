####  FIX SPECIES NAMES TYPOS AND TAXONOMIY ISSUES IN MACD SQLITE DATABASE   ####

library(DBI)
library(RSQLite)

QueryMACD = function(query){
  #  Executes queries to MACD database
  #
  #  Args: 
  #    query: a string SQL query
  #
  #  Returns:
  #    The output of the query to the SQL database
  #    If there is an output specified by the query
  #    Otherwise it returns nothing
   MACD_DB <- "MACD.sqlite"
  conn <- dbConnect(SQLite(), MACD_DB)
  query.output <- dbGetQuery(conn, query)
}

RemoveWhitespace = function(){
  #  A junk function holding queries to remove white space in MACD
  #  
  #  Calls QueryMACD function to execute removal of white space
  #  from genus and species fields in MACD
  #
  #  Returns:
  #    Nothing
  query1 = "update community_analysis_data set species = LTRIM(RTRIM(species))"
  query2 = "update community_analysis_data set genus = LTRIM(RTRIM(genus))"
  QueryMACD(query1)
  QueryMACD(query2)
}

CorrectNames = function(){
  #  Read info in file to make SQL queries to fix species names
  #
  #  Args: 
  #    None
  #
  #  Returns:
  #   Updates records in MACD SQLite database
  #   Nothing returned
changes.file = read.csv("Data/MACD_namechanges.csv", stringsAsFactors = FALSE)

for (row in 1:nrow(changes.file)) {
  if (changes.file$New_type[row] == "genus"){
    make.query = paste0("UPDATE community_analysis_data SET genus='",changes.file$New_name1[row],
                        "' WHERE genus='", changes.file$Old_genus[row],
                        "' AND species='", changes.file$Old_species[row], "'")
  } else if (changes.file$New_type[row] == "species") {
      make.query = paste0("UPDATE community_analysis_data SET species='",changes.file$New_name1[row],
                        "' WHERE genus='", changes.file$Old_genus[row],
                        "' AND species='", changes.file$Old_species[row], "'")
   } else if (changes.file$New_type[row] == "family"){
      make.query = paste0("UPDATE community_analysis_data SET family='",changes.file$New_name1[row],
                         "' WHERE genus='", changes.file$Old_genus[row],"'")
   } else {
      make.query = paste0("UPDATE community_analysis_data SET genus='",changes.file$New_name1[row],
                        "', species='", changes.file$NewName2[row],
                        "' WHERE genus='", changes.file$Old_genus[row],
                        "' AND species='", changes.file$Old_species[row], "'")
  }
  QueryMACD(make.query)
}
}

RemoveWhitespace()
CorrectNames()


