#' cleanTax Function
#' 
#' @author Shuntaro Koizumi, Christian Lucien Bodin
#'
#' @description
#' Cleans dataframe based on a list of tax. For the list of taxa use data(Taxonomy).
#' 
#' @param datain A dataframe containing columns River, Date (Date Class, set as YMD), Species (taxa name), and Value (Abundance)
#' @returns dataClean is a dataframe with corrected species names
#' @returns ErrorList is a list of removed names from the original dataframe
#' 
#' @export

cleanTax <- function(datain) {
  
  #Load in Taxonomy df

  # Add a new column to store original species names
  datain$Original_Species <- datain$Species
  
  # Clean up species names
  datain$Species <- gsub("\\s+$", "", datain$Species) # remove extra spaces
  datain$Species <- gsub("( indet\\.?| sp\\.?($|\\s))", "", datain$Species) # remove indet. or sp. at the end or followed by space
  datain$Species <- gsub("\\s*/\\s*", "/", datain$Species) # remove spaces before and after slashes
  
  # Create a mapping of corrected names using amatch
  correct_names <- stringdist::amatch(datain$Species, Taxonomy2$Species, maxDist = 2)
  
  # Remove unmatched names and return a list of removed names
  removed_names <- unique(datain$Species[is.na(correct_names)])
  datain <- datain[!is.na(correct_names),]
  
  # Replace species names with corrected names
  datain$Species <- Taxonomy2$Species[correct_names[!is.na(correct_names)]]
  datain <- datain[,! names(datain) %in% "Original_Species"]    
  # Return the cleaned up dataframe and list of removed names
  list2env(list(dataClean = datain, ErrorList = removed_names),envir = .GlobalEnv)
}

