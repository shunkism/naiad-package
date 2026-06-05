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
    
    # --- Synonym map: "old name" = "accepted name" ---
    synonyms <- c(
      "Limnaea glabra"     = "Omphiscola glabra",
      "Lymnaea glabra"     = "Omphiscola glabra",
      "Nigrobaetis niger"  = "Baetis niger"
      # Add more pairs here as needed
    )
    
    # Add a new column to store original species names
    datain$Original_Species <- datain$Species
    
    # Clean up species names
    datain$Species <- gsub("\\s+$", "", datain$Species)
    datain$Species <- gsub("( indet\\.?| sp\\.?($|\\s))", "", datain$Species)
    datain$Species <- gsub("\\s*/\\s*", "/", datain$Species)
    
    # --- Apply synonym replacement before fuzzy matching ---
    matched_syn <- synonyms[datain$Species]
    datain$Species <- ifelse(!is.na(matched_syn), matched_syn, datain$Species)
    
    # Fuzzy match against Taxonomy
    correct_names <- stringdist::amatch(datain$Species, Taxonomy$Species, maxDist = 2)
    
    # Remove unmatched names and return a list of removed names
    removed_names <- unique(datain$Species[is.na(correct_names)])
    datain <- datain[!is.na(correct_names),]
    
    # Replace species names with corrected names
    datain$Species <- Taxonomy$Species[correct_names[!is.na(correct_names)]]
    datain <- datain[, !names(datain) %in% "Original_Species"]
    
    list2env(list(dataClean = datain, ErrorList = removed_names), envir = .GlobalEnv)
  
}

