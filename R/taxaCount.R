#' taxaCount Function
#' 
#'@author Shuntaro Koizumi, Christian Bodin, Gaute Velle 
#' @description
#' Counts the number of Ephemeroptera, Plecoptera, Trichoptera, and all taxa
#' 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

taxaCount <- function(dataClean){
taxa <- left_join(dataClean, All.Indices, by = c('Species' = 'Taxa'))
  
  taxacount <- plyr::ddply(taxa, c('River','Station','Date'),summarize,
                    Ephemeroptera_TaxaN = length(unique(Species[Order == 'Ephemeroptera'])),
                    Plecoptera_TaxaN = length(unique(Species[Order == 'Plecoptera'])),
                    Trichoptera_TaxaN = length(unique(Species[Order == 'Trichoptera'])),
                    EPT_TaxaN = sum(Ephemeroptera_TaxaN, Plecoptera_TaxaN,Trichoptera_TaxaN),
                    All_TaxaN = length(unique(Species)))
  return(taxacount)
}