#' Shannon Diversity Index (H) Function
#' 
#'@author Shuntaro Koizumi, Gaute Velle 
#' @description
#' returns the Shannon diversity index 
#' 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

sdi <- function(dataClean){
  totInd <- plyr::ddply(dataClean, c('River','Station','Date'), summarize,
                  indTotal = sum(Value)
  )
  shdiDF <- merge(dataClean,totInd)
  shdiDF$prop <- shdiDF$Value / shdiDF$indTotal
  shdiDF$lnprop <- ifelse(shdiDF$prop==0,0,log(shdiDF$prop))
  SHDI <-  plyr::ddply(shdiDF, c('River','Station','Date'), summarize,
               H = -sum(prop*lnprop))
  
  return(SHDI)
}
