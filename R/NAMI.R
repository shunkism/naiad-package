#' NAMI (Nordic Acidity Macroinvertebrate Index)  
#' 
#'@author Shuntaro Koizumi, Gaute Velle 
#' @description
#' Returns NAMI values based on a set of indices published in Carlson et al. 2023 (Ecological Indicators).
#' 
#' NAMI is an MMI (multimetric index) comprised of seven indices. These seven indices are:
#' \enumerate{
#' \item The cumulative proportions of Gastropoda, Bivalvia, and Crustacea taxa 
#' \item The number of Bivalvia taxa 
#' \item Number of Ephemeroptera (excluding Leptophlebiidae)
#' \item Life Cycle Duration (>1 year)
#' \item Resistance Forms (Egg Stages)
#' \item Respiration Method (Plastron)
#' \item pH Preferendum (>5-5.5)
#'}
#'  After these seven metrics are calculated, each individual metric is standardized based on the inputted dataset,
#'  using the following transformation:
#'  
#'  \eqn{\text{Value} =  \dfrac{(\text{Metric Result} - \text{Lowest Metric Value})}{(\text{Highest Metric Value} - \text{Lowest Metric Value})}}
#'  
#'  These standardized metrics are then combined as the mean of the 0 to 1 scores of all core metrics. 
#'  
#'  MMI values are then recalibrated using the 90th and 10th percentiles as upper and lower limits of the metrics calculated 
#'  within the inputted dataset. Be aware that this assumes that the inputted dataset in @param dataClean has sufficient data
#'  and contains reference data. 
#'  
#'  Note: For metric 1 and 2 in the list above, species name from dataClean are matched with an internal dataframe which contains
#'  a list of species and associated higher classifications.
#'  
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

nami <- function(dataClean){

  #Metric 1: Cumulative Proportions of Gastropoda, Bivalvia, and Crustacea
  #Requires a list of higher taxonomic order to merge with dataClean. We could use All.Indices rda file but it seems like it
  #doesn't contain the necessary higher classifications for Crustacea and Gastropoda so we will create a new one using 
  #taxa list from freshwaterecology.info
  
  #Unfortunately, some of the classifications used aren't under the same names/are at a lower classification so we will have to
  #manually add some class types... Make sure that we reference that we added these manually
  
  dataClean$River <- stringr::str_replace_all(dataClean$River, "[^[:alnum:]]", " ")
  dataClean$Station <- stringr::str_replace_all(dataClean$Station, "[^[:alnum:]]", " ")
  
  dataClean$ID <- paste(dataClean$River,dataClean$Station,dataClean$Date,sep='_')
  ID <- unique(dataClean$ID)
  taxalist <- unique(allTaxa$Taxagroup)
  
  taxaID <- data.frame()
  for (i in 1:length(ID)) {
    temp <- data.frame(ID = rep(ID[i],length(taxalist)),Taxagroup = taxalist)
    taxaID <- rbind(temp,taxaID)
  }
  taxa <- left_join(dataClean, allTaxa, by = c('Species'))
  
  
  taxacount <- plyr::ddply(taxa, c('River','Station','Date','Taxagroup'), summarize,
                           sumValue = sum(Value))
  
  taxaID <- taxaID %>% tidyr::separate(ID, c('River','Station','Date'),sep ='_')
  
  taxacount <-merge(taxacount,taxaID,all.y = TRUE)
  taxacount$sumValue[is.na(taxacount$sumValue)] = 0
  
  countsum <- plyr::ddply(taxa, c('River','Station','Date'), summarize,
                          sumValue = sum(Value))
  
  taxacountsum <- left_join(taxacount, countsum, by = c('River','Station','Date'))
  
  taxacountsum$perc <- (taxacountsum$sumValue.x / taxacountsum$sumValue.y)*100 
  gastBivCrus <- taxacountsum[taxacountsum$Taxagroup == 'Gastropoda' | taxacountsum$Taxagroup == 'Bivalvia' | taxacountsum$Taxagroup == 'Crustacea',]
  m1 <- plyr::ddply(gastBivCrus, c('River','Station','Date'),summarize,
                    m1_raw = sum(perc))
  m1$m1_std <- (m1$m1_raw - min(m1$m1_raw)) / (max(m1$m1_raw)-min(m1$m1_raw))
  
  return(m1)
  }