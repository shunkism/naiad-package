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
  
  TaxOrd <- All.Indices %>% select(Order, Taxa)  %>% 
    filter(! is.na(Order)) %>% distinct()
  
  taxa <- dataClean %>% left_join(TaxOrd, by = c('Species' = 'Taxa')) %>% 
    mutate(Order = case_when(Species == "Ephemeroptera" ~ "Ephemeroptera",
                             Species == "Trichoptera" ~ "Trichoptera",
                             Species == "Plecoptera" ~ "Plecoptera", 
                             TRUE ~ Order))
  
  
  taxacount <- taxa %>%
    group_by(River, Station, Date) %>%
    summarise(
      Ephemeroptera_TaxaN = n_distinct(na.omit(Species[Order == 'Ephemeroptera'])),
      Plecoptera_TaxaN = n_distinct(na.omit(Species[Order == 'Plecoptera'])),
      Trichoptera_TaxaN = n_distinct(na.omit(Species[Order == 'Trichoptera'])),
      EPT_TaxaN = n_distinct(na.omit(Species[Order %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera')])),
      All_TaxaN = n_distinct(na.omit(Species)),
      .groups = "drop"
    )
  
  taxacount_rivertotal <- taxa %>%
    group_by(River, Date) %>%
    summarise(
      LocationTotal_Ephemeroptera_TaxaN = n_distinct(na.omit(Species[Order == 'Ephemeroptera'])),
      LocationTotal_Plecoptera_TaxaN = n_distinct(na.omit(Species[Order == 'Plecoptera'])),
      LocationTotal_Trichoptera_TaxaN = n_distinct(na.omit(Species[Order == 'Trichoptera'])),
      LocationTotal_EPT_TaxaN = n_distinct(na.omit(Species[Order %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera')])),
      LocationTotal_All_TaxaN = n_distinct(na.omit(Species)),
      .groups = "drop"
    )
  
  taxacount <- taxacount %>% 
    left_join(taxacount_rivertotal, by = c("River", "Date"))
  
  return(taxacount)
}