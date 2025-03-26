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
      Ephemeroptera_TaxaN = n_distinct(Species[Order == 'Ephemeroptera'], na.rm = TRUE),
      Plecoptera_TaxaN = n_distinct(Species[Order == 'Plecoptera'], na.rm = TRUE),
      Trichoptera_TaxaN = n_distinct(Species[Order == 'Trichoptera'], na.rm = TRUE),
      EPT_TaxaN = n_distinct(Species[Order %in% c('Ephemeroptera', 'Plecoptera', 'Trichoptera')], na.rm = TRUE),
      All_TaxaN = n_distinct(Species),
      .groups = "drop"
    )
  
  return(taxacount)
}