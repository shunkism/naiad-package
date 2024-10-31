#' F1F2 Function
#' 
#'@author Shuntaro Koizumi, Christian Bodin, Gaute Velle 
#' @description
#' Returns F1F2 index values to a dataClean dataframe
#' 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export


f1f2 <- function(dataClean){
  F1 <- dplyr::group_by(dataClean, Station,River,Date)
  F1 <- dplyr::left_join(F1,F1F2.Weight, by = 'Species')
  F1 <- dplyr::mutate(F1, Value = as.numeric(Value))
  F1 <- dplyr::mutate(F1, F1 = Verdi)
  F1 <- dplyr::select(F1, -Verdi)
  F1 <- dplyr::group_by(F1, Station, Date)
  F1 <- dplyr::mutate(F1, F1Station = ifelse(all(is.na(F1)), NA, max(F1, na.rm = TRUE)))
  
#F2 <- F1 %>%
#  dplyr::filter(Orden %in% c("Plecoptera", "Ephemeroptera")) %>%
#  dplyr::group_by(River, Station, Date, Orden, F1Station) %>%
#  dplyr::filter(!(F1 %in% c(0.5, 0.25, 1) & Orden == "Plecoptera")) %>%
#  dplyr::filter(!(F1 %in% c(0.5, 0.25, 0) & Orden == "Ephemeroptera")) %>%
#  dplyr::summarise(totvalue = sum(Value, na.rm = TRUE)) %>%
#  tidyr::pivot_wider(names_from = Orden, values_from = totvalue, values_fill = 0) %>%
#  dplyr::mutate(F2 = if_else(
#    !is.na(Plecoptera) & Plecoptera > 0 & !is.na(Ephemeroptera),
#    0.5 + (Ephemeroptera / Plecoptera),
#    F1Station)) %>% 
#  dplyr::select(River, Station, Date, F2)
#  
#  F1F2 <- F1 %>% 
#    dplyr::left_join(F2, by=c("Station", "Date", "River")) %>% 
#    dplyr::select(River, Station, F1Station, F2, Date) %>% 
#    dplyr::group_by(Station, F1Station, F2, Date) %>% 
#    dplyr::distinct()
  
return(F1)  
  }