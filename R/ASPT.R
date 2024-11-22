#' ASPT Function
#' 
#'@author Shuntaro Koizumi, Christian Bodin, Gaute Velle 
#' @description
#' Returns ASPT index values to a dataClean dataframe
#' 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

ASPT <- function(dataClean){
  ASPTdf <- dataClean %>% 
    left_join(rami.families, by="Species")
  
  
  Bmwp.aspt <- ASPTdf %>% 
    select(River, Station, Date, Family2, Value) %>% 
    group_by(Station, Date) %>%
    tidyr::nest(data=c(Family2, Value)) %>% 
    mutate(bmwp=purrr::map(data, biotic::calcBMWP)) %>% 
    tidyr::unnest(bmwp) %>% 
    mutate(ASPTeqr = ASPT/6.9) %>% 
    mutate(ASPTEQC=(Test=case_when(ASPTeqr >0.99 ~ "Very Good",
                                   ASPTeqr>0.87 & ASPTeqr<=0.99 ~ "Good",
                                   ASPTeqr>0.75 & ASPTeqr<=0.87 ~ "Moderate",
                                   ASPTeqr>0.64 & ASPTeqr<=0.75 ~ "Poor",
                                   ASPTeqr<=0.64 ~ "Very Poor"))) %>%
    select(River, Station, Date, BMWP, ASPT, ASPTeqr, ASPTEQC)
  
  return(Bmwp.aspt)
}