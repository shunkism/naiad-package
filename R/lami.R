#' Lake Acidification Macroinvertebrate Index (LAMI)
#'
#' @author Shuntaro Koizumi, Gaute Velle
#' @description
#' This package provides functions to calculate the Lake Acidification Macroinvertebrate Index (LAMI) from macroinvertebrate data.
#' 
#' Note that littoral and outlet data is combined (assumed that the information for littoral and outlet are supplied in station)
#' 
#' LAMI is calculated per Location and summarizes all Stations within a Location on a given date to pull out only unique Taxa. 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ? cleanTax() for help.
#' @export

lami <- function(dataClean){
  
  dataClean <- dataClean %>% 
    select(Date, River, Species) %>% 
    distinct()
  
  #Remove sp. from Species
  y <- LAMIscores %>% mutate(Species_clean = gsub(" sp.", "", Species))
  
  #Attach the LAMIvalue from LAMIscores rda to dataClean
  lami_merge_spec <- left_join(dataClean, y, by = c("Species" = "Species_clean"))
  
  #Remove rows with no LAMI value
  lami1 <- lami_merge_spec %>% filter(!is.na(LAMIvalue))%>% 
    distinct()
  
  #get a mean LAMI value for each river-date combination
  LAMI_dat <- lami1 %>% 
    select(River, Date, LAMIvalue) %>% 
    group_by(River, Date) %>% 
    summarise(LAMI = mean(LAMIvalue))
  
  return(LAMI_dat)
  }