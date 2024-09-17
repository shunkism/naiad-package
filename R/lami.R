#' Lake Acidification Macroinvertebrate Index (LAMI)
#'
#' @author Shuntaro Koizumi, Gaute Velle
#' @description
#' This package provides functions to calculate the Lake Acidification Macroinvertebrate Index (LAMI) from macroinvertebrate data.
#' 
#' @param dataClean A dataframe output from cleanTax(). See ? cleanTax() for help.
#' @export

lami <- function(dataClean){
  
  #Remove sp. from Species
  y <- LAMIscores %>% mutate(Species_clean = gsub(" sp.", "", Species))
  
  #Attach the LAMIvalue from LAMIscores rda to dataClean
  lami_merge_spec <- left_join(dataClean, y, by = c("Species" = "Species_clean"))
  
  #Remove rows with no LAMI value
  lami1 <- lami_merge_spec %>% filter(!is.na(LAMIvalue))%>% 
    distinct()
  
  
  #get a mean LAMI value for each river-station-date combination
  LAMI <- lami1 %>% 
    group_by(River, Station, Date) %>% 
    summarise(LAMI = mean(LAMIvalue, na.rm = TRUE))
  
  return(LAMI)
  }