#' Rami Function
#' 
#'@author Shuntaro Koizumi, Christian Bodin, Gaute Velle 
#' @description
#' Returns Rami index values to a dataClean dataframe
#' 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

rami <- function(dataClean){
  Ramidf <- dataClean %>% 
    left_join(rami.families, by="Species") %>% 
    left_join(Rami.Weight, by="Species") %>% 
    select(River, Station, Date, Species, Value, Order, AQEM, sk, Wk)
  
  Rami <- Ramidf %>% # summarise by River, site, date the total EPT
    dplyr::filter(Order=="Plecoptera" |
                    Order=="Ephemeroptera" |
                    Order=="Trichoptera") %>%  
    group_by(River, Station, Date) %>% 
    dplyr::summarise(EPT=n()) %>% # this is sum of EPT
    right_join(Ramidf) %>% # join it back to the main file
    mutate(h=Value/EPT) %>%  # calculate the quotient to estimate h
    mutate(hk=case_when(h<5 ~ 1, #make the hk manually
                        h>=5 & h<=20 ~ 3,
                        h>20 ~ 5)) %>% 
    mutate(Wk=as.numeric(Wk)) %>% 
    mutate(top=hk*Wk*sk,
           bottom=Wk*hk) %>% 
    group_by(River, Station, Date) %>% 
    dplyr::summarise(top=sum(top, na.rm=T), bottom=sum(bottom, na.rm=T)) %>% 
    mutate(rami=top/bottom)
  
  Ramiresult<-Rami %>% 
    group_by(River, Date) %>% 
    dplyr::summarise(rami_mean_River=mean(rami)) %>% 
    right_join(Rami) %>% 
    dplyr::select(-top, -bottom) %>% 
    dplyr::select(River, Date, Station, rami, rami_mean_River) %>% 
    mutate(RamiEqrPoor=rami/4.08) %>% 
    mutate(RamiEqr=rami/4.5)
  
  Ramiresult <- Ramiresult %>% 
    mutate(RamiEqrEQCPoor=(Test=case_when(RamiEqrPoor >0.85 ~ "Very Good",
                                          RamiEqrPoor>0.81 & RamiEqrPoor<=0.85 ~ "Good",
                                          RamiEqrPoor>0.75 & RamiEqrPoor<=0.81 ~ "Moderate",
                                          RamiEqrPoor>0.71 & RamiEqrPoor<=0.75 ~ "Poor",
                                          RamiEqrPoor<0.75 ~ "Very Poor"))) %>% 
    mutate(RamiEqrEQC=(Test=case_when(RamiEqr >0.86 ~ "Very Good",
                                      RamiEqr>0.82 & RamiEqr<=0.86 ~ "Good",
                                      RamiEqr>0.77 & RamiEqr<=0.82 ~ "Moderate",
                                      RamiEqr>0.73 & RamiEqr<=0.77 ~ "Poor",
                                      RamiEqr<0.73 ~ "Very Poor")))
  
  return(Ramiresult)
}