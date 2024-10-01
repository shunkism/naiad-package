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
#' \item Number of Ephemeroptera taxa (excluding Leptophlebiidae)
#' \item Life Cycle Duration (>1 year)
#' \item Resistance Forms (Egg Stages)
#' \item Respiration Method (Plastron)
#' \item pH Preferendum (>5-5.5)
#'}
#'  After these seven metrics are calculated, each individual metric is standardized based on the inputted dataset,
#'  using the following transformation:
#'  
#'  For metrics that decrease with increasing acidity:
#'  
#'  \eqn{\text{Value} =  \dfrac{(\text{Metric Result} - \text{Lowest Metric Value})}{(\text{Highest Metric Value} - \text{Lowest Metric Value})}}
#'  
#'  For metrics that increase with increasing acidity:
#'   
#'  \eqn{\text{Value} = 1 - \dfrac{(\text{Metric Result} - \text{Lowest Metric Value})}{(\text{Highest Metric Value} - \text{Lowest Metric Value})}}
#'  
#'  Valuess that are used for lowest and highest metric values in the equations above can be found in Carlson et al. 2023 (Ecol. Ind.)
#'  These standardized metrics are then combined as the mean of the 0 to 1 scores of all core metrics. 
#'  
#'  MMI values are then recalibrated using the 90th and 10th percentiles as upper and lower limits of the metrics calculated 
#'  within the inputted dataset. Be aware that this assumes that the inputted dataset in dataClean has sufficient data
#'  and contains reference data. 
#'  
#'  Note: For all 7 metrics, species name from dataClean are matched with an internal dataframe which contains
#'  a list of species and associated higher classifications. Check data("allTaxa") to see this list. If your species is 
#'  not listed, please contact me.
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
  
  #Remove special characters from river and station names, otherwise they get converted into strange strings of characters and names that can mess up some code below
  #dataClean$River <- stringr::str_replace_all(dataClean$River, "[^[:alnum:]]", " ")
  #dataClean$Station <- stringr::str_replace_all(dataClean$Station, "[^[:alnum:]]", " ")
    #The above code is causing more problems than solving them so just make sure to remove special characters before using this code
  
  #Make a unique ID code for each river/station/date so that they are all counted as a single "station"
  dataClean$ID <- paste(dataClean$River,dataClean$Station,dataClean$Date,sep='_')
  ID <- unique(dataClean$ID)
  
  #This basically pastes all the unique taxa groups that are expected based on the allTaxa dataframe and pastes them to each individual station
  #This is necessary to "create" zeroes for taxa that might not be observed in either the initial inputted dataframe or in individual stations
  
  taxalist <- unique(allTaxa$Taxagroup)
  
  taxaID <- data.frame()
  for (i in 1:length(ID)) {
    temp <- data.frame(ID = rep(ID[i],length(taxalist)),Taxagroup = taxalist)
    taxaID <- rbind(temp,taxaID)
  }
  
  #Merge the dataClean dataframe with the allTaxa rda file
  taxa <- left_join(dataClean, allTaxa, by = c('Species'), multiple = 'any')
  
  #Get a total number of individuals per taxa group 
  taxacount <- plyr::ddply(taxa, c('River','Station','Date','Taxagroup'), summarize,
                           sumValue = sum(Value))
  
  #Break up the unique IDs to get the river, station, and date columns back 
  taxaID <- taxaID %>% tidyr::separate(ID, c('River','Station','Date'),sep ='_')
  
  #merging the taxacount dataframe with the taxaID dataframe to "create" zeroes 
  taxacount <-merge(taxacount,taxaID,all.y = TRUE)
  taxacount$sumValue[is.na(taxacount$sumValue)] = 0
  
  #Get a sum of the abundances that exist in the taxa dataframe (the merged dataclean and alltaxa dfs)
  countsum <- plyr::ddply(taxa, c('River','Station','Date'), summarize,
                          sumValue = sum(Value))
  
  #Merging the sum of abundances with the new taxacount that has the zero abundances
  taxacountsum <- left_join(taxacount, countsum, by = c('River','Station','Date'))
  
  #Get a percentage (relative abundance) of each taxa vs. the total abundance within each station 
  taxacountsum$perc <- (taxacountsum$sumValue.x / taxacountsum$sumValue.y)*100 
  
  #make a df that has only gastropods, bivalves, and crustaceans 
  gastBivCrus <- taxacountsum[taxacountsum$Taxagroup == 'Gastropoda' | taxacountsum$Taxagroup == 'Bivalvia' | taxacountsum$Taxagroup == 'Crustacea',]
  
  #sum up the relative abundances of the gastropods, bivalves, and crustaceans 
  m1 <- plyr::ddply(gastBivCrus, c('River','Station','Date'),summarize,
                    m1_raw = sum(perc))
  
  #normalization equation
  m1$m1_std <- (m1$m1_raw - 0) / (18.225-0)
  
  #Calibrate numbers so that values >1 equals 1 and values <0 equals 0
  m1$m1_std[m1$m1_std>1] = 1 
  m1$m1_std[m1$m1_std<0] = 0 
  
  #Metric 2: Number of Bivalvia Taxa
  #Requires a similar list of higher taxonomic order to merge with dataClean. Thankfully we already did this in
  #the previous metric, so we can use the same procedure.
  #Have to make sure that we include zero counts, the same way we did for metric 1. 
  
  #summarize taxa dataframe into the number of unique species in each station
  bivTaxa <- plyr::ddply(taxa, c('River','Station','Date','Taxagroup'), summarize,
                         countTaxa = length(unique(Species)))
  
  #merge the above df with the taxaID df to get the zeroes 
  bivTaxa <-merge(bivTaxa,taxaID,all.y = TRUE)
  bivTaxa$countTaxa[is.na(bivTaxa$countTaxa)] = 0
  
  #pull out only bivalves from the above df 
  bivTaxa <- bivTaxa[bivTaxa$Taxagroup=='Bivalvia',]
  
  #Create an M2 (Metric 2) column, which is the number of bivalve taxa 
  bivTaxa$m2_raw <- bivTaxa$countTaxa
  
  #Cleaning up the output of the dataframe so it just has the river, station, date, and the raw M2 columns 
  m2 <- bivTaxa %>% 
    dplyr::select(River, Station, Date, m2_raw) 

  #Normalization equation 
    m2$m2_std <- (m2$m2_raw - 0) / (2-0)
  
  #Calibrate numbers so that values >1 equals 1 and values <0 equals 0
  m2$m2_std[m2$m2_std>1] = 1 
  m2$m2_std[m2$m2_std<0] = 0 
  
  
  #Metric 3: Number of Ephemeroptera taxa, excluding Leptophlebiidae
  #Use similar code to M2, just make sure to remove the Leptophlebiidae
  
  #Remove the Leptophlebiids
  ephTaxa <- taxa[! taxa$Family=='LEPTOPHLEBIIDAE',]

  #Remove rows that are just all NA
  ephTaxa <- ephTaxa[! is.na(ephTaxa$Value),]
  
  #Summarize the above df so that we get a count of the unique species 
  ephTaxa_count <- plyr::ddply(ephTaxa, c('River','Station','Date','Taxagroup'), summarize,
                               countTaxa = length(unique(Species)))
  
  #Merge with the taxaID df to "create" zeroes 
  ephTaxa_count <- merge(ephTaxa_count,taxaID,all.y = TRUE)
  ephTaxa_count$countTaxa[is.na(ephTaxa_count$countTaxa)] = 0
  
  #Pull out just the Ephemeroptera and create a new column that contains the counts of the Ephemeroptera-Leptophlebiidae 
  ephTaxa_count <- ephTaxa_count[ephTaxa_count$Taxagroup=='Ephemeroptera',]
  ephTaxa_count$m3_raw <- ephTaxa_count$countTaxa
  
  #Pull out just the relevant columns, like above
  m3 <- ephTaxa_count %>% 
    dplyr::select(River, Station, Date, m3_raw) 
  
  #Normalization equation 
  m3$m3_std <- (m3$m3_raw - 0) / (8.9 - 0)  
  
  #Calibrate numbers so that values >1 equals 1 and values <0 equals 0
  m3$m3_std[m3$m3_std>1] = 1 
  m3$m3_std[m3$m3_std<0] = 0 
  
  #Metric 4-7: Tachet Trait: (m4) Life Stage >1 year, (m5) Resistance Forms Egg Stages, (m6) Respiration Plastron, (m7) pH Preferendum >5-5.5 
  #The fuzzy coding available in the Freshwater Ecology Database have been converted to proportions so that values range from 0 to 1
  #For more information related to calculations for proportions contact Shuntaro Koizumi 
  
  
  #STEPS TO PROPERLY MERGE THE TAXA DATAFRAME WITH THE SWE TAXONOMY DATAFRAME
  # Step 1: Join dataClean with sweTax
  taxaswe <- left_join(dataClean, sweTax, by = c('Species' = 'Vetenskapligt namn'), multiple = 'any')
  
  # Step 2: Convert relevant metrics to numeric
  taxaswe$lifecycleduration_grtrthan1year <- as.numeric(taxaswe$lifecycleduration_grtrthan1year)
  taxaswe$ResForms_eggs_statoblasts <- as.numeric(taxaswe$ResForms_eggs_statoblasts)
  taxaswe$resp_plastron <- as.numeric(taxaswe$resp_plastron)
  taxaswe$pHpreferendum_grtr5to5.5 <- as.numeric(taxaswe$pHpreferendum_grtr5to5.5)
  
  # Step 3: Create a sample ID
  taxaswe$sampleID <- paste(taxaswe$River, taxaswe$Station, taxaswe$Date, sep = '_')
  
  #Select only the relevant columns
  taxaswe <- taxaswe %>%
    select(Species, sampleID, Value, lifecycleduration_grtrthan1year, ResForms_eggs_statoblasts, resp_plastron, pHpreferendum_grtr5to5.5)
  
  # Step 4: Identify rows with missing trait data
  missing_traits <- taxaswe %>%
    filter(is.na(lifecycleduration_grtrthan1year) | 
             is.na(ResForms_eggs_statoblasts) | 
             is.na(resp_plastron) | 
             is.na(pHpreferendum_grtr5to5.5))
  
  # Step 5: Pull relevant columns from allTaxa and rename
  allTaxa_simple <- allTaxa %>%
    select(Species, LifeCycle_grt1yr, ResForm_egg, Resp_pls, pHpref_grt5to5.5) %>%
    rename(lifecycleduration_grtrthan1year = LifeCycle_grt1yr,
           ResForms_eggs_statoblasts = ResForm_egg,
           resp_plastron = Resp_pls,
           pHpreferendum_grtr5to5.5 = pHpref_grt5to5.5
    )
  
  # Step 6: Merge allTaxa_simple with missing traits if applicable
  if (nrow(missing_traits) > 0) {
    merged_missing <- missing_traits %>%
      select(Species, sampleID) %>%
      left_join(allTaxa_simple, by = "Species") %>%
      # Convert merged_missing columns to numeric
      mutate(across(c(lifecycleduration_grtrthan1year, ResForms_eggs_statoblasts, 
                      resp_plastron, pHpreferendum_grtr5to5.5), 
                    as.numeric))
    
    # Step 7: Replace the missing traits in taxaswe with merged_missing
    taxaswe <- taxaswe %>%
      anti_join(missing_traits, by = "Species") %>%
      bind_rows(merged_missing)
  }
  
  # Step 8: Identify missing traits again
  missing_traits <- taxaswe %>%
    filter(is.na(lifecycleduration_grtrthan1year) | 
             is.na(ResForms_eggs_statoblasts) | 
             is.na(resp_plastron) | 
             is.na(pHpreferendum_grtr5to5.5))
  
  # Step 9: Clean up Species names by extracting genus where applicable
  misstraits_fixed <- missing_traits %>%
    mutate(Species = if_else(stringr::str_detect(Species, "^[A-Z][a-z]+ [a-z]+$"),
                             word(stringr::Species, 1), Species))
  
  # Step 10: Rename specific species names to match sweTax
  name_changes <- c("Lymnaea" = "Lymnaeidae",
                    "Chaetopteryx" = "Chaetopterygini",
                    "Pedicia" = "Pediciidae",
                    "Antocha" = "Limoniidae",
                    "Eloeophila" = "Tipulidae",
                    "Ormosia" = "Limoniidae",
                    "Colymbetinae" = "Dytiscidae",
                    "Hydroporinae" = "Dytiscidae",
                    "Turbellaria" = "Planariidae",
                    "Dixidae" = "Diptera",
                    "Tipuloidea" = "Diptera",
                    "Cylindrotomidae" = "Diptera",
                    "Anisoptera" = "Odonata",
                    "Zygoptera" = "Odonata")
  
  misstraits_fixed <- misstraits_fixed %>%
    mutate(Species = recode(Species, !!!name_changes))
  
  # Step 11: Merge misstraits_fixed with sweTax without duplicating columns
  misstraits_fixed <- misstraits_fixed %>%
    select(Species, sampleID) %>%
    left_join(sweTax, by = c('Species' = 'Vetenskapligt namn')) %>% 
    select(Species, sampleID, lifecycleduration_grtrthan1year, ResForms_eggs_statoblasts, resp_plastron, pHpreferendum_grtr5to5.5)
  
  
  # Combine updated misstraits_fixed with the main taxaswe
  taxaswe <- taxaswe %>%
    anti_join(missing_traits, by = "Species") %>%
    bind_rows(misstraits_fixed)
  
  #Functional Composition Code Starts Here:
  Inv_Sm <-  taxaswe %>% 
    select(sampleID, Species, Value) 
  
  #There seems to be some duplication, add up the counts for duplicated taxa for different sample statsasions 
  Inv_Sm <- plyr::ddply(Inv_Sm, c('sampleID','Species'),summarize,
                        sumValue = sum(Value))
  
  Inv_Sm <- as.data.frame(Inv_Sm)
  #Order inv_Sm species alphabetically
  Inv_Sm <- Inv_Sm[order(Inv_Sm$Species),]
  
  Inv_Sm <-  tidyr::pivot_wider(Inv_Sm, names_from = Species, values_from = sumValue)
  
  #Remove NAs and replace with 0 for numeric rows
  Inv_Sm <- Inv_Sm %>% 
    mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
  
  sampID <- Inv_Sm$sampleID
  Inv_Sm <- Inv_Sm[,-1]
  
  #Make Inv_Sm into a matrix
  Inv_Sm2 <- as.matrix(Inv_Sm)
  
  #Make Tachet trait matrix with only life cycle >1 year, resistance forms egg stages, respiration plastron, and pH preferendum >5-5.5
  Inv_T <- taxaswe %>% 
    ungroup() %>% 
    select(Species, lifecycleduration_grtrthan1year, ResForms_eggs_statoblasts, resp_plastron, pHpreferendum_grtr5to5.5)
  
  #Remove duplicated species
  Inv_T <- Inv_T[!duplicated(Inv_T$Species),]
  
  #Order Species in Inv_T alphabetically
  Inv_T <- Inv_T[order(Inv_T$Species),]
  
  #Make Inv_T a matrix with row names as the species
  invTspecies <- Inv_T$Species
  Inv_T <- Inv_T[,-1]
  Inv_T <- as.matrix(Inv_T)
  rownames(Inv_T) <- invTspecies
  
  tt <- FD::functcomp(Inv_T,Inv_Sm2)
  
  #Reattach the sampleID from Inv_Sm to tt
  tt$sampleID <- sampID
  
  #Get the River, Station, and Date and merge with sampleID into River, Station, and Date in tt
  taxaIDnames <- taxaswe %>% 
    select(River, Station, Date, sampleID) %>% 
    unique()
  
  tt <- merge(tt,taxaIDnames,by = 'sampleID')
  
  #remove sampleId column by name
  tt <- tt[ , !(names(tt) %in% c("sampleID"))]
  
  #Convert date to date class using lubridate
  tt$Date <- lubridate::ymd(tt$Date)
  
  #Conver tt df to tachet df and change the namnes of the columns of lifecyle >1 year, resistance forms egg stages,
  #respiration plastron, and pH preferendum >5-5.5 to m4_raw, m5_raw, m6_raw, and m7_raw
  tachet <- tt %>% 
    dplyr::select(River, Station, Date, lifecycleduration_grtrthan1year, ResForms_eggs_statoblasts, resp_plastron, pHpreferendum_grtr5to5.5) %>% 
    rename(m4_raw = lifecycleduration_grtrthan1year, m5_raw = ResForms_eggs_statoblasts, m6_raw = resp_plastron, m7_raw = pHpreferendum_grtr5to5.5)
  
  #Get a mean of each of the metrics so that we get a metric value for each metric and each station
  #tachet <- plyr::ddply(taxa, c('River','Station','Date'), summarize,
  #                      m4_raw = mean(LifeCycle_grt1yr, na.rm = TRUE),
  #                      m5_raw = mean(ResForm_egg, na.rm = TRUE),
  #                      m6_raw = mean(Resp_pls, na.rm = TRUE),
  #                      m7_raw = mean(pHpref_grt5to5.5, na.rm = TRUE))
  
  
  
  #Normalization Equations
  tachet$m4_std <- (tachet$m4_raw-0.091)/(0.321-0.091)
  tachet$m5_std <- (tachet$m5_raw-0.012)/(0.258-0.012)
  tachet$m6_std <- (tachet$m6_raw-0)/(0.051-0)
  tachet$m7_std <- 1-((tachet$m7_raw-0.187)/(0.22-0.187))
  
  #Calibrate metrics so that values >1 equals 1 and values <0 equals 0
  tachet$m4_std[tachet$m4_std>1] = 1 
  tachet$m4_std[tachet$m4_std<0] = 0 
  
  tachet$m5_std[tachet$m5_std>1] = 1 
  tachet$m5_std[tachet$m5_std<0] = 0 
  
  tachet$m6_std[tachet$m6_std>1] = 1 
  tachet$m6_std[tachet$m6_std<0] = 0 
  
  tachet$m7_std[tachet$m7_std>1] = 1 
  tachet$m7_std[tachet$m7_std<0] = 0 
  
  #Merge seven metrics into one dataframe and calculate the NAMI value by finding the mean across the seven normalized metric values
  namimets <- merge(m1,m2)
  namimets <- merge(namimets, m3)
  namimets <- merge(namimets, tachet)
  namimets$NAMI <- rowMeans(namimets[,c("m1_std","m2_std","m3_std","m4_std","m5_std","m6_std","m7_std")],na.rm = TRUE)
  
  #Pull out the relevant columns. We might want to consider having an option to spit out the different metric outputs, at times when things go wrong
  nami <- namimets %>% 
    select(River, Station, Date, NAMI)
  
  return(nami)
  }