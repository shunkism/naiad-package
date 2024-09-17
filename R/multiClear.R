#' Multimetric Invertebrate Index for Clear Lakes (MultiClear)
#'
#' @author Shuntaro Koizumi, Gaute Velle
#' @description
#' Function outputs MultiClear, which is a multimetric index (MMI) for assessing the ecological status of clear lakes in Norway (Sandin et al. 2014).
#' 
#' MultiClear is comprised of four indices. These four indices are:
#' \enumerate{
#' \item Total Number of Taxa of Gastropoda
#' \item Total Number of Taxa of Ephemeroptera
#' \item Acid Water Indicator Community (AWIC) Index Scores
#' \item NGIG-Adjusted Henriksson and Medins Index}
#' 
#' The four indices are then combined into a single MMI by calculating the mean of the four metrics. 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ? cleanTax() for help.
#' @export

multiClear <- function(dataClean){
  
  #M1 - Total Number Taxa Gastropoda
  
  
  #Make ID column
  dataClean$ID <- paste(dataClean$River, dataClean$Station, dataClean$Date, sep = "_")
  ID <- unique(dataClean$ID)
  
  #This basically pastes all the unique taxa groups that are expected based on the allTaxa dataframe and pastes them to each individual station
  #This is necessary to "create" zeroes for taxa that might not be observed in either the initial inputted dataframe or in individual stations
  
  taxalist <- unique(allTaxa$Taxagroup)
  
  taxaID <- data.frame()
  for (i in 1:length(ID)) {
    temp <- data.frame(ID = rep(ID[i],length(taxalist)),Taxagroup = taxalist)
    taxaID <- rbind(temp,taxaID)
  }
  
  #Break up the unique IDs to get the river, station, and date columns back 
  taxaID <- taxaID %>% tidyr::separate(ID, c('River','Station','Date'),sep ='_')
  
  #Merge dataClean and allTaxa to get the higher taxonomic columns 
  taxa <- left_join(dataClean, allTaxa, by = c("Species"), multiple = 'any')
  
  #Get a total number of distinct taxa per taxagroup per station-date combination
  taxacount <- plyr::ddply(taxa, c('River','Station','Date','Taxagroup'), summarize,
                           nTaxa = n_distinct(Species))
  
  
  
  #merging the taxacount dataframe with the taxaID dataframe to "create" zeroes 
  taxacount <-merge(taxacount,taxaID,all.y = TRUE)
  taxacount$nTaxa[is.na(taxacount$nTaxa)] = 0
  
  #Filtering for Gastropoda in column Taxagroup
  taxacount_Gastropoda <- taxacount %>% 
    filter(Taxagroup == "Gastropoda")
  
  
  M1 <- taxacount_Gastropoda %>% 
    rename(M1_raw = nTaxa)
  
  #Assign values based on values in M1_raw, where if M1_raw = 0 then M1_std = 1, if M1_raw =1 then M1_std = 3, if M1_raw >= 2 then M1_std = 5
  M1$M1_std <- ifelse(M1$M1_raw == 0, 1, ifelse(M1$M1_raw == 1, 3, 5))
  
  #M2 - Total Number of unique Ephemeroptera Taxa
  
  taxacount_Ephemeroptera <- taxacount %>% 
    filter(Taxagroup == "Ephemeroptera")
  
  M2 <- taxacount_Ephemeroptera %>% 
    rename(M2_raw = nTaxa)
  
  #Assign valuesbased on values in M1_raw, where if M1_raw = 0 then M1_std = 1, if M1_raw =1 then M1_std = 3, if M1_raw >= 2 then M1_std = 5
  M2$M2_std <- ifelse(M2$M2_raw == 0, 1, ifelse(M2$M2_raw == 1, 3, 5))
  
  #M3 - Acid Water Indicator Community (AWIC) Index Scores
  
  #Change capitalization of family column in taxa dataframe from all caps to first letter capitalized
  taxa$Family <- tolower(taxa$Family)
  taxa$Family <- tools::toTitleCase(taxa$Family)
  
  taxasmall_awic <- taxa %>% 
    select(River, Station, Date, Family, Species, Value)
  
  #left join with MultiClear_M3 rda file to get the AWIC scores
  taxasmall_awic <- left_join(taxasmall_awic, MultiClear_M3, by = c("Family"))
  
  #remove NA in the MC3 score
  taxasmall_awic <- taxasmall_awic %>% 
    filter(!is.na(MC3score))
  
  #Sum up the MC3score per station-date combination and divide by the number of observations to get the average MC3score
  M3 <- taxasmall_awic %>% 
    group_by(River, Station, Date) %>% 
    summarize(M3_raw = sum(MC3score)/n())
  
  #Assign values based on values in M3_raw, where if M3_raw < 4.3 then M3_std = 1, if M3_raw = 4.3 to 5 then M3_std = 3,if M3_raw >5 then M3_std = 5
  M3$M3_std <- ifelse(M3$M3_raw < 4.3, 1, ifelse(M3$M3_raw >= 4.3 & M3$M3_raw <= 5, 3, 5))
  
  #M4A <- Presence of EPT Taxa, depending on acidification tolerance in MultiClear_M4A rda file
  
  taxasmall_ept <- taxa %>% 
    select(River, Station, Date, Species, Value)
  
  #left join with MultiClear_M4A rda file to get the EPT scores
  taxasmall_ept <- left_join(taxasmall_ept, MultiClear_M4A, by = c("Species"))
  
  #Remove NAs in the MC4aScores
  taxasmall_ept <- taxasmall_ept %>% 
    filter(!is.na(MC4aScores))
  #To calculate MC4a_raw, we have to satisfy the following conditions:
  #1. If there are species belonging to Group 1 (MC4aScores == 1) in the station-date combination, then MC4a_raw = 3
  #2. If there are species belonging to Group 2 (MC4aScores == 2) but NONE in Group 1 in the station-date combination, then MC4a_raw = 2
  #3. If there are species belonging to Group 3 (MC4aScores == 3) but NONE in Group 1 or Group 2 in the station-date combination, then MC4a_raw = 1
  #4. If there are NO species belonging to Group 1, 2, or 3 in the station-date combination, then MC4a_raw = 0
  
  M4A <- taxasmall_ept %>% 
    group_by(River, Station, Date) %>% 
    summarize(MC4a_raw = case_when(
      any(MC4aScores == 1) ~ 3,
      any(MC4aScores == 2) & !any(MC4aScores == 1) ~ 2,
      any(MC4aScores == 3) & !any(MC4aScores == 1) & !any(MC4aScores == 2) ~ 1,
      !any(MC4aScores %in% c(1, 2, 3)) ~ 0
    ))
  
  #M4B  <- Presence of Gammaridae in Family column, if Present then M4B_raw = 3, if absent then M4B_raw = 0
  
  taxasmall_gam <- taxa %>% 
    select(River, Station, Date, Family, Species, Value)
  
  #Summarize taxasmall_gam to see if there is Gammaridae in family
  M4B <- taxasmall_gam %>% 
    group_by(River, Station, Date) %>% 
    summarize(MC4b_raw = case_when(
      any(Family == "Gammaridae", na.rm=TRUE) ~ 3,
      !any(Family == "Gammaridae", na.rm=TRUE) ~ 0
    ))
  
  #M4C <- Presence of Hirudinea, Elmidae, Gastropoda and Mussels (Unionidae and Margaritiferidae) in the Family column
  # Hirudinea = 1, Elmidae = 1, Gastropoda = 1, Mussels = 1, scores totalled up to get M4C_raw
  
  taxasmall_hir <- taxa %>% 
    select(River, Station, Date, Family ,Taxagroup, Species, Value)
  
  #Summarize taxasmall_hir to see if there is Hirudinea, Elmidae, Gastropoda, Mussels in family
  
  M4C <- taxasmall_hir %>% 
    group_by(River, Station, Date) %>% 
    summarize(M4C_raw = sum(
      any(Taxagroup == "Hirudinea", na.rm = TRUE),
      any(Family == "Elmidae", na.rm = TRUE),
      any(Taxagroup == "Gastropoda", na.rm = TRUE),
      any(Family %in% c("Unionidae", "Margaritiferidae"), na.rm = TRUE)
    ))
  
  #M4D <- Ratio between the abundance of Ephemeroptera (excluding Baetis niger in Species) and the abundance of Plecoptera
  
  taxasmall_eph_ple <- taxa %>% 
    select(River, Station, Date, Taxagroup, Species, Value)
  
  #Remove Baetis niger in Species
  taxasmall_eph_ple <- taxasmall_eph_ple %>% 
    filter(Species != "Baetis niger")
  
  #Summarize taxasmall_eph_ple to get the ratio between Ephemeroptera (excluding Baetis niger in Species)  and Plecoptera
  M4D <- taxasmall_eph_ple %>% 
    group_by(River, Station, Date) %>% 
    summarize(PlecEphemRat = sum(Value[Taxagroup == "Ephemeroptera"])/sum(Value[Taxagroup == "Plecoptera"]))
  M4D$PlecEphemRat[M4D$PlecEphemRat == Inf] = NA
  M4D$PlecEphemRat[is.nan(M4D$PlecEphemRat)] = NA
  
  #Convert plecEphemRat to M4D_raw based on the following conditions:
  #1. If PlecEphemRat > 1 then M4D_raw = 2
  #2. If PlecEphemRat is between 0.75 and 1 then M4D_raw = 1
  #3. If PlecEphemRat < 0.75 then M4D_raw = 0
  
  M4D$M4D_raw <- ifelse(M4D$PlecEphemRat > 1, 2, ifelse(M4D$PlecEphemRat >= 0.75 & M4D$PlecEphemRat <= 1, 1, 0))
  
  #M4E <- Number of taxa that match in the standardized taxa list (MultiClear_M4E rda file)
  
  taxasmall_taxa <- taxa %>% 
    select(River, Station, Date, Taxagroup, Species, Value)
  
  #left join with MultiClear_M4E rda file to get the standardized taxa list
  taxasmall_taxa <- left_join(taxasmall_taxa, MultiClear_M4E, by = c("Species"))
  taxasmall_taxa$Score[is.na(taxasmall_taxa$Score)] = 0
  
  #Count the number of taxa that match in the standardized taxa list
  M4E <- taxasmall_taxa %>% 
    group_by(River, Station, Date) %>% 
    summarize(StanTaxCount = sum(Score))
  
  #Convert StanTaxCount to M4E_raw based on the following conditions:
  #1. If StanTaxCount >= 32  then M4E_raw = 2
  #2. If StanTaxCount >= 17 and StanTaxCount < 32 then M4E_raw = 1
  #3. If StanTaxCount < 17 then M4E_raw = 0
  
  M4E$M4E_raw <- ifelse(M4E$StanTaxCount >= 32, 2, ifelse(M4E$StanTaxCount >= 17 & M4E$StanTaxCount < 32, 1, 0))
  
  #Merge all M4 dataframes together, without removing any rows 
  M4 <- merge(M4A, M4B, by = c("River", "Station", "Date"), all = TRUE)
  M4 <- merge(M4, M4C, by = c("River", "Station", "Date"), all = TRUE)
  M4 <- merge(M4, M4D, by = c("River", "Station", "Date"), all = TRUE)
  M4 <- merge(M4, M4E, by = c("River", "Station", "Date"), all = TRUE)
  
  #Make all NAs in MC4a_raw to 0, given that the total absence of EPT led to 0s
  M4$MC4a_raw[is.na(M4$MC4a_raw)] = 0
  
  #Calculate a column NGIG_HM, which is the sum of M4A_raw, M4B_raw, M4C_raw, M4D_raw, and M4E_raw. Call the rows directly in case dataframes are not the same
  #make sure na.rm = TRUE is set to remove NAs
  M4$NGIG_HM <- rowSums(cbind(M4$MC4a_raw, M4$MC4b_raw, M4$M4C_raw, M4$M4D_raw, M4$M4E_raw), na.rm = TRUE)
  
  #Assign values based on NGIG_HM, where if NGIG_HM = 0 to 1 then M4_raw = 1, if NGIG_HM = 2 then M4_raw = 3, if NGIG_HM >2 then M4_raw = 5
  M4$M4_std <- ifelse(M4$NGIG_HM >= 0 & M4$NGIG_HM <= 1, 1, ifelse(M4$NGIG_HM == 2, 3, 5))
  
  #Make M4 dataframe with only the necessary columns
  M4 <- M4 %>% 
    select(River, Station, Date, M4_std)
  
  #Merge all M1, M2, M3, and M4 dataframes together, without removing any rows
  MultiClear <- merge(M1, M2, by = c("River", "Station", "Date"), all = TRUE)
  MultiClear <- merge(MultiClear, M3, by = c("River", "Station", "Date"), all = TRUE)
  MultiClear <- merge(MultiClear, M4, by = c("River", "Station", "Date"), all = TRUE)
  
  #Calculate the MultiClear index, which is the mean of M1_std, M2_std, M3_std, and M4_std. Make sure to na.rm = TRUE to remove NAs
  MultiClear$MultiClear <- rowMeans(cbind(MultiClear$M1_std, MultiClear$M2_std, MultiClear$M3_std, MultiClear$M4_std), na.rm = TRUE)
  
  #Make MultiClear dataframe with only the necessary columns
  MultiClear <- MultiClear %>% 
    select(River, Station, Date, MultiClear)
  
  #Return multiClear dataframe
  return(MultiClear)
}