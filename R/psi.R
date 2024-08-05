#' taxaCount Function
#' 
#'@author Shuntaro Koizumi, Christian Bodin, Gaute Velle 
#' @description
#' Counts the number of Ephemeroptera, Plecoptera, Trichoptera, and all taxa
#' 
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

psi <- function(dataClean){
  FSSRTaxa <- merge(dataClean, FSSR, by.x = "Species", by.y = "Taxa", all.x = TRUE) %>% 
    na.omit("FSSR")
  
  
  calculate_score <- function(FSSR, Value) {
    case_when(
      FSSR == "A" & between(Value, 1, 9) ~ 2,
      FSSR == "A" & between(Value, 10, 99) ~ 3,
      FSSR == "A" & between(Value, 100, 999) ~ 1,
      FSSR == "A" & Value >= 1000 ~ 2,
      FSSR == "B" & between(Value, 1, 9) ~ 1,
      FSSR == "B" & between(Value, 10, 99) ~ 2,
      FSSR == "B" & between(Value, 100, 999) ~ 3,
      FSSR == "B" & Value >= 1000 ~ 1,
      FSSR == "C" & between(Value, 1, 9) ~ 1,
      FSSR == "C" & between(Value, 10, 99) ~ 2,
      FSSR == "C" & between(Value, 100, 999) ~ 3,
      FSSR == "C" & Value >= 1000 ~ 1,
      FSSR == "D" & between(Value, 1, 9) ~ 2,
      FSSR == "D" & between(Value, 10, 99) ~ 3,
      FSSR == "D" & between(Value, 100, 999) ~ 1,
      FSSR == "D" & Value >= 1000 ~ 2,
      TRUE ~ NA_real_  # Handle any other cases or missing values
    )
  }
  
  # Apply the function to create the new 'Score' column
  FSSRTaxa <- FSSRTaxa %>%
    mutate(Score = mapply(calculate_score, FSSR, Value)) %>% 
    select(River, Station, Date, Species, Value, FSSR, Score)
  
  # Calculating the PSI scores for each sample.
  
  PSIScore <- FSSRTaxa %>%
    group_by(River, Station, Date) %>%
    summarise(
      PSI = (sum(Score[FSSR %in% c("A", "B")]) / sum(Score)) * 100
    ) %>%
    mutate(
      Condition = case_when(
        PSI >= 81 ~ "Minimally Sedimented/Unsedimented",
        PSI >= 61 & PSI <= 80 ~ "Slightly Sedimented",
        PSI >= 41 & PSI <= 60 ~ "Moderately Sedimented",
        PSI >= 21 & PSI <= 40 ~ "Sedimented",
        PSI >= 0 & PSI <= 20 ~ "Heavily Sedimented",
        TRUE ~ NA_character_
      )
    )
  
  return(PSIScore)
}