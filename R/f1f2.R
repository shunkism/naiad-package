#' F1F2 Function
#' 
#' @author Shuntaro Koizumi, Christian Bodin, Gaute Velle 
#' @description
#' Returns F1F2 index values to a dataClean dataframe
#' 
#' @param dataClean A dataframe output from cleanTax(). See ?cleanTax() for help. 
#' @export

f1f2 <- function(dataClean) {
  # Ensure 'Value' column is numeric and join with F1F2.Weight on 'Species'
  F1 <- dataClean %>%
    dplyr::group_by(Station, River, Date) %>%
    dplyr::left_join(F1F2.Weight, by = 'Species') %>%
    dplyr::mutate(Value = as.numeric(Value),
                  F1 = Verdi) %>%
    dplyr::select(-Verdi)
  
# Calculate F1Station as the max F1 per station and date
F1 <- F1 %>%
  dplyr::group_by(River, Station, Date) %>%
  dplyr::mutate(F1Station = ifelse(all(is.na(F1)), 0, max(F1, na.rm = TRUE)))

# Calculate F2 index, considering only certain species in Plecoptera and Ephemeroptera orders
F2 <- F1 %>%
  dplyr::group_by(River, Station, Date) %>%        # Work at station-date level
  dplyr::summarise(
    F1Station = dplyr::first(F1Station),           # Carry station-level F1
    Plecoptera = sum(Value[Orden == "Plecoptera" & F1 == 0], na.rm = TRUE),   # Count qualifying Plecoptera
    Ephemeroptera = sum(Value[Orden == "Ephemeroptera" & F1 == 1], na.rm = TRUE), # Count qualifying Ephemeroptera
    .groups = "drop"                               # Remove grouping
  ) %>%
  dplyr::mutate(
    F2 = dplyr::case_when(
      Plecoptera > 0 & Ephemeroptera > 0 ~ 0.5 + (Ephemeroptera / Plecoptera), # Compute F2 if both present
      TRUE ~ F1Station                                                        # Otherwise use F1Station
    ),
    F2 = pmin(F2, 1.0)                           # Cap F2 at 1
  ) %>%
  dplyr::select(River, Station, Date, F2)        # Keep only output columns
  
  
  # Join F1 and F2 results, select relevant columns, and return unique records
  F1F2 <- F1 %>%
    dplyr::left_join(F2, by = c("Station", "Date", "River")) %>%
    dplyr::select(River, Station, Date, F1Station, F2) %>%
    dplyr::distinct()
  
  return(F1F2)
}
