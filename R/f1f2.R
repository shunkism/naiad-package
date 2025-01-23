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
    dplyr::group_by(Station, Date) %>%
    dplyr::mutate(F1Station = ifelse(all(is.na(F1)), 0, max(F1, na.rm = TRUE)))
  
  # Calculate F2 index, considering only certain species in Plecoptera and Ephemeroptera orders
  F2 <- F1 %>%
    dplyr::filter(Orden %in% c("Plecoptera", "Ephemeroptera")) %>%
    dplyr::group_by(River, Station, Date, Orden, F1Station) %>%
    dplyr::filter(!(F1 %in% c(0.5, 0.25, 1) & Orden == "Plecoptera")) %>%
    dplyr::filter(!(F1 %in% c(0.5, 0.25, 0) & Orden == "Ephemeroptera")) %>%
    dplyr::summarise(totvalue = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    tidyr::pivot_wider(
      names_from = Orden,
      values_from = totvalue,
      values_fill = list(totvalue = 0)  # Fill missing columns with 0
    ) %>%
    dplyr::mutate(
      Plecoptera = ifelse("Plecoptera" %in% colnames(.), Plecoptera, 0),  # Handle missing Plecoptera column
      Ephemeroptera = ifelse("Ephemeroptera" %in% colnames(.), Ephemeroptera, 0)  # Handle missing Ephemeroptera column
    ) %>%
    dplyr::mutate(F2 = dplyr::case_when(
      Plecoptera > 0 ~ 0.5 + (Ephemeroptera / Plecoptera),
      TRUE ~ F1Station  # Default to F1Station if Plecoptera is zero or missing
    )) %>%
    dplyr::mutate(F2 = ifelse(F2 > 1.0, 1.0, F2)) %>%  # Cap F2 at 1.0
    dplyr::select(River, Station, Date, F2)
  
  # Join F1 and F2 results, select relevant columns, and return unique records
  F1F2 <- F1 %>%
    dplyr::left_join(F2, by = c("Station", "Date", "River")) %>%
    dplyr::select(River, Station, Date, F1Station, F2) %>%
    dplyr::distinct()
  
  return(F1F2)
}
