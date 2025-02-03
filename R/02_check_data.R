#' Standardize and clean data
#' @param data The input data frame
#' @return Cleaned data frame
check_data <- function(Data, Species, Value) {
  Data <- Data |>
    dplyr::filter(complete.cases(Data$Species), !grepl("[0-9]|(sp|spp)$|(sp.|spp.)|indet.", Data$Species), Value > 0)
  Data$Species <- stringr::str_replace_all(Data$Species, "\\s*\\([^\\)]+\\)", "")
  return(Data)
}

convert_data = function(Data){
  Data <- Data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Converted_Value = if (Metric %in% measurements::conv_unit_options$length) {
        measurements::conv_unit(Value, Metric, "mm")
      } else if (Metric %in% measurements::conv_unit_options$mass) {
        measurements::conv_unit(Value, Metric, "mg")
      } else {
        Value
      }
    ) |>
    dplyr::ungroup()
  return(Data)
}
