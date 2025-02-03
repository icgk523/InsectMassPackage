check_column <- function(df, column = NULL, default_column_name = NULL) {
  # If no column is specified, check if the default column exists
  if (is.null(column)) {
    if (!default_column_name %in% colnames(df)) {
      stop(paste(default_column_name, "column not found in the data frame and no column specified."))
    }
    return(default_column_name)
  } 
  
  # If a column is specified, check if it exists in the data frame
  if (!column %in% colnames(df)) {
    stop(paste("The specified column", column, "does not exist in the data frame."))
  } else {
    colnames(df)[colnames(df) == column] = default_column_name
  }
  
  return(df)
}


