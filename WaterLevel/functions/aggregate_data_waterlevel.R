##function -- take a data table and find duplicated timestamps. In duplicated rows, take the mean of numeric values (excluding rms) 
##take the first of the two character values.
## preserve original column order. 
## assumes time stamp column is a character data type. This is best for quickly aggregating with new data.
## This is the bottle neck of the workflow so it is written in base R for speed. 

aggregate_data_waterlevel <- function(dt) {
  setDT(dt)
  
  # Save original column order. This is important for aggregating later with potential straggling data from the following month's files 
  original_order <- names(dt)
  
  # Identify numeric and character columns. these need to be aggregated separately because we want to take the mean of the numeric 
  #columns and the first of the two values for character columns 
  numeric_cols <- names(dt)[sapply(dt, is.numeric) & names(dt) != "rms"]
  char_cols <- setdiff(original_order, c(numeric_cols, "TIMESTAMP"))
  
  #This section will go through and actually aggregate the data. Handling character columns and numerical columns seperately. 
  aggregated_data <- dt[, {
    result <- vector("list", length(char_cols) + length(numeric_cols))
    names(result) <- c(char_cols, numeric_cols)
    for (j in seq_along(char_cols)) {
      result[[char_cols[j]]] <- .SD[[char_cols[j]]][1]
    }
    for (j in seq_along(numeric_cols)) {
      result[[numeric_cols[j]]] <- mean(.SD[[numeric_cols[j]]], na.rm = TRUE)
    }
    result
  }, by = "TIMESTAMP", .SDcols = c(char_cols, numeric_cols)]
  
  # Reorder columns to match original using the order that we saved. 
  setcolorder(aggregated_data, intersect(original_order, names(aggregated_data)))
  
  return(aggregated_data)
}

