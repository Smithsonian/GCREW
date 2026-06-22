#this function will first check that the loggernet variables in the final filtered design table for the file being processed are correct.
#final step: get the research names for the ID cols from above and rename them in the final table. 

convert_loggernet_headers_waterlevel <- function(design_table_limited, dt){
  
  new_headers <- design_table_limited$research_name
  expected_current_headers <- gsub(",", ".",
                                   gsub("[()]", ".", design_table_limited$loggernet_variable))
  actual_current_headers <- colnames(dt)
  
  header_test <- match(actual_current_headers, expected_current_headers)
  
  ordered_new_headers <- new_headers[match(actual_current_headers, expected_current_headers)]
  
  if (anyNA(header_test)){
    stop("Design table loggernet variables do not match the variables in the file.
             Please check variable date ranges, names, and order for accuracy.")
  } else {
    colnames(dt) <-ordered_new_headers
  }
  
  return(dt)
  
}
