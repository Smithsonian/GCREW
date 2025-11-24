# This function will quickly create a rough plot of the variable that just got cleaned so that the processer can 
#evaluate if the variability constant for that variable is too high or too low (higher to remove less points and lower to remove more points)


#arguements: 
#csv_data = the cleaned csv dataset 
#variable =  the variable that you would like to look at

library(ggplot2)


plot_variable_chapada <- function(normalized_data, variable){


  plot <- ggplot(normalized_data, aes(x = timestamp_local, y = variable)) +
    geom_line(color = "steelblue") +
    labs(title = "Time Series Plot", x = "Time", y = variable) +
    theme_minimal()
  
  return(plot)
  
}

