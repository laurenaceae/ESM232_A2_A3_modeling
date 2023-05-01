#' Almond yield anomaly calculator
#' 
#' This function calculates almond yield anomaly in response to climate.
#' The equation and parameters used here are from Lobell et al. (2006)
#' @param tmin_c Vector of minimum temperatures (C)
#' @param precip Vector of precipitation (mm)
#' @param month Vector of months (numeric)
#' @param year Vector of years (numeric)
#' @return Data frame of almond yield anomaly by year (tons/acre)
#' @author Lauren Harris, Alia Ajina, Mia Guarnieri
#' 
almond_yield <- function(precip, tmin_c, month, year){
  
  # put into a data frame (we did this in case column names differ from variable names)
  df <- data.frame(precip, tmin_c, month, year)
  
  df_feb <- df[df$month == 2,] # filter to February (outside loop for efficiency)

  df_jan <- df[df$month == 1,] # filter to January (outside loop for efficiency)
  
  # make new data frame to fill in
  output_df <- data.frame(matrix(ncol = 2))
  colnames(output_df) <- c("year", "anomaly")
  
  # for loop to fill in data frame with years and anomaly values
  for (i in min(unique(df$year)):max(unique(df$year))){ # loop over all years
    
    # calculate Tn_feb for the year
    df_feb_i <- df_feb[df_feb$year == i,] # filter to year
    Tn_feb_i <- suppressWarnings(mean(df_feb_i$tmin_c)) # calculate mean min temp
    
    # calculate P_jan for the year
    df_jan_i <- df_jan[df_jan$year == i,] # filter to year
    P_jan_i <- sum(df_jan_i$precip) # calculate sum 
    
    # use equation
    Y = (-0.015*Tn_feb_i) - ((0.0046)*(Tn_feb_i^2)) - (0.07*P_jan_i) + (0.0043*(P_jan_i^2)) + 0.28
    
    # add to output data frame
    output_df_row <- c(i, Y)
    output_df <- rbind(output_df, output_df_row)
  }
  
  # return cleaned up data frame
  output_df <- drop_na(output_df) # remove NA rows (first row)
  output_df$anomaly[output_df$anomaly == -Inf] <- NA # replace non-answers with NA (for lack of data)
  
  # extract min, mean, and max yield anomaly for the whole dataframe
  min = min(output_df$anomaly, na.rm = TRUE)
  mean = mean(output_df$anomaly, na.rm = TRUE)
  max = max(output_df$anomaly, na.rm = TRUE)
  
  output_list <- list(min = min, mean = mean, max = max)
  
  return(output_list)

}