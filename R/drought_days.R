#' @param d Dataframe of observed and modeled streamflow data (in mm). Column names must be "wy" for water year, "model" for modeled values, and "obs" for observed values.
#' @return Combined data frame of observed and model data with total number of days in each year that had less than 0.5 mm of streamflow, and the difference between observed and model data.
#' @authors Lauren Harris, Mia Guarnieri, Alia Ajina

drought_days <- function(d){
  
  #get the count of modeled dry days
  m_count <- d %>% 
    select(wy, model) %>%
    filter(model < 0.5) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  #rename columns
  colnames(m_count) = c("year", "count_m")
  
  #get the count of observed dry days
  o_count <- d %>% 
    select(wy, obs) %>% 
    filter(obs < 0.5) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  #rename columns
  colnames(o_count) = c("year", "count_o")
  
  #combine count of modeled and observed drought days into one dataframe, and then take the difference in a new column
  combined_df <- merge(m_count, o_count, by = "year") %>% 
    mutate(diff = count_m - count_o)
  
  #return the new dataframe
  return(combined_df)
  
}
