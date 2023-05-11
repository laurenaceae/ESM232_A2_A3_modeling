#' @param d 
#' @return Combined data frame of observed and model data with total number of days in each year that had less than 0.5 inches of precipitation
#' @authors Lauren Harris, Mia Guarnieri, Alia Ajina
#' 

drought_days <- function(d){
  
  m_count <- d %>% 
    select(wy, model) %>% 
    filter(model < 0.5) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  colnames(m_count) = c("year", "count_m")
  
  o_count <- d %>% 
    select(wy, obs) %>% 
    filter(obs < 0.5) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  colnames(o_count) = c("year", "count_o")
  
  combined_df <- merge(m_count, o_count, by = "year") %>% 
    mutate(diff = count_m - count_o)
  
  return(combined_df)
  
}
