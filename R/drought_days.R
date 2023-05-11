#' @param name description
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
