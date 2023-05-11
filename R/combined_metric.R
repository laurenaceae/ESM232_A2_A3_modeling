#' @param m modeled data
#' @param obs observed data
#' @param wy water year
#' @return Combined dataset of the correlation of wet and dry days in the observed and modeled data; wet days were considered a day with greater than 10 inches of precip; dry days were considered a day with less than 0.5 inches of precip
#' @authors Lauren Harris, Mia Guarnieri, Alia Ajina

combined_metric <- function(m, obs, wy){
  
  # drought days
  
  d_m_count <- data.frame(model = m, wy = wy) %>% 
    filter(model < 0.5) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  colnames(d_m_count) = c("year", "count_m")
  
  d_o_count <- data.frame(obs = obs, wy = wy) %>% 
    filter(obs < 0.5) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  colnames(d_o_count) = c("year", "count_o")
  
  d_combined_df <- merge(d_m_count, d_o_count, by = "year") 
  
  # wet days
  
  w_m_count <- data.frame(model = m, wy = wy) %>% 
    filter(model > 10) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  colnames(w_m_count) = c("year", "count_m")
  
  w_o_count <- data.frame(obs = obs, wy = wy) %>% 
    filter(obs > 10) %>% 
    group_by(wy) %>% 
    summarise(n())
  
  colnames(w_o_count) = c("year", "count_o")
  
  w_combined_df <- merge(w_m_count, w_o_count, by = "year") 
  
  # correlation
  
  w_cor <- cor(x = w_combined_df$count_m, y = w_combined_df$count_o)
  
  d_cor <- cor(x = d_combined_df$count_m, y = d_combined_df$count_o)
  
  combined_metric <- w_cor + d_cor
  
  return(combined_metric)
}
