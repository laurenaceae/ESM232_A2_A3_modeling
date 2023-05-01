#' Almond yield profit calculator
#' 
#' This function calculates profits of almond yield in California per acre per year.
#' @param a anomaly in almond yield, in tons per acre
#' @param p price of almonds per ton, in USD (default = $3000/ton)
#' @param m mean almond yield, in tons per acre (default = 1 ton/acre)
#' @return Annual profit for almond yield, in USD/acre
#' @author Lauren Harris, Alia Ajina, Mia Guarnieri
#' 
almond_profit <- function(a, p = 3000, m = 1){
  
  mean_profit <- m * p
  
  anomaly_profit <- a * p
  
  total_profit <- mean_profit + anomaly_profit
  
  return(total_profit)
  
}