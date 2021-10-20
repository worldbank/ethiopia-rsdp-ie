# Summary Statistics

ROUND_NUM <- 4

# Define Helper Functions ------------------------------------------------------

if_na_return <- function(x, 
                         value = "N/A"){
  # If "x" is na, then return "value"
  
  if(is.na(x)){
    x <- value
  }
  
  return(x)
}

if_zero_return <- function(x, 
                         value = "N/A"){
  # If "x" is na, then return "value"
  
  if(x %in% 0){
    x <- value
  }
  
  return(x)
}

# Sum Stats --------------------------------------------------------------------
data <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda", "merged_datasets", "panel_data_clean.Rds"))

## N cells in year
sum(data$year %in% 1996) %>% prettyNum(big.mark=",",scientific=FALSE)

sink(file.path(panel_rsdp_imp_data_file_path, 
          "woreda", 
          "outputs",
          "tables",
          "sum_stats_dep_vars.tex"))

cat("\\begin{tabular}{l | lll | lll } \n")
cat("\\hline \n")
cat("Variable & \\multicolumn{3}{c|}{Average}  \\\\ \n")
cat("         & 1996 & 2012 & 2016             \\\\ \n")
cat("\\hline \n")
for(var in c("MA_pop2000_theta1", 
             "MA_pop2000_theta2", 
             "MA_pop2000_theta5", 
             "MA_pop2000_theta8", 
             "MA_pop2000_theta1_exclude100km",
             "MA_pop2000_theta2_exclude100km",
             "MA_pop2000_theta5_exclude100km",
             "MA_pop2000_theta8_exclude100km")){
  
  if(var %in% "MA_pop2000_theta1")              var_name <- "Market Access: $\\theta = 1$"
  if(var %in% "MA_pop2000_theta2")              var_name <- "Market Access: $\\theta = 2$"
  if(var %in% "MA_pop2000_theta5")              var_name <- "Market Access: $\\theta = 5$"
  if(var %in% "MA_pop2000_theta8")              var_name <- "Market Access: $\\theta = 8$"
  
  if(var %in% "MA_pop2000_theta1_exclude100km") var_name <- "Market Access: $\\theta = 1$; Exclude Origin Woredas within 100km"
  if(var %in% "MA_pop2000_theta2_exclude100km") var_name <- "Market Access: $\\theta = 2$; Exclude Origin Woredas within 100km"
  if(var %in% "MA_pop2000_theta5_exclude100km") var_name <- "Market Access: $\\theta = 5$; Exclude Origin Woredas within 100km"
  if(var %in% "MA_pop2000_theta8_exclude100km") var_name <- "Market Access: $\\theta = 8$; Exclude Origin Woredas within 100km"
  
  mean_1996 <- data[[var]][data$year %in% 1996] %>% mean(na.rm = T) %>% round(ROUND_NUM)
  mean_2012 <- data[[var]][data$year %in% 2012] %>% mean(na.rm = T) %>% round(ROUND_NUM)
  mean_2016 <- data[[var]][data$year %in% 2016] %>% mean(na.rm = T) %>% round(ROUND_NUM) %>% if_na_return()

  cat(var_name, " & ",
      mean_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      mean_2012 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      mean_2016 %>% prettyNum(big.mark=",",scientific=FALSE), " \\\\ \n")
  
}

cat("\\hline \n")
cat("\\end{tabular} \n")

sink()


