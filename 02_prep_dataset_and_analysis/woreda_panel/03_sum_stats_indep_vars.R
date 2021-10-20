# Summary Statistics

ROUND_NUM <- 3

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
          "sum_stats.tex"))

cat("\\begin{tabular}{l | lll | lll } \n")
cat("\\hline \n")
cat("Variable & \\multicolumn{3}{c|}{Average} & \\multicolumn{3}{c}{Number Woredas $>$ 0} \\\\ \n")
cat("         & 1996 & 2012 & 2016           &  1996 & 2012 & 2016                  \\\\ \n")
cat("\\hline \n")
for(var in c("dmspols_zhang", 
             "dmspols_zhang_2",
             "dmspols_zhang_6",
             "globcover_urban",
             "globcover_cropland",
             "ndvi",
             "ndvi_cropland")){
  
  if(var %in% "dmspols_zhang")         var_name <- "NTL"
  if(var %in% "dmspols_zhang_base0na") var_name <- "NTL ($>$ 0 at Baseline)"
  if(var %in% "dmspols_zhang_2")       var_name <- "NTL $>$ 2"
  if(var %in% "dmspols_zhang_6")       var_name <- "NTL $>$ 6"
  if(var %in% "globcover_urban")       var_name <- "Urban"
  if(var %in% "globcover_cropland")    var_name <- "Cropland"
  if(var %in% "ndvi")                  var_name <- "NDVI"
  if(var %in% "ndvi_cropland")         var_name <- "NDVI in Cropland"
  
  mean_1996 <- data[[var]][data$year %in% 1996] %>% mean(na.rm = T) %>% round(ROUND_NUM)
  mean_2012 <- data[[var]][data$year %in% 2012] %>% mean(na.rm = T) %>% round(ROUND_NUM)
  mean_2016 <- data[[var]][data$year %in% 2016] %>% mean(na.rm = T) %>% round(ROUND_NUM) %>% if_na_return()

  sumNon_1996 <- sum(data[[var]][data$year %in% 1996] > 0, na.rm = T) 
  sumNon_2012 <- sum(data[[var]][data$year %in% 2012] > 0, na.rm = T) 
  sumNon_2016 <- sum(data[[var]][data$year %in% 2016] > 0, na.rm = T) %>% if_zero_return()
  
  cat(var_name, " & ",
      mean_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      mean_2012 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      mean_2016 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      sumNon_1996 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      sumNon_2012 %>% prettyNum(big.mark=",",scientific=FALSE), " & ",
      sumNon_2016 %>% prettyNum(big.mark=",",scientific=FALSE), " \\\\ \n")
  
}

cat("\\hline \n")
cat("\\end{tabular} \n")

sink()


