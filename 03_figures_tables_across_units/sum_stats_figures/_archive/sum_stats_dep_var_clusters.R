# Summary Statistics

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
#data_woreda        <- readRDS(file.path(panel_rsdp_imp_data_file_path, "woreda",                      "merged_datasets", "panel_data_clean.Rds"))
data_ntl           <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_ntl",             "merged_datasets", "panel_data_clean.Rds"))
data_urban         <- readRDS(file.path(panel_rsdp_imp_data_file_path, "clusters_of_globcover_urban", "merged_datasets", "panel_data_clean.Rds"))
#data_grid_near_rd  <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_nearroad",       "merged_datasets", "grid_data_clean.Rds"))
#data_grid_full     <- readRDS(file.path(panel_rsdp_imp_data_file_path, "dmspols_grid_ethiopia",       "merged_datasets", "panel_data_clean.Rds"))

# Function for Sum Stats -------------------------------------------------------
make_sum_stats <- function(data, 
                           variables,
                           ROUND_NUM_NTL = 2,
                           ROUND_NUM_URBAN = 2){
  
  for(var in variables){
    
    if(var %in% "dmspols_zhang")          var_name <- "NTL"
    if(var %in% "dmspols_harmon")         var_name <- "NTL"
    if(var %in% "dmspols_zhang_base0na")  var_name <- "NTL ($>$ 0 at Baseline)"
    if(var %in% "dmspols_zhang_2")        var_name <- "NTL $\\geq$ 2"
    if(var %in% "dmspols_zhang_6")        var_name <- "NTL $\\geq$ 6"
    if(var %in% "dmspols_zhang_sum2")     var_name <- "NTL $\\geq$ 2"
    if(var %in% "dmspols_zhang_sum6")     var_name <- "NTL $\\geq$ 6"
    if(var %in% "globcover_urban")        var_name <- "GC-Urban"
    if(var %in% "globcover_cropland")     var_name <- "Cropland"
    if(var %in% "globcover_urban_sum")    var_name <- "GC-Urban"
    if(var %in% "globcover_cropland_sum") var_name <- "Cropland"
    if(var %in% "ndvi")                   var_name <- "NDVI"
    if(var %in% "ndvi_cropland")          var_name <- "NDVI in Cropland"
    
    if(var %in% "globcover_urban"){
      ROUND_NUM <- ROUND_NUM_URBAN
    } else{
      ROUND_NUM <- ROUND_NUM_NTL
    }
    
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
}

# Make Table -------------------------------------------------------------------
sink(file.path(paper_tables,
               "depvar_sumstats_clusters.tex"))

cat("\\begin{tabular}{l | ccc | ccc } \n")
cat("\\hline \n")
cat("Variable & \\multicolumn{3}{c|}{Average} & \\multicolumn{3}{c}{Number of Units with Value $>$ 0} \\\\ \n")
cat("         & 1996 & 2013 & 2016           &  1996 & 2013 & 2016                  \\\\ \n")
cat("\\hline \n")

N <- data_ntl %>%
  filter(year %in% 1996) %>%
  nrow() %>%
  prettyNum(big.mark=",",scientific=FALSE)
# N <- data_ntl %>%
#   filter(year %in% 1996,
#          distance_anyimproved_ever <= 5000) %>%
#   nrow() %>%
#   prettyNum(big.mark=",",scientific=FALSE)
cat("\\multicolumn{4}{l}{{\\bf Cities [Defined from Lit Pixels]}} & ")
cat(paste0("\\multicolumn{3}{r}{N Units: ", N, "} \\\\ \n"))
#cat("\\multicolumn{7}{c}{ } \\\\ \n")
make_sum_stats(data_ntl,
               variables = c("dmspols_harmon",
                             #"dmspols_zhang_sum2",
                             #"dmspols_zhang_sum6",
                             #"dmspols_zhang_base0na",
                             "globcover_urban_sum"))

cat("\\hline \n")
N <- data_urban %>%
  filter(year %in% 1996) %>%
  nrow() %>%
  prettyNum(big.mark=",",scientific=FALSE)
# N <- data_urban %>%
#   filter(year %in% 1996,
#          distance_anyimproved_ever <= 5000) %>%
#   nrow() %>%
#   prettyNum(big.mark=",",scientific=FALSE)
cat("\\multicolumn{4}{l}{{\\bf Cities [Defined from Globcover Urban Pixels]}} & ")
cat(paste0("\\multicolumn{3}{r}{N Units: ", N, "} \\\\ \n"))
#cat("\\multicolumn{7}{c}{ } \\\\ \n")
make_sum_stats(data_urban,
               variables = c("dmspols_harmon",
                             #"dmspols_zhang_sum2",
                             #"dmspols_zhang_sum6",
                             #"dmspols_zhang_base0na",
                             "globcover_urban_sum"))


cat("\\hline \n")
cat("\\end{tabular} \n")

sink()


