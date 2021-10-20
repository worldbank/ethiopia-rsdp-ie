# Summary Statistics

#### PARAMETERS
NEAR_TARGETTED_LOCATION <- 5000
RM_DISTANE_ADDIS <- 0 # (100km scale; so 1 = 100km)
NEAR_ROAD <- 5000

rsdp_type <- "rsdp123"
dataset_type <- "kebele"

# Functions --------------------------------------------------------------------
std.error <- function(x){
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

make_stars <- function(x){
  s <- ""
  if(x <= 0.1) s <- "*"
  if(x <= 0.05) s <- "**"
  if(x <= 0.01) s <- "***"
  
  return(s)
}

# Load/Prep Data ---------------------------------------------------------------
for(rsdp_type in c("rsdp123", "rsdp1234")){
  for(dataset_type in c("kebele", "dmspols_grid_ethiopia")){
    
    if(dataset_type %in% "kebele") round_num <- 2
    if(dataset_type %in% "dmspols_grid_ethiopia") round_num <- 4
    
    df <- readRDS(file.path(panel_rsdp_imp_data_file_path, dataset_type, "merged_datasets", "panel_data_clean.Rds"))
    
    # Define rsdp/iv variables
    df$far_targettedlocs <- df[[paste0("distance_",rsdp_type,"_targettedlocs")]]   > NEAR_TARGETTED_LOCATION
    df$near_rsdp         <- df[[paste0("distance_", rsdp_type)]]                  <= NEAR_ROAD
    
    if(dataset_type %in% "kebele"){
      df$globcover_urban    <- df$globcover_urban_sum
      df$globcover_cropland <- df$globcover_cropland_sum
    }
    
    # Prep data
    df <- df %>%
      dplyr::filter(year %in% c(1992, 1996)) %>%
      dplyr::filter(far_targettedlocs %in% T) %>%
      arrange(year) %>%
      dplyr::group_by(cell_id) %>%
      dplyr::mutate(dmspols_harmon_diff     = (dmspols_harmon[year==1996]     - dmspols_harmon[year==1992]),
                    globcover_urban_diff    = (globcover_urban[year==1996]    - globcover_urban[year==1992]),
                    globcover_cropland_diff = (globcover_cropland[year==1996] - globcover_cropland[year==1992]),
                    dmspols_harmon_1996     = (dmspols_harmon[year %in% 1996]),
                    globcover_urban_1996    = (globcover_urban[year %in% 1996]),
                    globcover_cropland_1996 = (globcover_cropland[year %in% 1996])) %>%
      ungroup() %>%
      dplyr::filter(year %in% 1996)
    
    # Make Table -------------------------------------------------------------------
    make_table_row <- function(var, var_name, df, round_num){
      
      df$var <- df[[var]]
      
      ttest_out <- t.test(var ~ near_rsdp, data = df)
      
      near_mean <- df$var[df$near_rsdp %in% 1] %>% mean(na.rm=T) %>% round(round_num)
      near_ster <- df$var[df$near_rsdp %in% 1] %>% std.error() %>% round(round_num)
      
      far_mean <- df$var[df$near_rsdp %in% 0] %>% mean(na.rm=T) %>% round(round_num)
      far_ster <- df$var[df$near_rsdp %in% 0] %>% std.error() %>% round(round_num)
      
      diff <- near_mean - far_mean
      diff <- diff %>% round(round_num)
      #diff <- sprintf(paste0("%.",round_num,"f"),diff)
      
      stars <- make_stars(ttest_out$p.value)
      
      top <- paste0(var_name, 
                    " & ", 
                    near_mean, 
                    " & ", 
                    far_mean,
                    " & ",
                    diff, 
                    "$^{", stars, "}$",
                    " \\\\ ")
      
      bottom <- paste0(" & (", 
                       near_ster, 
                       ") & (", 
                       far_ster, 
                       ") & \\\\ ")
      
      out <- paste(top, bottom)
      
      return(cat(out))
    }
    
    sink(file.path(paper_tables,
                   paste0("depvar_ttest_iv_",dataset_type,"_",rsdp_type,".tex")))
    cat("\\begin{tabular}{lccc} ")
    
    cat("\\hline ")
    cat(" & (1) & (2) & T-test \\\\ ")
    cat(" & Treatment & Control & Difference \\\\ ")
    cat(" & ($<5$km RSDP) & ($>5$km RSDP) & \\\\ ")
    cat("Variable & Mean/SE & Mean/SE & (1) - (2) \\\\ ")
    cat("\\hline ")
    
    if(dataset_type %in% "dmspols_grid_ethiopia") make_table_row("dmspols_harmon_1996", "NTL (1996)", df, round_num)
    if(dataset_type %in% "kebele")                make_table_row("dmspols_harmon_1996", "NTL (1996)", df, round_num)
    
    if(dataset_type %in% "dmspols_grid_ethiopia") make_table_row("globcover_urban_1996", "Urban (1996)", df, round_num)
    if(dataset_type %in% "kebele")                make_table_row("globcover_urban_1996", "Urban (1996)", df, round_num)
    
    if(dataset_type %in% "dmspols_grid_ethiopia") make_table_row("globcover_cropland_1996", "Cropland (1996)", df, round_num)
    if(dataset_type %in% "kebele")                make_table_row("globcover_cropland_1996", "Cropland (1996)", df, round_num)
    
    if(dataset_type %in% "dmspols_grid_ethiopia") make_table_row("dmspols_harmon_diff", "NTL (Pre-trends)", df, round_num)
    if(dataset_type %in% "kebele")                make_table_row("dmspols_harmon_diff", "NTL (Pre-trends)", df, round_num)
    
    if(dataset_type %in% "dmspols_grid_ethiopia") make_table_row("globcover_urban_diff", "Urban (Pre-trends)", df, round_num)
    if(dataset_type %in% "kebele")                make_table_row("globcover_urban_diff", "Urban (Pre-trends)", df, round_num)
    
    if(dataset_type %in% "dmspols_grid_ethiopia") make_table_row("globcover_cropland_diff", "Cropland (Pre-trends)", df, round_num)
    if(dataset_type %in% "kebele")                make_table_row("globcover_cropland_diff", "Cropland (Pre-trends)", df, round_num)
    
    cat("\\hline ")
    
    cat("N & ",
        df[df$near_rsdp %in% 1,] %>% nrow() %>% prettyNum(big.mark=",",scientific=FALSE),
        " & ",
        df[df$near_rsdp %in% 0,] %>% nrow() %>% prettyNum(big.mark=",",scientific=FALSE),
        " & \\\\ ")
    
    cat("\\hline ")
    cat("\\end{tabular} ")
    
    sink()
    
  }
}







