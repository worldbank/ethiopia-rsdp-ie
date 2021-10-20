calc_ihs <- function(x) log(x + sqrt(x^2 + 1))

str_remove_vec <- function(x, rx){
  # Remove items in vector "x" that contain "rx"
  x[!grepl(rx, x)]
}

generate_road_improved_variables <- function(road_var, 
                                             data,
                                             all_years_improved_var,
                                             NEAR_CUTOFF){
  # DESCRIPTION: Creates variables indicating years since road improved,
  # and first year road was improved.
  
  # INPUT:
  # road_var: name of road variable that captures distance to road in meters
  # data: dataset
  # all_years_improved_var: T/F, whether to add a variable indicating all
  # years near an improved road
  
  print(road_var)
  final_vars <- c("year_roadTEMP", "years_since_roadTEMP", "post_roadTEMP")
  
  road_type <- road_var %>% str_replace_all("distance_", "")
  data$distance_roadTEMP <- data[[road_var]]
  
  ## Variable for year of first improvement
  data <- data %>%
    
    # Whether near improved road
    mutate(near_roadTEMP = distance_roadTEMP <= NEAR_CUTOFF) %>%
    
    # Year road improved (if any). Only consider earliest improved road. If cell near
    # area where another road was improved, we don't consider this. So:
    # 0 0 0 0 2007 0 0 2010 0 0 0 --> would yield 2007, while all zeros returns NA
    mutate(near_roadTEMP_X_year = near_roadTEMP * year) %>%
    mutate(near_roadTEMP_X_year = na_if(near_roadTEMP_X_year, 0) %>% as.numeric())
  
  # Create variable indicating all years road improved: e.g., 2007;2010
  if(all_years_improved_var){
    data <- data %>%
      group_by(cell_id) %>%
      mutate(near_roadTEMP_all_years = paste(near_roadTEMP_X_year, collapse=";") %>% str_replace_all("NA;|;NA", "")) 
    
    final_vars <- c(final_vars, "near_roadTEMP_all_years")
  }
  
  ## Variable for each cell of first year became near an improved road
  data_dt <- as.data.table(data)
  data <- data_dt[, year_roadTEMP:=min(near_roadTEMP_X_year,na.rm=T), by=list(cell_id)] %>% as.data.frame()
  data$year_roadTEMP[data$year_roadTEMP %in% Inf] <- NA
  
  ## Years since road improved and binary 1/0 road improved variable
  data$years_since_roadTEMP <- data$year - data$year_roadTEMP
  data$post_roadTEMP <- data$years_since_roadTEMP >= 0
  data$post_roadTEMP[is.na(data$post_roadTEMP)] <- 0
  
  # +/- 10 years aggregate
  data$years_since_roadTEMP[data$years_since_roadTEMP >= 10] <- 10
  data$years_since_roadTEMP[data$years_since_roadTEMP <= -10] <- -10
  
  # Subset variables and rename
  data <- data %>%
    dplyr::select(all_of(final_vars))
  
  # Prep variables
  data$years_since_roadTEMP <- data$years_since_roadTEMP %>% as.factor() %>% relevel("-1")
  data$post_roadTEMP <- data$post_roadTEMP %>% as.numeric()
  
  names(data) <- names(data) %>% str_replace_all("roadTEMP", road_type)
  
  return(data)
}