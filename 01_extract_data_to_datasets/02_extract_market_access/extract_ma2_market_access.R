# Compute Market Access

# TODO: Need to break up by "uid" group - too large

UID_CHUNK_SIZE <- 1000

# Load for UIDs
loc_tt <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", 
                            paste0("ma1_travel_times_for_market_access_",1996,".Rds")))
orig_uids_vec <- unique(loc_tt$orig_uid)
rm(loc_tt); gc()

# Separate UIDs into chunks
orig_uids_list <- split(orig_uids_vec, ceiling(seq_along(orig_uids_vec)/UID_CHUNK_SIZE))

if(OVERWRITE_EXTRACTED_DATA){
  file.path(panel_rsdp_imp_data_file_path, DATASET_TYPE, "individual_datasets") %>%
    list.files(full.names = T,
               pattern = "*.Rds") %>%
    str_subset("ma2_market_access_") %>%
    lapply(file.remove)
}

# Load Data --------------------------------------------------------------------
for(year in 1996:2016){ # 1996:2016
  for(uids_i in orig_uids_list){
    
    OUT_PATH <- file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", 
                          paste0("ma2_market_access_",year,"_uidgroup",uids_i[1],".Rds"))    
    
    print(paste0(year, " ", uids_i[1], "/", max(unlist(orig_uids_list))))
    
    if(!file.exists(OUT_PATH) | OVERWRITE_EXTRACTED_DATA){

      location_traveltimes <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", paste0("ma1_travel_times_for_market_access_",year,".Rds")))
      woreda <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "polygons_no_road_cut.Rds"))
      gpw <- raster(file.path(gpw_dir, "RawData", "gpw-v4-population-density_2000.tif"))
      ntl <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_1996_calDMSP.tif"))
      globcover_urban <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), (1996 - 1991))
      
      globcover_urban <- crop(globcover_urban, woreda)
      globcover_urban[] <- as.numeric(globcover_urban[] %in% 190)
      
      location_traveltimes <- location_traveltimes[location_traveltimes$orig_uid %in% uids_i,]
      
      # Merge in Distance
      distance_df <- readRDS(file.path(panel_rsdp_imp_dir, DATASET_TYPE, "individual_datasets", "ma1_travel_times_for_market_access_lineardist.Rds"))
      distance_df <- distance_df[distance_df$orig_uid %in% uids_i,]
      
      location_traveltimes <- merge(location_traveltimes,
                                    distance_df,
                                    by = c("orig_uid", "dest_uid"))
      
      # Remove if orig=dest
      location_traveltimes <- location_traveltimes[location_traveltimes$orig_uid != location_traveltimes$dest_uid,]
      location_traveltimes <- location_traveltimes[location_traveltimes$travel_time > 0,]
      
      # Extract Population -----------------------------------------------------------
      # Add woreda population from gpw. First, use geometry, then centroid. If population
      # from geometry is NA, then use centroid
      
      ## Population using geometry
      gpw <- crop(gpw, woreda)
      woreda$pop_geom <- velox(gpw)$extract(sp = woreda, fun = function(x) sum(x, na.rm=T)) %>% as.vector()
      woreda$pop_geom_log <- log(woreda$pop_geom)
      
      ## Population using centroid
      # Use when geometry returns NA
      woreda_pop_centroid <- gCentroid(woreda, byid=T)
      woreda$pop_centroid <- raster::extract(gpw, woreda_pop_centroid)
      
      ## If population using geometry is NA, use from centroid
      woreda$pop_geom[is.na(woreda$pop_geom)] <- woreda$pop_centroid[is.na(woreda$pop_geom)]
      
      ## Cleanup
      woreda@data <- woreda@data %>%
        dplyr::rename(dest_pop2000 = pop_geom,
                      dest_poplog2000 = pop_geom_log,
                      dest_uid = cell_id)
      
      woreda$dest_poplog2000 <- log(woreda$dest_pop2000)
      
      # Merge Data -------------------------------------------------------------------
      location_traveltimes <- merge(as.data.table(location_traveltimes), 
                                    as.data.table(woreda@data), 
                                    by="dest_uid")
      
      # Pop Divided by TT ------------------------------------------------------------
      location_traveltimes <- location_traveltimes %>%
        dplyr::rename(
          pop = dest_pop2000,
          poplog = dest_poplog2000,
          tt = travel_time
        )
      
      for(y_var in c("pop", "poplog")){
        for(tt_var in c("tt")){ # "ic"
          for(theta in c(1,2,3.8,5,8)){
            #location_traveltimes$pop_DIV_tt_theta1 <- location_traveltimes$dest_pop2000 / (location_traveltimes$travel_time^1)
            
            location_traveltimes[[paste0(y_var, "_DIV_", tt_var, "_theta", theta)]] <-
              location_traveltimes[[y_var]] / (location_traveltimes[[tt_var]]^theta)
          }
        }
      }
      
      # Calculate Market Access ------------------------------------------------------
      calc_MA <- function(location_traveltimes, 
                          suffix,
                          MA_all_df = NULL){
        # If MA_all_df is not null, merges results to that dataset
        
        #### Calculate Market Access
        MA_df <- location_traveltimes[, list(
          ## Travel Time
          MA_pop2000_tt_theta1   = sum(pop_DIV_tt_theta1), 
          MA_pop2000_tt_theta2   = sum(pop_DIV_tt_theta2), 
          MA_pop2000_tt_theta3_8 = sum(pop_DIV_tt_theta3.8), 
          MA_pop2000_tt_theta5   = sum(pop_DIV_tt_theta5),
          MA_pop2000_tt_theta8   = sum(pop_DIV_tt_theta8),
          
          MA_poplog2000_tt_theta1   = sum(poplog_DIV_tt_theta1), 
          MA_poplog2000_tt_theta2   = sum(poplog_DIV_tt_theta2), 
          MA_poplog2000_tt_theta3_8 = sum(poplog_DIV_tt_theta3.8), 
          MA_poplog2000_tt_theta5   = sum(poplog_DIV_tt_theta5),
          MA_poplog2000_tt_theta8   = sum(poplog_DIV_tt_theta8)
          
        ), by=list(orig_uid, year)] %>%
          as.data.frame() %>%
          dplyr::rename(cell_id = orig_uid)
        
        MA_df <- MA_df %>% rename_at(vars(-cell_id, -year), ~ paste0(., suffix))
        
        if(is.null(MA_all_df)){
          MA_df_out <- MA_df
        } else{
          MA_df_out <- merge(MA_all_df, MA_df, by = c("cell_id", "year"), all=T)
        }
        
        return(MA_df_out)
      }
      
      MA_all_df <- calc_MA(location_traveltimes, "")
      
      for(dist in c(10, 20, 50, 100)){
        print(paste0(dist, "km"))
        
        MA_all_df <- calc_MA(location_traveltimes %>%
                               dplyr::filter(distance > dist*1000), 
                             paste0("_exclude",dist,"km"), 
                             MA_all_df)
        
        
      }
      
      # Export ------------------------------------------------------------------
      saveRDS(MA_all_df, OUT_PATH)
      
      rm(location_traveltimes)
      gc(); gc()
    }
  }
}


