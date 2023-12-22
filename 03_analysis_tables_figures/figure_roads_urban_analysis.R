# Roads Urban Analysis

globcover_urban_fun <- function(r){
  r[] <- as.numeric(r[] %in% c(190))
  return(r)
}

# Load data --------------------------------------------------------------------
rsdp_sf <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds")) %>%
  st_as_sf()

gc_r <- raster(file.path(gc_dir, "RawData", "2016_2018_data", 
                         paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-","2018","-v2.1.1.tif"))) %>% 
  crop(extent(rsdp_sf))

eth_sf <- readRDS(file.path(gadm_dir, "RawData", "gadm36_ETH_1_sp.rds")) %>% st_as_sf()
addis_sf <- eth_sf[eth_sf$NAME_1 %in% "Addis Abeba",]

# Prep data --------------------------------------------------------------------
#### Land Cover
urban_r <- calc(gc_r, fun=globcover_urban_fun)

#### Roads
rsdp_sf <- rsdp_sf %>%
  dplyr::filter(Speed2016 >= 50) 

rsdp_buff_sf <- rsdp_sf %>% 
  st_buffer(dist = 150) 

rsdp_buff_one_sf <- rsdp_buff_sf %>%
  st_combine() %>%
  st_make_valid()

# Proportion and length by road type -------------------------------------------
prop_urban_df <- map_df(unique(rsdp_sf$Speed2016), function(speed_i){
  print(speed_i)
  road_i <- rsdp_sf[rsdp_sf$Speed2016 %in% speed_i,] %>% 
    st_buffer(dist = 150) %>%
    st_union() %>%
    st_make_valid() %>%
    st_as_sf()
  
  if(nrow(road_i) == 2) road_i <- road_i[2,]
  
  len_m <- st_length(rsdp_sf[rsdp_sf$Speed2016 %in% speed_i,]) %>% sum %>% as.numeric()
  prop_urban <- exact_extract(urban_r, road_i, "mean") %>% as.numeric()
  
  return(data.frame(speed_i = speed_i,
                    len_m = len_m,
                    prop_urban = prop_urban))
})

prop_urban_df <- prop_urban_df %>%
  dplyr::mutate(class = case_when(
    speed_i == 10 ~ "Town roads (earth)",
    speed_i == 15 ~ "Town roads (gravel)",
    speed_i == 20 ~ "Town roads (cobbled), earth surfaced, or URRAP roads",
    speed_i == 25 ~ "Federal grave, regional gravel, or regional rural roads",
    speed_i == 30 ~ "Town (asphalt) or earth surface roads",
    speed_i == 35 ~ "URRAP, federal gravel, or regional rural roads",
    speed_i == 45 ~ "Regional gravel road",
    speed_i == 50 ~ "Federal gravel road",
    speed_i == 70 ~ "Asphat road (highway)",
    speed_i == 120 ~ "Asphat road (expressway)"
  )) %>%
  dplyr::mutate(tex = paste0(speed_i, "km/h & ",
                             class, " & ",
                             round(len_m/1000, 0), "km & ",
                             round(prop_urban, 4), " \\\\ \n" )) %>%
  arrange(speed_i) 

sink(file.path(paper_tables, "speed_length_urban.tex"))
cat("\\begin{tabular}{lllc} ")
cat("\\hline")
cat("Speed & Road Classification & Length & Prop. Urban \\\\ \n")
prop_urban_df$tex %>%
  paste(collapse = " ") %>%
  cat()
cat("\\hline")
cat("\\end{tabular}")
sink()

# Proportion -------------------------------------------------------------------
exact_extract(urban_r, rsdp_buff_one_sf, "mean")
rsdp_sf$prop_urban <- exact_extract(urban_r, rsdp_buff_sf, "mean") %>% as.numeric()

# Length in Addis --------------------------------------------------------------
i <- 1
rsdp_sf$prop_addis <- lapply(1:nrow(rsdp_sf), function(i){
  print(i)
  length_m <- rsdp_sf[i,] %>% st_length() %>% as.numeric()
  length_addis_m <- rsdp_sf[i,] %>% st_intersection(addis_sf) %>% st_length() %>% as.numeric()
  
  if(length(length_addis_m) == 0) length_addis_m <- 0
  
  return(length_addis_m / length_m)
}) %>%
  unlist()

rsdp_sf$LINKLENGTH[rsdp_sf$prop_addis >= 0.5]

# Map --------------------------------------------------------------------------
# To ensure not jsut cpatuing roads in cities
rsdp_sf <- rsdp_sf %>%
  dplyr::filter(prop_addis <= 0.5,
                LINKLENGTH >= 10) %>%
  arrange(-prop_urban)

p_list <- lapply(1:18, function(i){
  road_i <- rsdp_sf[i,]
  
  urban_r_i <- urban_r %>%
    crop(extent(road_i %>% st_buffer(dist = 1000)))
  
  r_df <- rasterToPoints(urban_r_i, spatial = TRUE) |> as.data.frame()
  names(r_df) <- c("value", "x", "y")
  r_df$value[r_df$value == 0] <- NA
  
  rd_label <- paste0("Road: ", road_i$LINKNAME, ", Length: ",
                     round(road_i$LINKLENGTH,1), "km")
  rd_subtitle <- paste0("Proportion Urban within 150m of road: ", 
                        round(road_i$prop_urban, 4))
  
  ##### Map
  p <- ggplot() +
    geom_sf(data = rsdp_sf[i,]) +
    geom_raster(data = r_df,
                aes(x = x, y = y,
                    fill = value)) +
    geom_sf(data = rsdp_sf[i,]) +
    labs(title = rd_label,
         subtitle = rd_subtitle) +
    scale_fill_gradient2(low = "red",
                         mid = "red",
                         high = "red",
                         midpoint = 1,
                         na.value = NA) +
    #coord_quickmap() +
    theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none")
  p
})

p <- ggarrange(p_list[[1]], p_list[[2]], p_list[[3]], p_list[[4]], p_list[[5]],
               p_list[[6]], p_list[[7]], p_list[[8]], p_list[[9]], p_list[[10]],
               p_list[[11]], p_list[[12]], p_list[[13]], p_list[[14]], p_list[[15]],
               p_list[[16]], p_list[[17]], p_list[[18]],
               ncol = 3,
               nrow = 6)

ggsave(p, filename = file.path(paper_figures, "urban_roads.png"),
       height = 10, width = 11)


# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = rsdp_buff_sf)

