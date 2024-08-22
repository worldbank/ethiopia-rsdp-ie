# Road Improvement Map

## Parameters
MAP_BACKGROUND_FILL <- "gray91" # darkseagreen
# honeydew1

#for(MAP_BACKGROUND_FILL in c("ivory", "honeydew1", "darkseagreen", "gray90", "black", "snow", "papayawhip")){

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds")) %>%
  st_as_sf()
eth_adm <- readRDS(file.path(wb_boundaries_dir, "FinalData", "ethiopia.Rds")) %>%
  st_as_sf()

dmsp1996 <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_1992_calDMSP.tif")) %>% 
  crop(eth_adm) %>% mask(eth_adm)
dmsp2012 <- raster(file.path(ntl_harmon_dir, "RawData", "Harmonized_DN_NTL_2013_calDMSP.tif")) %>% 
  crop(eth_adm) %>% mask(eth_adm)

gc1996 <- raster(file.path(gc_dir, "RawData", "1992_2015_data", "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"), 1) %>% 
  crop(eth_adm) %>% mask(eth_adm)
gc2016 <- raster(file.path(gc_dir, "RawData", "2016_2018_data", "C3S-LC-L4-LCCS-Map-300m-P1Y-2016-v2.1.1.tif")) %>% 
  crop(eth_adm) %>% mask(eth_adm)

# Nighttime Lights -------------------------------------------------------------
#### Prep Data
## 1996
dmsp1996_df <- dmsp1996 %>%
  coordinates() %>%
  as.data.frame() 
dmsp1996_df$value <- dmsp1996[]
dmsp1996_df$value_log <- log(dmsp1996_df$value + 1)

## 2012
dmsp2012_df <- dmsp2012 %>%
  coordinates() %>%
  as.data.frame() 
dmsp2012_df$value <- dmsp2012[]
dmsp2012_df$value_log <- log(dmsp2012_df$value + 1)

#### Map
make_dmsp_figure <- function(df, title){
  
  ggplot() +
    geom_sf(data = eth_adm,
                 #aes(x = long, y = lat, group = group),
                 color = "black", 
                 fill = MAP_BACKGROUND_FILL,
                 size = .15) +
    geom_raster(data = df[df$value_log > 0,] , 
                aes(x = x, y = y,
                    fill = value_log)) + 
    scale_fill_gradient2(low = "darkorange1",
                         mid = "firebrick1",
                         high = "yellow",
                         midpoint = 2, 
                         limits = c(0, 4.2)) +
    labs(title = title) +
    coord_sf() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white",
                                         color = "white"),
          plot.title = element_text(hjust = 0.5, color = "black"))
  
}

p_dmsp1996 <- make_dmsp_figure(dmsp1996_df, "Nighttime Lights")
p_dmsp2012 <- make_dmsp_figure(dmsp2012_df, "Nighttime Lights")

# GlobCover-Urban --------------------------------------------------------------
#### Prep Data
## 1996
gc1996_df <- gc1996 %>%
  coordinates() %>%
  as.data.frame()
gc1996_df$value <- gc1996[]
gc1996_df <- gc1996_df[gc1996_df$value %in% 190,]

gc1996_sf <- gc1996_df %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_buffer(dist = 2000)

## 2016
gc2016_df <- gc2016 %>%
  coordinates() %>%
  as.data.frame() 
gc2016_df$value <- gc2016[]
gc2016_df <- gc2016_df[gc2016_df$value %in% 190,]

gc2016_sf <- gc2016_df %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_buffer(dist = 2000)

#### Map
make_gc_figure <- function(df, title){
  
  ggplot() +
    geom_sf(data = eth_adm,
            color = "black", 
            fill = MAP_BACKGROUND_FILL,
            size = .15) +
    geom_sf(data = df, 
            color = "red") + 
    labs(title = title) +
    coord_sf() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white",
                                         color = "white"),
          plot.title = element_text(hjust = 0.5, color = "black"))
  
}

p_gc1996_urban <- make_gc_figure(gc1996_sf, "Urban Land")
p_gc2016_urban <- make_gc_figure(gc2016_sf, "Urban Land")

# GlobCover-Cropland --------------------------------------------------------------
#### Prep Data
## 1996
gc1996_df <- gc1996 %>%
  coordinates() %>%
  as.data.frame()
gc1996_df$value <- gc1996[]
gc1996_df <- gc1996_df[gc1996_df$value %in% c(10,11,12,20,30),]

## 2016
gc2016_df <- gc2016 %>%
  coordinates() %>%
  as.data.frame() 
gc2016_df$value <- gc2016[]
gc2016_df <- gc2016_df[gc2016_df$value %in% c(10,11,12,20,30),]

#### Map
make_gc_figure <- function(df, title){
  
  ggplot() +
    geom_sf(data = eth_adm,
            #aes(x = long, y = lat, group = group),
            color = "black", 
            fill = MAP_BACKGROUND_FILL,
            size = .15) +
    geom_raster(data = df[df$value > 0,] , 
                aes(x = x, y = y),
                fill = "chartreuse3") + 
    labs(title = title) +
    coord_sf() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white",
                                         color = "white"),
          plot.title = element_text(hjust = 0.5, color = "black"))
  
}

p_gc1996_crop <- make_gc_figure(gc1996_df, "Cropland")
p_gc2016_crop <- make_gc_figure(gc2016_df, "Cropland")

# RSDP Upgrades ----------------------------------------------------------------
## Road completion year
roads <- roads %>%
  dplyr::rename(completion_year = Complete_G) %>%
  dplyr::select(completion_year)

roads_existing <- roads[roads$completion_year <= 1996,]
roads_improved <- roads[roads$completion_year > 1996,]

## Tidy dataframe
# roads_improved$id <- row.names(roads_improved)
# roads_improved_tidy <- tidy(roads_improved)
# roads_improved_tidy <- merge(roads_improved_tidy, roads_improved@data, by = "id")

## Factor 
p_rsdp <- ggplot() +
  geom_sf(data = eth_adm,
          #aes(x = long, y = lat, group = group),
          fill = MAP_BACKGROUND_FILL, 
          color = "black", size=.2) + # gray40
  geom_sf(data = roads_existing,
          #aes(x = long, y = lat, group = group),
          color = "gray",
          size = .15) +
  geom_sf(data = roads_improved,
          #aes(x = long, y = lat, group = group),
          color = "gray70",
          size = .4) +
  geom_sf(data = roads_improved,
          aes(color = completion_year),
          size = .3) +
  theme_void() +
  scale_colour_gradientn(colours = rev(brewer.pal(n = 11, name = "Spectral"))) +
  labs(color = "Road\nImprovement\nYear",
       title = "Road Improvements") +
  theme(plot.background = element_rect(fill = "white",
                                       color = "white"),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        legend.title = element_text(color = "black", hjust = 0.5),
        legend.text = element_text(color = "black")) +
  coord_sf() 

# Append and Export ------------------------------------------------------------
p_b <- ggarrange(p_dmsp1996, p_gc1996_urban, p_gc1996_crop, nrow = 1) %>%
  annotate_figure(top = text_grob("Baseline", color = "black", face = "bold", size = 14, vjust = 1)) +
  bgcolor("white") + 
  border("white")

p_e <- ggarrange(p_dmsp2012, p_gc2016_urban, p_gc2016_crop, nrow = 1) %>%
  annotate_figure(top = text_grob("Endline", color = "black", face = "bold", size = 14, vjust = 1)) +
  bgcolor("white") + 
  border("white")

p_all <- ggarrange(p_b,
                   p_e, 
                   p_rsdp,
                   ncol = 1,
                   heights = c(0.25, 0.25, 0.5)) 

ggsave(p_all, 
       filename = file.path(paper_figures,
                            "maps_NTL_GC_RSDP.png"),
       height = 12,
       width = 10)



