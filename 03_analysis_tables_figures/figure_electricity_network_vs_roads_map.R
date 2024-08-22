# Map of Major Roads vs Electricity Network

# Load Data --------------------------------------------------------------------
elec_sp <- read_sf(file.path(elec_net_dir, "RawData", 
                             "Ethiopia Electricity Transmission Network.shp"))

rsdp_sp <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds")) %>%
  st_as_sf()

eth <- readRDS(file.path(wb_boundaries_dir, "FinalData", "ethiopia.Rds")) %>%
  st_as_sf()

# Map --------------------------------------------------------------------------
rsdp_sp <- rsdp_sp[rsdp_sp$Speed2016 >= rsdp_sp$Speed2007,]

elec_sp <- elec_sp[elec_sp$STATUS %in% c("Planned",
                                         "Under Construction"),]

rsdp_sp <- st_intersection(rsdp_sp, eth)
elec_sp <- st_intersection(elec_sp, eth)

# elec_sp$id <- row.names(elec_sp)
# elec_sp_tidy <- tidy(elec_sp)
# elec_sp_tidy <- merge(elec_sp_tidy, elec_sp@data, by = "id")
# 
# rsdp_sp$id <- row.names(rsdp_sp)
# rsdp_sp_tidy <- tidy(rsdp_sp)
# rsdp_sp_tidy <- merge(rsdp_sp_tidy, rsdp_sp@data, by = "id")
# rsdp_sp_tidy$one <- "constant"

p <- ggplot() +
  geom_sf(data = rsdp_sp,
            #aes(x = long, y = lat, group = group, 
                aes(size = "RSDP Roads\nImproved\n2007-2016"),
            color = "gray50") +
  geom_sf(data = elec_sp,
            #aes(x = long, y = lat, group = group,
                aes(color = "UEAP Grid Lines"),
            size = 0.4) +
  geom_sf(data = eth,
             #  aes(x = long, y = lat, group = group), 
               fill = NA,
               color = "black",
               size = 0.1) +
  scale_size_manual(values = 0.6) + 
  scale_color_manual(values = c("firebrick1")) + 
  labs(color = NULL,
       size = NULL) +
  theme_void() +
  coord_sf() 

ggsave(p, filename = file.path(paper_figures,
                               "ueap_vs_rsdp.png"),
       height = 3.8, width = 6)


