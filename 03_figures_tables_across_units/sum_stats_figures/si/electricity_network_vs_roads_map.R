# Map of Major Roads vs Electricity Network

# Load Data --------------------------------------------------------------------
elec_sp <- readOGR(file.path(data_file_path, "Electricity Network", "RawData", 
                             "Ethiopia Electricity Transmission Network.shp"))

rsdp_sp <- readRDS(file.path(data_file_path, "RSDP Roads", "FinalData", 
                             "RoadNetworkPanelData_1996_2016.Rds"))

eth <- readRDS(file.path(data_file_path, "GADM", "RawData", 
                         "gadm36_ETH_0_sp.Rds"))

# Map --------------------------------------------------------------------------
rsdp_sp <- rsdp_sp[rsdp_sp$Speed2016 >= rsdp_sp$Speed2005,]

elec_sp <- elec_sp[elec_sp$STATUS %in% c("Planned",
                                         "Under Construction"),]

elec_sp$id <- row.names(elec_sp)
elec_sp_tidy <- tidy(elec_sp)
elec_sp_tidy <- merge(elec_sp_tidy, elec_sp@data, by = "id")

rsdp_sp$id <- row.names(rsdp_sp)
rsdp_sp_tidy <- tidy(rsdp_sp)
rsdp_sp_tidy <- merge(rsdp_sp_tidy, rsdp_sp@data, by = "id")
rsdp_sp_tidy$one <- "a"

p <- ggplot() +
  geom_path(data = rsdp_sp_tidy,
            aes(x = long, y = lat, group = group, 
                size = "RSDP Roads\nImproved\n2005-2015"),
            color = "gray50") +
  geom_path(data = elec_sp_tidy,
            aes(x = long, y = lat, group = group,
                color = "UEAP Grid Lines"),
            size = 0.4) +
  geom_polygon(data = eth,
               aes(x = long, y = lat, group = group), 
               fill = NA,
               color = "black",
               size = 0.1) +
  scale_size_manual(values = 0.6) + 
  scale_color_manual(values = c("firebrick1")) + 
  labs(color = NULL,
       size = NULL) +
  theme_void() +
  coord_quickmap() 

ggsave(p, filename = file.path(paper_figures,
                               "ueap_vs_rsdp.png"),
       height = 3.8, width = 6)


