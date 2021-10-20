# Map of RSDP Phases

# Load Data --------------------------------------------------------------------
roads_2016 <- readRDS(file.path(finaldata_file_path, "roads", "RoadNetworkPanelData_1996_2016.Rds"))
roads_2016 <- roads_2016[roads_2016$rsdp_phase %in% 1:4,]
roads_2016@data <- roads_2016@data %>%
  dplyr::select(rsdp_phase, years_since_phase_start)

eth_adm0 <- readRDS(file.path(rawdata_file_path, "GADM", "gadm36_ETH_0_sp.rds"))

# Prep Roads Data ----------------------------------------------------------------
roads_2016$id <- row.names(roads_2016)
roads_2016_tidy <- broom::tidy(roads_2016)
roads_2016_tidy <- merge(roads_2016_tidy, roads_2016, by="id")

roads_2016_tidy$rsdp_phase <- paste("Phase", roads_2016_tidy$rsdp_phase) %>% as.factor()

p <- ggplot() +
  geom_polygon(data=eth_adm0, aes(x=long, y=lat, group=group), fill="gray80") +
  geom_path(data=roads_2016_tidy, 
            aes(x=long, y=lat, group=group, color=factor(years_since_phase_start)),
            size=.1) +
  facet_wrap(~rsdp_phase) +
  theme_void() +
  coord_quickmap() +
  labs(color = "Years Since\nPhase Started")

ggsave(p, filename = file.path(figures_file_path, "map_rsdp_phases.png"), width=5.5, height=5.5)

##########################
roads_2016 <- readRDS(file.path(finaldata_file_path, "roads", "RoadNetworkPanelData_1996_2016.Rds"))
roads_2016_50_1996 <- roads_2016[roads_2016$Speed1996 %in% 50,]
roads_2016_50_1996_improved <- roads_2016_50_1996[roads_2016_50_1996$Speed2012 > 50,]
plot(roads_2016_50_1996_improved[roads_2016_50_1996_improved$Complete_G %in% 2012,], add=T, col="blue")
roads_2016_50_1996_improved$Complete_G %>% table()
