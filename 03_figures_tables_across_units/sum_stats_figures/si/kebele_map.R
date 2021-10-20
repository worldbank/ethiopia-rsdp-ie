# Kebele Map

# Load Data --------------------------------------------------------------------
kebele <- readOGR(file.path(project_file_path, "Data", "Kebeles", "RawData", "0cfabb08-2469-4e3a-8770-6aa5425bc49d2020328-1-lp203n.6k02l.shp"))
kebele <- spTransform(kebele, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ggplot() +
  geom_polygon(data = a,
               aes(x = lat, y = long, group = group),
               fill = NA,
               color = "black",
               size = 0.1) +
  theme_void() +
  coord_quickmap()


