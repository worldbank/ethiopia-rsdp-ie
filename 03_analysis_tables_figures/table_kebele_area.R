# Table of Kebeles Area

# Load Data ------------------------------------------------------------------
kebele <- readRDS(file.path(panel_rsdp_imp_dir, 
                            "kebele", "individual_datasets", 
                            "polygons.Rds"))

# Stats ------------------------------------------------------------------------
# Buffering fixes some issues with the polygon
kebele <- kebele %>% gBuffer_chunks(width = 0, chunk_size = 100)
kebele_sf <- kebele %>% st_as_sf()
kebele_sf$area_m <- kebele_sf %>% st_area()
kebele_sf$area_km <- kebele_sf$area_m / (1000^2)
kebele_sf$area_km <- kebele_sf$area_km %>% as.numeric()

summary(kebele_sf$area_km)

# Table ------------------------------------------------------------------------
ROUND_NUM <- 3

sink(file.path(paper_tables, "kebele_area_table.tex"))

cat("\\begin{tabular}{cccccc} \n")
cat("\\hline \n")
cat("Min & 25$^{th}$ Percentile & Median & Mean & 75$^{th}$ Percentile & Max \\\\ \n")
cat("\\hline \n")
cat(kebele_sf$area_km %>% min() %>% round(ROUND_NUM), " & ",
    kebele_sf$area_km %>% quantile(probs = 0.25) %>% round(ROUND_NUM), " & ",
    kebele_sf$area_km %>% median() %>% round(ROUND_NUM), " & ",
    kebele_sf$area_km %>% mean() %>% round(ROUND_NUM), " & ",
    kebele_sf$area_km %>% quantile(probs = 0.75) %>% round(ROUND_NUM), " & ",
    kebele_sf$area_km %>% max() %>% round(ROUND_NUM) %>% prettyNum(big.mark = ","), " \\\\ \n ")
cat("\\hline \n")
cat("\\end{tabular}")

sink()

