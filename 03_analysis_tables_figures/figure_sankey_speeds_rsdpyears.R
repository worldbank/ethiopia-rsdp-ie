# Road Improvement: Sankey Graph

#### Parameters
COLOR_0 <- "darkslategray1"
COLOR_10 <- "dodgerblue1"
COLOR_20 <- "darkorange"
COLOR_30 <- "firebrick1"
COLOR_45 <- "darkolivegreen2"
COLOR_50 <- "forestgreen"
COLOR_70 <- "darkorchid1"
COLOR_120 <- "darkorchid4"
PLOT_BACKGROUND_COLOR <- "white"
TEXT_COLOR = "black"

# Load Data --------------------------------------------------------------------
roads <- readRDS(file.path(rsdp_dir, "RawData", "RoadNetworkPanelData_1996_2016.Rds"))

# Aggregate Data ---------------------------------------------------------------

## Simplify Speeds
speed_vars <- paste0(c("Speed"),1996:2016)
for(var in speed_vars){
  roads[[var]][roads[[var]] == 15] <- 10
  roads[[var]][roads[[var]] == 25] <- 20
  roads[[var]][roads[[var]] == 35] <- 30
}

transitions_df <- roads@data %>%
  group_by(Speed1996, Speed2002, Speed2007, Speed2010, Speed2016) %>%
  summarise(LINKLENGTH = sum(LINKLENGTH))

# Prep for Figure --------------------------------------------------------------
transitions_df$N <- transitions_df$LINKLENGTH
transitions_df$Speed1996 <- paste0(transitions_df$Speed1996, "_", 1:nrow(transitions_df), "_1") 
transitions_df$Speed2002 <- paste0(transitions_df$Speed2002, "_", 1:nrow(transitions_df), "_2") 
transitions_df$Speed2007 <- paste0(transitions_df$Speed2007, "_", 1:nrow(transitions_df), "_3") 
transitions_df$Speed2010 <- paste0(transitions_df$Speed2010, "_", 1:nrow(transitions_df), "_4") 
transitions_df$Speed2016 <- paste0(transitions_df$Speed2016, "_", 1:nrow(transitions_df), "_5") 

# Make Riverplot Object --------------------------------------------------------
#### Edges
edges1 <- subset(transitions_df, select=c(Speed1996, Speed2002, Speed2007, Speed2010, Speed2016, N))
names(edges1) <- c(paste0("N",1:5), "Value")

#### Nodes
nodes1 <- bind_rows(
  unique(edges1$N1) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=1),
  
  unique(edges1$N2) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=2),
  
  unique(edges1$N3) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=3),
  
  unique(edges1$N4) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=4),
  
  unique(edges1$N5) %>% 
    as.data.frame %>%
    dplyr::rename(ID=".") %>%
    mutate(x=5)
)

nodes1$y <- as.numeric(gsub("_.*","",nodes1$ID))

#### Style
styles = lapply(1:nrow(nodes1), function(n) {
  nodes1_i <- nodes1[n,]
  if(grepl("0_", nodes1_i$ID)) color = COLOR_0
  if(grepl("10_", nodes1_i$ID)) color = COLOR_10
  #if(grepl("15_", nodes1_i$ID)) color = COLOR_15
  if(grepl("20_", nodes1_i$ID)) color = COLOR_20
  #if(grepl("25_", nodes1_i$ID)) color = COLOR_25
  if(grepl("30_", nodes1_i$ID)) color = COLOR_30
  #if(grepl("35_", nodes1_i$ID)) color = COLOR_35
  if(grepl("45_", nodes1_i$ID)) color = COLOR_45
  if(grepl("50_", nodes1_i$ID)) color = COLOR_50
  if(grepl("70_", nodes1_i$ID)) color = COLOR_70
  if(grepl("120_", nodes1_i$ID)) color = COLOR_120
  
  #if(grepl("60_", nodes1_i$ID)) color = COLOR_60
  #if(grepl("70_", nodes1_i$ID)) color = COLOR_70
  
  if(grepl("_1\\b", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol=TEXT_COLOR,nodestyle="point",textpos=2)
  }
  
  if(!grepl("_1\\b|_5\\b", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol=TEXT_COLOR,nodestyle="point",textpos=NULL)
  }
  
  if(grepl("_5\\b", nodes1_i$ID)){
    out <- list(col = color, lty = 0, srt="0", textcol=TEXT_COLOR,nodestyle="point",textpos=4)
  }
  
  return(out)
})
names(styles) = nodes1$ID

#### Labels
labels <- c(gsub("_.*","",nodes1$ID))
labels[labels == "0"] <- "No Existing\nRoad"
labels[labels == "10"] <- "10/15"
labels[labels == "20"] <- "20/25"
labels[labels == "30"] <- "30/35"

labels[nodes1$x %in% 2:4] <- ""
labels[2:7] <- ""
labels[9] <- ""
labels[11:15] <- ""
labels[17:22] <- ""
labels[25:28] <- ""
labels[119] <- ""
labels[121:140] <- ""

#### Make Object
edges1a <- edges1[,c(1,2,6)]
edges1b <- edges1[,c(2,3,6)] %>% dplyr::rename(N1=N2) %>% dplyr::rename(N2=N3)
edges1c <- edges1[,c(3,4,6)] %>% dplyr::rename(N1=N3) %>% dplyr::rename(N2=N4)
edges1d <- edges1[,c(4,5,6)] %>% dplyr::rename(N1=N4) %>% dplyr::rename(N2=N5)
edges1_2 <- bind_rows(edges1a, edges1b, edges1c, edges1d)

#### Adjust Nodes
nodes1$y[grepl(paste0("70_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("70_",1:21,collapse="|"), nodes1$ID)] - 10
nodes1$y[grepl(paste0("120_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("120_",1:21,collapse="|"), nodes1$ID)] - 50

nodes1$y[grepl(paste0("30_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("30_",1:21,collapse="|"), nodes1$ID)] + 6
nodes1$y[grepl(paste0("10_",1:21,collapse="|"), nodes1$ID)] <- nodes1$y[grepl(paste0("10_",1:21,collapse="|"), nodes1$ID)] - 2

nodes1 <- nodes1 %>% as.data.frame()
edges1_2 <- edges1_2 %>% as.data.frame()
riverplot_obj <- makeRiver(nodes1, edges1_2,
                           node_labels=labels,
                           node_styles=styles)

# Make figure ------------------------------------------------------------------
# Y location of text
year_y <- 0.125
phase_y <- 0.075
note_y <- 0.025

# X location of text
year_start <- 0.175
year_inc <- 0.16 # Space between year text

phase_start <- 0.25
phase_inc <- 0.162

png(file.path(paper_figures, "sankey_speed_rsdpyears.png"),
    width = 5*480, 
    height = 5*480,
    res=300, 
    bg=PLOT_BACKGROUND_COLOR) 
riverplot(riverplot_obj, nsteps=100, fix.pdf=T, plot_area=c(0.75, 0.75), xscale=.9)
text(year_start + year_inc*0, year_y,"1996",font=1,col=TEXT_COLOR) 
text(year_start + year_inc*1, year_y,"2002",font=1,col=TEXT_COLOR)
text(year_start + year_inc*2, year_y,"2007",font=1,col=TEXT_COLOR)
text(year_start + year_inc*3, year_y,"2010",font=1,col=TEXT_COLOR) 
text(year_start + year_inc*4, year_y,"2016",font=1,col=TEXT_COLOR) 

text(phase_start + phase_inc*0, phase_y,"Phase I",font=2,col=TEXT_COLOR) 
text(phase_start + phase_inc*1, phase_y,"Phase II",font=2,col=TEXT_COLOR)
text(phase_start + phase_inc*2, phase_y,"Phase III",font=2,col=TEXT_COLOR)
text(phase_start + phase_inc*3, phase_y,"Phase IV",font=2,col=TEXT_COLOR) 

text(0.032, note_y, "Note: Width of lines corresponds to road segment length.",
     font=1,cex=.7,adj=0)
dev.off()






