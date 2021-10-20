# Download GADM

for(i in 1:3){
  getData('GADM', 
          country='ETH',
          level=i, 
          path = file.path(gadm_dir, "RawData"))
}
