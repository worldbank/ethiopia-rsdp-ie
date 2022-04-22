1. Follow the instructions here to download GlobCover data for 2016 - 2018:
http://maps.elie.ucl.ac.be/CCI/viewer/download.php

2. Run the following code to convert the netcdf files into geotif files: (first cd into the directory with the files)
gdalwarp -of Gtiff -co COMPRESS=LZW -co TILED=YES -ot Byte -te -180.0000000 -90.0000000 180.0000000 90.0000000 -tr 0.002777777777778 0.002777777777778 -t_srs EPSG:4326 NETCDF:C3S-LC-L4-LCCS-Map-300m-P1Y-2017-v2.1.1.nc:lccs_class C3S-LC-L4-LCCS-Map-300m-P1Y-2017-v2.1.1.tif

3. The following files should be put into this folder:
C3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.tifC3S-LC-L4-LCCS-Map-300m-P1Y-2016-v2.1.1.ncC3S-LC-L4-LCCS-Map-300m-P1Y-2016-v2.1.1.tifC3S-LC-L4-LCCS-Map-300m-P1Y-2017-v2.1.1.ncC3S-LC-L4-LCCS-Map-300m-P1Y-2017-v2.1.1.tifC3S-LC-L4-LCCS-Map-300m-P1Y-2018-v2.1.1.nc
