// =============================================================
// Extract Elevation to Ethiopia
// =============================================================

// RUN THIS IN GOOGLE EARTH ENGINE ONLINE CODE EDITOR.
// ONCE DATA IS PROCESSED, PUT DATA IN:
// [Project Directory]/Data/Elevation/RawData

// CLICK HERE FOR LINK IN GEE:
// https://code.earthengine.google.com/6fa8ea6d5c77cb61ed6a0d5adf39047a

// Set to folder in Google Drive
var GEE_FOLDER = 'gee_extracts';

// =============================================================
// Function to Extract Elevation
var extract_elev = function(country_name, 
                            file_name, 
                            google_drive_folder){
  // Extracts elevation to country level
  
  // Args:
  //   country_name: Country to export
  //   file_name: Name to call exported image
  //   google_drive_folder: Name of folder to export image
  
  // Get country feature
  var countries = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017');
  var country = countries.filter(ee.Filter.eq('country_na', country_name));
  
  // 90 meters
  var image = ee.Image('CGIAR/SRTM90_V4');

  // 30 meters
  //var image = ee.Image('USGS/SRTMGL1_003');

  // Clip to country and export image to google drive
  image = image.clip(country);

  // Original resolution is 90
  Export.image.toDrive({
    folder: google_drive_folder,
    image: image,
    scale: 10000,
    region: country,
    description: file_name,
    maxPixels : 9196361426 
  });
  
  return image;
  
};

// =============================================================
// Export Data to Google Drive
var elev_10000m = extract_elev('Ethiopia','eth_elevation_10000m',GEE_FOLDER);

// =============================================================
// Map image
var Viz = {min: 1, max: 2000, palette: ['000000', 'FFFF00'], opacity: 1};
Map.addLayer(elev_10000m, Viz, 'Elevation');














