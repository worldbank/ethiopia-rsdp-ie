// =============================================================
// Extract Precipitation to Ethiopia
// =============================================================

// RUN THIS IN GOOGLE EARTH ENGINE ONLINE CODE EDITOR
// CLICK HERE FOR LINK IN GEE:
// https://code.earthengine.google.com/db929596eb7013d0c6738020db15f358

// Set to folder in Google Drive
var GEE_FOLDER = 'gee_extracts';

// =============================================================
// Function to Extract VIIRS Annual Average Data
var extract_precip = function(country_name, 
                               begin_date,
                               end_date,
                               file_name, 
                               google_drive_folder){
  // Extract precipitation to country
  
  // Args:
  //   begin_date: Beginning data
  //   end_date: End date
  //   country_name: Country to export
  //   file_name: Name to call exported image
  //   google_drive_folder: Name of folder to export image
  
  // Get country feature
  var countries = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017');
  var country = countries.filter(ee.Filter.eq('country_na', country_name)).geometry();

  // Load VIIRS ImageCollection and filter by date
  var image = ee.ImageCollection('NOAA/PERSIANN-CDR');
  image = ee.ImageCollection(
    image.filterDate(begin_date,end_date)
  );

  image = image.reduce(ee.Reducer.mean()); 
  image = image.select('precipitation_mean');

  // Clip to country and export image to google drive
  image = image.clip(country);

  Export.image.toDrive({
    folder: google_drive_folder,
    image: image,
    scale: 25000,
    region: country.bounds(),
    description: file_name,
    maxPixels : 196361426 
  });
  
  return image;
  
};

// =============================================================
// Export Data to Google Drive

var yr;
for (yr = 1992; yr < 2019; yr++) {
  var precip = extract_precip('Ethiopia', yr + '-01-01', yr + '-12-31','eth_precip_' + yr + '_avg_NOAA_PERSIANN-CDR_25km', GEE_FOLDER);
}

// =============================================================
// Map a Images to Check How they Look 
var Viz = {min: 1, max: 10, palette: ['000000', 'FFFF00'], opacity: 1};
Map.addLayer(precip, Viz, 'Precipitation');













