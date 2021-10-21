// =============================================================
// Extract Temperature to Ethiopia
// =============================================================

// RUN THIS IN GOOGLE EARTH ENGINE ONLINE CODE EDITOR
// ONCE DATA IS PROCESSED, PUT DATA IN:
// [Project Directory]/Data/Temperature/RawData

// CLICK HERE FOR LINK IN GEE:
// https://code.earthengine.google.com/bf8fdec9ba1dc3d8612d2dba516454c0

// Set to folder in Google Drive
var GEE_FOLDER = 'gee_extracts';

// =============================================================
// Function to Extract VIIRS Annual Average Data
var extract_temp = function(country_name, 
                            reduce_type,
                            begin_date,
                            end_date,
                            file_name, 
                            google_drive_folder){
  // Extracts temperature data
  
  // Args:
  //   country_name: Country to export
  //   begin_date: Beginning data for VIIRS image.
  //   end_date: End date for VIIRS image.
  //   file_name: Name to call exported image
  //   google_drive_folder: Name of folder to export image
  
  // Get country feature
  var countries = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017');
  var country = countries.filter(ee.Filter.eq('country_na', country_name)).geometry();
  
  // Load VIIRS ImageCollection and filter by date
  var image = ee.ImageCollection('NASA/FLDAS/NOAH01/C/GL/M/V001');
  image = ee.ImageCollection(
    image.filterDate(begin_date,end_date)
  );

  if(reduce_type == "mean"){
    image = image.reduce(ee.Reducer.mean()); 
    image = image.select('Tair_f_tavg_mean');
  }
  
  if(reduce_type == "min"){
    image = image.reduce(ee.Reducer.min()); 
    image = image.select('Tair_f_tavg_min');
  }
  
  if(reduce_type == "max"){
    image = image.reduce(ee.Reducer.max()); 
    image = image.select('Tair_f_tavg_max');
  }

  // Clip to country and export image to google drive
  image = image.clip(country);

  Export.image.toDrive({
    folder: google_drive_folder,
    image: image,
    scale: 10000,
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
  var temp = extract_temp('Ethiopia', 'mean', yr + '-01-01', yr + '-12-31','eth_temp_' + yr + '_avg_FLDAS_10km', GEE_FOLDER);
  var temp = extract_temp('Ethiopia', 'min',  yr + '-01-01', yr + '-12-31','eth_temp_' + yr + '_min_FLDAS_10km', GEE_FOLDER);
  var temp = extract_temp('Ethiopia', 'max',  yr + '-01-01', yr + '-12-31','eth_temp_' + yr + '_max_FLDAS_10km', GEE_FOLDER);
}

// =============================================================
// Map a Images to Check How they Look 
var Viz = {min: 1, max: 400, palette: ['000000', 'FFFF00'], opacity: 1};
Map.addLayer(temp, Viz, 'Temperature');














