// Canopy height data
var canopy = ee.Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1');

// crop the image to the region of interest
var canopyAntioquia = canopy.clip(antioquia);

// transform NA data to -9999
var canopyFilled = canopyAntioquia.unmask(-9999);

// visualization 
Map.centerObject(antioquia, 8);
Map.addLayer(canopyAntioquia, {min:0, max:30}, 'Canopy Antioquia');

// export
Export.image.toDrive({
  image: canopyFilled,
  description: 'Canopy_Antioquia_10m',
  folder: 'earth_engine',
  fileNamePrefix: 'Canopy_Antioquia',
  region: antioquia.geometry(),
  scale: 10,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});