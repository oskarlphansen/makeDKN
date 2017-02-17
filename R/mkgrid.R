#DKN CRS
DKNcrs <- function(){
  #Define CRS
  crs = sp::CRS("+init=epsg:32632")
  crs
}

#DKN extent
DKNextent <- function(){
  #DS corners
  ymin = 6000000
  xmin = 400000
  ymax = 6400000
  xmax = 900000

  # Create spatial element from coordinates
  m <- matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE)
  p <- sp::Polygon(m, hole = FALSE)
  ps <- sp::Polygons(list(p), 1)
  sps <-sp::SpatialPolygons(list(ps), proj4string = DKNcrs())
  return(sps)
}


#mk DKN grid
#
# This function create a DKN-grid with the parameters: extent, resolution (in meters)
#
# Future idea: Choose output to be either raster or polygon.
mkDKN <- function(ext, res){

  #Check extent-layer and use spTransform and message, if not in the same CRS.
  if(!inherits(ext, "Spatial")){stop("Extent layer must be a spatial.")}
  if(!sp::proj4string(DKNextent()) == sp::proj4string(ext)){
    ext <-sp::spTransform(ext, DKNcrs())
    warning(paste("CRS of input extent not the same as CRS of DKN. It was changed to", DKNcrs(), sep= " "))}

  #Check if extent-layer is inside DS-corners
  if(!rgeos::gCovers(DKNextent(), ext)){stop("Extent layer must be within the extent of the DKN definition.")}

  # Create an empty raster with defined resolution and DS-extent
  grid <- raster::raster(raster::extent(DKNextent()), crs = DKNcrs(), resolution = res)

  # Transform this raster into a polygon inside extent.
  gridpolygon <- raster::rasterToPolygons(raster::crop(grid, raster::extent(ext), snap = "out"))

  # Remove cells with no overlap.
  gridpolygon <- gridpolygon[ext, ]

  # Name grid-points

  ##Get centroids
  centroids <- as.data.frame.matrix(sp::coordinates(rgeos::gCentroid(gridpolygon, byid = T)))
  centroids <- centroids/100000
  cent.names <- paste(centroids$x, centroids$y, sep = "_")
  cent.names <- gsub(".", "", cent.names, fixed = T)
  cent.names <- paste(paste(res, "m", sep = ""), cent.names, sep = "_")
  gridpolygon@data <- data.frame("dkname" = cent.names)
  return(gridpolygon)
}
