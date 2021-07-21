library(dggridR)
library(rgdal)
library(geosphere)
library(plotrix)
library(ggplot2)
library(gridExtra)

# Read point datasets
Cameras = read.csv("Data/TrafficCameras.csv")
str(Cameras)

# create a vector to store tested resolution levels
res.levels = (21:30)

## Create a function to return the mean/max/min values of distances
cell.calculate.stats = function(proj,aperture,topology,res,input.df) {
  # Construct a dgg object 
  DGG = dgconstruct(proj, aperture, topology, res, precision = 7,
                    resround = "nearest", metric = TRUE, show_info = TRUE,
                    azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)
  # Convert the lat&lon of points to corresponding linear address of grid cells
  Cell_address = dgGEO_to_SEQNUM(DGG,input.df$longitude,input.df$latitude)$seqnum
  # Convert the linear address of grid cells to lat&lon which are the center coordinates of the cells
  Cellcenter_lon = dgSEQNUM_to_GEO(DGG,Cell_address)$lon_deg
  Cellcenter_lat = dgSEQNUM_to_GEO(DGG,Cell_address)$lat_deg
  # Store the lat/lon as matrix which is needed inputs for distGeo
  Coords.point = as.matrix(cbind(input.df$longitude,input.df$latitude))
  Coords.cell = as.matrix(cbind(Cellcenter_lon, Cellcenter_lat))
  # Calculate the distances between original points and the converted points (in meter)
  # The geodesic distance is calculated based on WGS84 ellipsoid
  input.df$res = rep(res, nrow(input.df))
  input.df$delta.dist = distGeo(Coords.point,Coords.cell)
  input.df$CellAddress = Cell_address
  return(input.df)
}

ISEA3H.df = Cameras[FALSE,]

## Invoke the function in a loop
for(i in res.levels) {
  ISEA3H.df = rbind(ISEA3H.df, cell.calculate.stats("ISEA",3,"HEXAGON",i,Cameras))
}

ISEA4H.df = Cameras[FALSE,]
for(i in res.levels) {
  ISEA4H.df = rbind(ISEA4H.df, cell.calculate.stats("ISEA",4,"HEXAGON",i,Cameras))
}

ISEA4T.df = Cameras[FALSE,]
for(i in res.levels) {
  ISEA4T.df = rbind(ISEA4T.df, cell.calculate.stats("ISEA",4,"TRIANGLE",i,Cameras))
}

ISEA4D.df = Cameras[FALSE,]
for(i in res.levels) {
  ISEA4D.df = rbind(ISEA4D.df, cell.calculate.stats("ISEA",4,"DIAMOND",i,Cameras))
}

