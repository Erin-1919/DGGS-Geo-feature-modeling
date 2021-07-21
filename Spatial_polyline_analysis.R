library(dggridR)
library(rgdal)
library(geosphere)
library(plotrix)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyverse)

# create a vector to store tested tested orientation
azimuth = seq(-90,90,by=15)

# create empty dataframes
ISEA3H24.stats.df = ISEA3H25.stats.df = ISEA3H26.stats.df = ISEA3H27.stats.df = ISEA3H28.stats.df = 
  ISEA4H18.stats.df = ISEA4H19.stats.df = ISEA4H20.stats.df = ISEA4H21.stats.df = ISEA4H22.stats.df = 
  ISEA4T17.stats.df = ISEA4T18.stats.df = ISEA4T19.stats.df = ISEA4T20.stats.df = ISEA4T21.stats.df = 
  ISEA4D18.stats.df = ISEA4D19.stats.df = ISEA4D20.stats.df = ISEA4D21.stats.df = ISEA4D22.stats.df = 
  data.frame(Azimuth = integer(),Resolution = integer(),Orig.length = double(),Convert.length = double(),
             Delta.length = double(),Delta.length.abs = double(),Delta.length.pct = double())

## Create a function to return the transformed vertex lat/lon
cell.convert = function(azimuthdeg,proj,aperture,topology,res,input.df) {
  # Construct a dgg object 
  DGG = dgconstruct(proj, aperture, topology, res, precision = 7,
                    azimuth_deg = 0, pole_lat_deg = azimuthdeg, pole_lon_deg = 11.25)
  # Convert the lat&lon of points to corresponding linear address of grid cells
  Cell_address = dgGEO_to_SEQNUM(DGG,input.df$lon,input.df$lat)$seqnum
  # Convert the linear address of grid cells to lat&lon which are the center coordinates of the cells
  Cellcenter_lon = dgSEQNUM_to_GEO(DGG,Cell_address)$lon_deg
  Cellcenter_lat = dgSEQNUM_to_GEO(DGG,Cell_address)$lat_deg
  input.df$CellAddress = Cell_address
  input.df$Cellcenter_lon = Cellcenter_lon
  input.df$Cellcenter_lat = Cellcenter_lat
  input.df$res = rep(res, nrow(input.df))
  input.df$azimuth = rep(azimuthdeg, nrow(input.df))
  input.df = input.df[!duplicated(input.df$CellAddress),]
  return(input.df)
}

## Create a function to calculate the line length in the converted dggs grids, along with some stats
polyline.length = function(azimuthdeg,input.df,res){
  input.df = filter(input.df,azimuth == azimuthdeg)
  streetvertex.convert.matrix = input.df[,c("Cellcenter_lon","Cellcenter_lat")]
  # Calculate the original length and the length after convension
  Orig.length = 527.9683
  Convert.length = lengthLine(streetvertex.convert.matrix)
  # Calculate some stats
  Delta.length = Orig.length - Convert.length
  Delta.length.abs = abs(Delta.length)
  Delta.length.pct = Delta.length.abs/Orig.length
  results = c(azimuthdeg,res,Orig.length,Convert.length,Delta.length,Delta.length.abs,Delta.length.pct)
  return(results)
}

## ISEA3H28  ISEA4H22  ISEA4D22 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_05m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA3H28.df = ISEA4H22.df = ISEA4D22.df = StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA3H28.df = rbind(ISEA3H28.df, cell.convert(a,"ISEA",3,"HEXAGON",28,StreetsVertex))
  ISEA4H22.df = rbind(ISEA4H22.df, cell.convert(a,"ISEA",4,"HEXAGON",22,StreetsVertex))
  ISEA4D22.df = rbind(ISEA4D22.df, cell.convert(a,"ISEA",4,"DIAMOND",22,StreetsVertex))
}

for(a in azimuth) {
  ISEA3H28.stats.df = rbind(ISEA3H28.stats.df, polyline.length(a,ISEA3H28.df,28))
  ISEA4H22.stats.df = rbind(ISEA4H22.stats.df, polyline.length(a,ISEA4H22.df,22))
  ISEA4D22.stats.df = rbind(ISEA4D22.stats.df, polyline.length(a,ISEA4D22.df,22))
}

# Rename the fields
names(ISEA3H28.stats.df) = names(ISEA4H22.stats.df) = names(ISEA4D22.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA3H27  ISEA4T21 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_1m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA3H27.df = ISEA4T21.df = StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA3H27.df = rbind(ISEA3H27.df, cell.convert(a,"ISEA",3,"HEXAGON",27,StreetsVertex))
  ISEA4T21.df = rbind(ISEA4T21.df, cell.convert(a,"ISEA",4,"TRIANGLE",21,StreetsVertex))
}

for(a in azimuth) {
  ISEA3H27.stats.df = rbind(ISEA3H27.stats.df, polyline.length(a,ISEA3H27.df,27))
  ISEA4T21.stats.df = rbind(ISEA4T21.stats.df, polyline.length(a,ISEA4T21.df,21))
}

# Rename the fields
names(ISEA3H27.stats.df) = names(ISEA4T21.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA4H21  ISEA4D21  ISEA4T20 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_1-5m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA4H21.df = ISEA4D21.df = ISEA4T20.df = StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA4H21.df = rbind(ISEA4H21.df, cell.convert(a,"ISEA",4,"HEXAGON",21,StreetsVertex))
  ISEA4D21.df = rbind(ISEA4D21.df, cell.convert(a,"ISEA",4,"DIAMOND",21,StreetsVertex))
  ISEA4T20.df = rbind(ISEA4T20.df, cell.convert(a,"ISEA",4,"TRIANGLE",20,StreetsVertex))
}

for(a in azimuth) {
  ISEA4H21.stats.df = rbind(ISEA4H21.stats.df, polyline.length(a,ISEA4H21.df,21))
  ISEA4D21.stats.df = rbind(ISEA4D21.stats.df, polyline.length(a,ISEA4D21.df,21))
  ISEA4T20.stats.df = rbind(ISEA4T20.stats.df, polyline.length(a,ISEA4T20.df,20))
}

# Rename the fields
names(ISEA4H21.stats.df) = names(ISEA4D21.stats.df) = names(ISEA4T20.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA3H26 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_2m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA3H26.df =  StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA3H26.df = rbind(ISEA3H26.df, cell.convert(a,"ISEA",3,"HEXAGON",26,StreetsVertex))
}

for(a in azimuth) {
  ISEA3H26.stats.df = rbind(ISEA3H26.stats.df, polyline.length(a,ISEA3H26.df,26))
}

# Rename the fields
names(ISEA3H26.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA3H25  ISEA4H20  ISEA4D20 ISEA4T19 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_3m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA3H25.df = ISEA4H20.df = ISEA4D20.df = ISEA4T19.df = StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA3H25.df = rbind(ISEA3H25.df, cell.convert(a,"ISEA",3,"HEXAGON",25,StreetsVertex))
  ISEA4H20.df = rbind(ISEA4H20.df, cell.convert(a,"ISEA",4,"HEXAGON",20,StreetsVertex))
  ISEA4D20.df = rbind(ISEA4D20.df, cell.convert(a,"ISEA",4,"DIAMOND",20,StreetsVertex))
  ISEA4T19.df = rbind(ISEA4T19.df, cell.convert(a,"ISEA",4,"TRIANGLE",19,StreetsVertex))
}

for(a in azimuth) {
  ISEA3H25.stats.df = rbind(ISEA3H25.stats.df, polyline.length(a,ISEA3H25.df,25))
  ISEA4H20.stats.df = rbind(ISEA4H20.stats.df, polyline.length(a,ISEA4H20.df,20))
  ISEA4D20.stats.df = rbind(ISEA4D20.stats.df, polyline.length(a,ISEA4D20.df,20))
  ISEA4T19.stats.df = rbind(ISEA4T19.stats.df, polyline.length(a,ISEA4T19.df,19))
}

# Rename the fields
names(ISEA3H25.stats.df) = names(ISEA4H20.stats.df) = names(ISEA4D20.stats.df) = names(ISEA4T19.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA3H24  ISEA4H19  ISEA4D19 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_6m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA3H24.df = ISEA4H19.df = ISEA4D19.df = StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA3H24.df = rbind(ISEA3H24.df, cell.convert(a,"ISEA",3,"HEXAGON",24,StreetsVertex))
  ISEA4H19.df = rbind(ISEA4H19.df, cell.convert(a,"ISEA",4,"HEXAGON",19,StreetsVertex))
  ISEA4D19.df = rbind(ISEA4D19.df, cell.convert(a,"ISEA",4,"DIAMOND",19,StreetsVertex))
}

for(a in azimuth) {
  ISEA3H24.stats.df = rbind(ISEA3H24.stats.df, polyline.length(a,ISEA3H24.df,24))
  ISEA4H19.stats.df = rbind(ISEA4H19.stats.df, polyline.length(a,ISEA4H19.df,19))
  ISEA4D19.stats.df = rbind(ISEA4D19.stats.df, polyline.length(a,ISEA4D19.df,19))
}

# Rename the fields
names(ISEA3H24.stats.df) = names(ISEA4H19.stats.df) = names(ISEA4D19.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA4T18 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_7m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA4T18.df =  StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA4T18.df = rbind(ISEA4T18.df, cell.convert(a,"ISEA",4,"TRIANGLE",18,StreetsVertex))
}

for(a in azimuth) {
  ISEA4T18.stats.df = rbind(ISEA4T18.stats.df, polyline.length(a,ISEA4T18.df,18))
}

# Rename the fields
names(ISEA4T18.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## ISEA4H18  ISEA4D18  ISEA4T17 ##

# Read point datasets
StreetsVertex = read.csv("Result/Split_points_13m.csv")[ ,c('ORIG_FID', 'lat','lon')]
StreetsVertex = filter(StreetsVertex,ORIG_FID == 29)
# Invoke the function in a loop of street ID
ISEA4H18.df = ISEA4D18.df = ISEA4T17.df = StreetsVertex[FALSE,]
for(a in azimuth) {
  ISEA4H18.df = rbind(ISEA4H18.df, cell.convert(a,"ISEA",4,"HEXAGON",18,StreetsVertex))
  ISEA4D18.df = rbind(ISEA4D18.df, cell.convert(a,"ISEA",4,"DIAMOND",18,StreetsVertex))
  ISEA4T17.df = rbind(ISEA4T17.df, cell.convert(a,"ISEA",4,"TRIANGLE",17,StreetsVertex))
}

for(a in azimuth) {
  ISEA4H18.stats.df = rbind(ISEA4H18.stats.df, polyline.length(a,ISEA4H18.df,18))
  ISEA4D18.stats.df = rbind(ISEA4D18.stats.df, polyline.length(a,ISEA4D18.df,18))
  ISEA4T17.stats.df = rbind(ISEA4T17.stats.df, polyline.length(a,ISEA4T17.df,17))
}

# Rename the fields
names(ISEA4H18.stats.df) = names(ISEA4D18.stats.df) = names(ISEA4T17.stats.df) =
  c("Azimuth","Resolution","OrigLength","ConvertLength","DeltaLength","DeltaLengthABS","DeltaLengthPCT")

## Visualize the results

ISEA3H.df = rbind(ISEA3H24.stats.df,ISEA3H25.stats.df,ISEA3H26.stats.df,ISEA3H27.stats.df,ISEA3H28.stats.df)
ISEA4H.df = rbind(ISEA4H18.stats.df,ISEA4H19.stats.df,ISEA4H20.stats.df,ISEA4H21.stats.df,ISEA4H22.stats.df)
ISEA4T.df = rbind(ISEA4T17.stats.df,ISEA4T18.stats.df,ISEA4T19.stats.df,ISEA4T20.stats.df,ISEA4T21.stats.df)
ISEA4D.df = rbind(ISEA4D18.stats.df,ISEA4D19.stats.df,ISEA4D20.stats.df,ISEA4D21.stats.df,ISEA4D22.stats.df)

ISEA3H.plot = ggplot(data = ISEA3H.df, aes(x = Azimuth, y = DeltaLengthABS)) +
  geom_line(aes(group = Resolution,colour = factor(Resolution)),size=1.5) +
  scale_y_continuous(name = "Absolute Delta Length (m)",
                     breaks = seq(0, 120, by = 20),limits=c(0, 125)) +
  scale_x_continuous(name = "Latitude of the pole (°)",
                     breaks = seq(-90, 90, by = 15),limits=c(-91, 91)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="top", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold")) 

ISEA4H.plot = ggplot(data = ISEA4H.df, aes(x = Azimuth, y = DeltaLengthABS)) +
  geom_line(aes(group = Resolution,colour = factor(Resolution)),size=1.5) +
  scale_y_continuous(name = "Absolute Delta Length (m)",
                     breaks = seq(0, 140, by = 20),limits=c(0, 140)) +
  scale_x_continuous(name = "Latitude of the pole (°)",
                     breaks = seq(-90, 90, by = 15),limits=c(-91, 91)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="top", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"))

ISEA4T.plot = ggplot(data = ISEA4T.df, aes(x = Azimuth, y = DeltaLengthABS)) +
  geom_line(aes(group = Resolution,colour = factor(Resolution)),size=1.5) +
  scale_y_continuous(name = "Absolute Delta Length (m)",
                     breaks = seq(0, 180, by = 20),limits=c(0, 180)) +
  scale_x_continuous(name = "Latitude of the pole (°)",
                     breaks = seq(-90, 90, by = 15),limits=c(-91, 91)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="top", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold")) 

ISEA4D.plot = ggplot(data = ISEA4D.df, aes(x = Azimuth, y = DeltaLengthABS)) +
  geom_line(aes(group = Resolution,colour = factor(Resolution)),size=1.5) +
  scale_y_continuous(name = "Absolute Delta Length (m)",
                     breaks = seq(0, 280, by = 40),limits=c(0, 280)) +
  scale_x_continuous(name = "Latitude of the pole (°)",
                     breaks = seq(-90, 90, by = 15),limits=c(-91, 91)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="top", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold")) 

grid.arrange(ISEA3H.plot, ISEA4H.plot, ISEA4T.plot, ISEA4D.plot, nrow=2)


