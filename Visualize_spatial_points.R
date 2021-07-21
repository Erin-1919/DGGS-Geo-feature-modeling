library(dggridR)
library(ggplot2)
library(gridExtra)

## Prepare
# Read point datasets
Camaras = read.csv("TrafficCamaras.csv")
# Camaras = read.csv("AllTrafficCamaras.csv")
str(Camaras)
# create a vector to store tested resolution levels
res.levels = (21:30)

## Create a new dataframe to store results
ISEA3H.df = ISEA4H.df = ISEA4T.df = ISEA4D.df = 
  data.frame(Address = character(), longitude = double(), latitude = double(),quad = character(),CellAddress = integer(),
             Cellcenter_lon = integer(), Cellcenter_lat = integer(), Resolution = integer())

## Creat a function to return the transformed vertex lat/lon
cell.convert = function(proj,aperture,topology,res,input.df) {
  # Construct a dgg object 
  DGG = dgconstruct(proj, aperture, topology, res, precision = 7,
                    resround = "nearest", metric = TRUE, show_info = TRUE,
                    azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)
  # Convert the lat&lon of points to corresponding linear address of grid cells
  Cell_address = dgGEO_to_SEQNUM(DGG,input.df$longitude,input.df$latitude)$seqnum
  # Convert the linear address of grid cells to lat&lon which are the center coordinates of the cells
  Cellcenter_lon = dgSEQNUM_to_GEO(DGG,Cell_address)$lon_deg
  Cellcenter_lat = dgSEQNUM_to_GEO(DGG,Cell_address)$lat_deg
  input.df$CellAddress = Cell_address
  input.df$Cellcenter_lon = Cellcenter_lon
  input.df$Cellcenter_lat = Cellcenter_lat
  input.df$res = res
  return(input.df)
}

## Invoke the function in a loop
for(i in res.levels) {
  ISEA3H.df = rbind(ISEA3H.df, cell.convert("ISEA",3,"HEXAGON",i,Camaras))
}

for(i in res.levels) {
  ISEA4H.df = rbind(ISEA4H.df, cell.convert("ISEA",4,"HEXAGON",i,Camaras))
}

for(i in res.levels) {
  ISEA4T.df = rbind(ISEA4T.df, cell.convert("ISEA",4,"TRIANGLE",i,Camaras))
}

for(i in res.levels) {
  ISEA4D.df = rbind(ISEA4D.df, cell.convert("ISEA",4,"DIAMOND",i,Camaras))
}


## Rename the fields
names(ISEA3H.df) = names(ISEA4H.df) = names(ISEA4T.df) = names(ISEA4D.df) = 
  c("Address","latitude","longitude","quad","CellAddress","Cellcenter_lon","Cellcenter_lat","Resolution")


# We show the results of resolution levels 25-27

dggs.ISEA3H.25 = dgconstruct(projection = "ISEA", aperture = 3, topology = "HEXAGON",
                             res = 25, precision = 7, area = NA, spacing = NA, cls = NA,
                             resround = "nearest", metric = TRUE, show_info = TRUE,
                             azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)

dggs.ISEA4H.25 = dgconstruct(projection = "ISEA", aperture = 4, topology = "HEXAGON",
                             res = 25, precision = 7, area = NA, spacing = NA, cls = NA,
                             resround = "nearest", metric = TRUE, show_info = TRUE,
                             azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)

dggs.ISEA4T.25 = dgconstruct(projection = "ISEA", aperture = 4, topology = "TRIANGLE",
                             res = 25, precision = 7, area = NA, spacing = NA, cls = NA,
                             resround = "nearest", metric = TRUE, show_info = TRUE,
                             azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)

dggs.ISEA4D.25 = dgconstruct(projection = "ISEA", aperture = 4, topology = "DIAMOND",
                             res = 25, precision = 7, area = NA, spacing = NA, cls = NA,
                             resround = "nearest", metric = TRUE, show_info = TRUE,
                             azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)

dggs.ISEA3H.26 = dgsetres(dggs.ISEA3H.25,26)
dggs.ISEA3H.27 = dgsetres(dggs.ISEA3H.26,27)

dggs.ISEA4H.26 = dgsetres(dggs.ISEA4H.25,26)
dggs.ISEA4H.27 = dgsetres(dggs.ISEA4H.26,27)

dggs.ISEA4T.26 = dgsetres(dggs.ISEA4T.25,26)
dggs.ISEA4T.27 = dgsetres(dggs.ISEA4T.26,27)

dggs.ISEA4D.26 = dgsetres(dggs.ISEA4D.25,26)
dggs.ISEA4D.27 = dgsetres(dggs.ISEA4D.26,27)


# Here we use one point (Address = "5 Avenue / 3 Street SE") as an example
# Filter the datasets
ISEA3H.25.df = filter(ISEA3H.df,Resolution == 25 & Address == "5 Avenue / 3 Street SE")
ISEA3H.26.df = filter(ISEA3H.df,Resolution == 26 & Address == "5 Avenue / 3 Street SE")
ISEA3H.27.df = filter(ISEA3H.df,Resolution == 27 & Address == "5 Avenue / 3 Street SE")

ISEA4H.25.df = filter(ISEA3H.df,Resolution == 25 & Address == "5 Avenue / 3 Street SE")
ISEA4H.26.df = filter(ISEA3H.df,Resolution == 26 & Address == "5 Avenue / 3 Street SE")
ISEA4H.27.df = filter(ISEA3H.df,Resolution == 27 & Address == "5 Avenue / 3 Street SE")

ISEA4T.25.df = filter(ISEA4T.df,Resolution == 25 & Address == "5 Avenue / 3 Street SE")
ISEA4T.26.df = filter(ISEA4T.df,Resolution == 26 & Address == "5 Avenue / 3 Street SE")
ISEA4T.27.df = filter(ISEA4T.df,Resolution == 27 & Address == "5 Avenue / 3 Street SE")

ISEA4D.25.df = filter(ISEA4D.df,Resolution == 25 & Address == "5 Avenue / 3 Street SE")
ISEA4D.26.df = filter(ISEA4D.df,Resolution == 26 & Address == "5 Avenue / 3 Street SE")
ISEA4D.27.df = filter(ISEA4D.df,Resolution == 27 & Address == "5 Avenue / 3 Street SE")


# Here we find out the vertices of each cell
vertex.grid.ISEA3H.25 = dgcellstogrid(dggs.ISEA3H.25,ISEA3H.25.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA3H.26 = dgcellstogrid(dggs.ISEA3H.26,ISEA3H.26.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA3H.27 = dgcellstogrid(dggs.ISEA3H.27,ISEA3H.27.df$CellAddress,frame=TRUE,wrapcells=TRUE)

vertex.grid.ISEA4H.25 = dgcellstogrid(dggs.ISEA4H.25,ISEA4H.25.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA4H.26 = dgcellstogrid(dggs.ISEA4H.26,ISEA4H.26.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA4H.27 = dgcellstogrid(dggs.ISEA4H.27,ISEA4H.27.df$CellAddress,frame=TRUE,wrapcells=TRUE)

vertex.grid.ISEA4T.25 = dgcellstogrid(dggs.ISEA4T.25,ISEA4T.25.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA4T.26 = dgcellstogrid(dggs.ISEA4T.26,ISEA4T.26.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA4T.27 = dgcellstogrid(dggs.ISEA4T.27,ISEA4T.27.df$CellAddress,frame=TRUE,wrapcells=TRUE)

vertex.grid.ISEA4D.25 = dgcellstogrid(dggs.ISEA4D.25,ISEA4D.25.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA4D.26 = dgcellstogrid(dggs.ISEA4D.26,ISEA4D.26.df$CellAddress,frame=TRUE,wrapcells=TRUE)
vertex.grid.ISEA4D.27 = dgcellstogrid(dggs.ISEA4D.27,ISEA4D.27.df$CellAddress,frame=TRUE,wrapcells=TRUE)


# Wrap the ggplot lines into a function
point.plot = function (vertexgrid1,ISEAdf1,vertexgrid2,ISEAdf2,vertexgrid3,ISEAdf3,titletext) {
  Camara1 = filter(Camaras,ï..Address == "5 Avenue / 3 Street SE")
  pt.plot = ggplot() +
    # cell and cell boundaries
    geom_polygon(data=vertexgrid1, aes(x=long, y=lat, group=group, color="gray60", alpha=0.2)) +
    geom_path (data=vertexgrid1, aes(x=long, y=lat, group=group),color="gray60") +
    # cell centroids
    geom_point (data=ISEAdf1, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
    # cell and cell boundaries
    geom_polygon(data=vertexgrid2, aes(x=long, y=lat, group=group, color="gray60", alpha=0.5)) +
    geom_path (data=vertexgrid2, aes(x=long, y=lat, group=group),color="gray60") +
    # cell centroids
    geom_point (data=ISEAdf2, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
    # cell and cell boundaries
    geom_polygon(data=vertexgrid3, aes(x=long, y=lat, group=group, color="gray60", alpha=0.8)) +
    geom_path (data=vertexgrid3, aes(x=long, y=lat, group=group),color="gray60") +
    # cell centroids
    geom_point (data=ISEAdf3, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
    # Original 14 points
    geom_point(data = Camara1, aes(x=longitude, y=latitude), size = 2, colour = "red") +
    coord_equal() + labs(title = titletext) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
          legend.position="none", plot.title=element_text(face="bold",size=12,hjust = 0.5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  return (pt.plot)
}


point.plot(vertex.grid.ISEA3H.25,ISEA3H.25.df,
           vertex.grid.ISEA3H.26,ISEA3H.26.df,
           vertex.grid.ISEA3H.27,ISEA3H.27.df,"ISEA3H")

point.plot(vertex.grid.ISEA4H.25,ISEA4H.25.df,
           vertex.grid.ISEA4H.26,ISEA4H.26.df,
           vertex.grid.ISEA4H.27,ISEA4H.27.df,"ISEA4H")

point.plot(vertex.grid.ISEA4T.25,ISEA4T.25.df,
           vertex.grid.ISEA4T.26,ISEA4T.26.df,
           vertex.grid.ISEA4T.27,ISEA4T.27.df,"ISEA4T")

point.plot(vertex.grid.ISEA4D.25,ISEA4D.25.df,
           vertex.grid.ISEA4D.26,ISEA4D.26.df,
           vertex.grid.ISEA4D.27,ISEA4D.27.df,"ISEA4D")



grid.arrange(point.plot(vertex.grid.ISEA3H.25,ISEA3H.25.df,
                        vertex.grid.ISEA3H.26,ISEA3H.26.df,
                        vertex.grid.ISEA3H.27,ISEA3H.27.df,"ISEA3H"),
             point.plot(vertex.grid.ISEA4H.25,ISEA4H.25.df,
                        vertex.grid.ISEA4H.26,ISEA4H.26.df,
                        vertex.grid.ISEA4H.27,ISEA4H.27.df,"ISEA4H"),
             point.plot(vertex.grid.ISEA4T.25,ISEA4T.25.df,
                        vertex.grid.ISEA4T.26,ISEA4T.26.df,
                        vertex.grid.ISEA4T.27,ISEA4T.27.df,"ISEA4T"),
             point.plot(vertex.grid.ISEA4D.25,ISEA4D.25.df,
                        vertex.grid.ISEA4D.26,ISEA4D.26.df,
                        vertex.grid.ISEA4D.27,ISEA4D.27.df,"ISEA4D"),nrow=2)












Camara1 = filter(Camaras,ï..Address == "5 Avenue / 3 Street SE")
pt.plot1 = ggplot() +
  # cell and cell boundaries
  geom_polygon(data=vertex.grid.ISEA3H.25, aes(x=long, y=lat, group=group, color="gray60", alpha=0.2)) +
  geom_path (data=vertex.grid.ISEA3H.25, aes(x=long, y=lat, group=group),color="gray60") +
  # cell centroids
  geom_point (data=ISEA3H.25.df, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
  # cell and cell boundaries
  geom_polygon(data=vertex.grid.ISEA3H.26, aes(x=long, y=lat, group=group, color="gray60", alpha=0.5)) +
  geom_path (data=vertex.grid.ISEA3H.26, aes(x=long, y=lat, group=group),color="gray60") +
  # cell centroids
  geom_point (data=ISEA3H.26.df, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
  # cell and cell boundaries
  geom_polygon(data=vertex.grid.ISEA3H.27, aes(x=long, y=lat, group=group, color="gray60", alpha=0.8)) +
  geom_path (data=vertex.grid.ISEA3H.27, aes(x=long, y=lat, group=group),color="gray60") +
  # cell centroids
  geom_point (data=ISEA3H.27.df, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
  # Original 14 points
  geom_point(data = Camara1, aes(x=longitude, y=latitude), size = 2, colour = "red") +
  coord_equal() + labs(title = "titletext") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none", plot.title=element_text(face="bold",size=12,hjust = 0.5),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pt.plot1
