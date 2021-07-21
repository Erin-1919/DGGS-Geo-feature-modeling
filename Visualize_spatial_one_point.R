library(dggridR)
library(ggplot2)
library(gridExtra)

point.plot = function (longitude,latitude,proj,aperture,topology,res1,res2,res3,titletext) {
  # Construct a dgg object 
  DGG1 = dgconstruct(proj, aperture, topology, res1, precision = 7,
                    resround = "nearest", metric = TRUE, show_info = TRUE,
                    azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)
  DGG2 = dgconstruct(proj, aperture, topology, res2, precision = 7,
                     resround = "nearest", metric = TRUE, show_info = TRUE,
                     azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)
  DGG3 = dgconstruct(proj, aperture, topology, res3, precision = 7,
                     resround = "nearest", metric = TRUE, show_info = TRUE,
                     azimuth_deg = 0, pole_lat_deg = 58.28252559, pole_lon_deg = 11.25)
  # Convert the lat&lon of points to corresponding linear address of grid cells
  Cell_address1 = dgGEO_to_SEQNUM(DGG1,longitude,latitude)$seqnum
  Cell_address2 = dgGEO_to_SEQNUM(DGG2,longitude,latitude)$seqnum
  Cell_address3 = dgGEO_to_SEQNUM(DGG3,longitude,latitude)$seqnum
  # Get the boundary vertices of each cell
  vertex.grid1 = dgcellstogrid(DGG1,Cell_address1,frame=TRUE,wrapcells=TRUE)
  vertex.grid2 = dgcellstogrid(DGG2,Cell_address2,frame=TRUE,wrapcells=TRUE)
  vertex.grid3 = dgcellstogrid(DGG3,Cell_address3,frame=TRUE,wrapcells=TRUE)
  # Get cell centroids coords and form dataframes
  df = df1 = df2 = df3 = data.frame(Cellcenter_lon = double(),Cellcenter_lat = double())
  df[1,1] = longitude
  df[1,2] = latitude
  Cellcenter_lon1 = dgSEQNUM_to_GEO(DGG1,Cell_address1)$lon_deg
  Cellcenter_lat1 = dgSEQNUM_to_GEO(DGG1,Cell_address1)$lat_deg
  df1[1,1] = Cellcenter_lon1
  df1[1,2] = Cellcenter_lat1
  Cellcenter_lon2 = dgSEQNUM_to_GEO(DGG2,Cell_address2)$lon_deg
  Cellcenter_lat2 = dgSEQNUM_to_GEO(DGG2,Cell_address2)$lat_deg
  df2[1,1] = Cellcenter_lon2
  df2[1,2] = Cellcenter_lat2
  Cellcenter_lon3 = dgSEQNUM_to_GEO(DGG3,Cell_address3)$lon_deg
  Cellcenter_lat3 = dgSEQNUM_to_GEO(DGG3,Cell_address3)$lat_deg
  df3[1,1] = Cellcenter_lon3
  df3[1,2] = Cellcenter_lat3
  # Start to plot
  pt.plot = ggplot() +
    # cell and cell boundaries
    geom_polygon(data=vertex.grid1, aes(x=long, y=lat, group=group, color="gray60", alpha=0.2)) +
    geom_path (data=vertex.grid1, aes(x=long, y=lat, group=group),color="gray60") +
    # cell centroids
    geom_point (data=df1, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
    # cell and cell boundaries
    geom_polygon(data=vertex.grid2, aes(x=long, y=lat, group=group, color="gray60", alpha=0.5)) +
    geom_path (data=vertex.grid2, aes(x=long, y=lat, group=group),color="gray60") +
    # cell centroids
    geom_point (data=df2, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
    # cell and cell boundaries
    geom_polygon(data=vertex.grid3, aes(x=long, y=lat, group=group, color="gray60", alpha=0.8)) +
    geom_path (data=vertex.grid3, aes(x=long, y=lat, group=group),color="gray60") +
    # cell centroids
    geom_point (data=df3, aes(x=Cellcenter_lon, y=Cellcenter_lat),size = 2,color = "black") +
    # Original points
    geom_point(data = df, aes(x=Cellcenter_lon, y=Cellcenter_lat), size = 2, colour = "red") +
    coord_equal() + labs(title = titletext) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
          legend.position="none", plot.title=element_text(face="bold",size=12,hjust = 0.5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  return (pt.plot)
}

grid.arrange(point.plot (-114.0555,51.04818,"ISEA",3,"HEXAGON",24,25,26,"ISEA3H"),
             point.plot (-114.0555,51.04818,"ISEA",4,"HEXAGON",24,25,26,"ISEA4H"),
             point.plot (-114.0555,51.04818,"ISEA",4,"TRIANGLE",24,25,26,"ISEA4T"), 
             point.plot (-114.0555,51.04818,"ISEA",4,"DIAMOND",24,25,26,"ISEA4D"),nrow=2)
