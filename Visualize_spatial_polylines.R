library(dggridR)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Wrap the ggplot lines into a function
set.plot = function (vertexgrid,ISEAdf,titletext) {
  st.plot = ggplot() +
    # cell and cell boundaries
    geom_polygon(data=vertexgrid, aes(x=long, y=lat, group=group, color="gray60", alpha=0.2)) +
    geom_path (data=vertexgrid, aes(x=long, y=lat, group=group),color="white") +
    # cell centroids
    geom_point (data=ISEAdf, aes(x=Cellcenter_lon, y=Cellcenter_lat),color = "gray30") +
    # converted street
    geom_line (data=ISEAdf, aes(x=Cellcenter_lon, y=Cellcenter_lat), size = 1, colour = "gray30") +
    # Original street with 14 vertices
    geom_line(data = OriStreet, aes(x=longitude, y=latitude), size = 1, colour = "red") +
    coord_equal() + labs(title = titletext) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
          legend.position="none", plot.title=element_text(face="bold",size=12,hjust = 0.5),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  return (st.plot)
}

street.plot = function (proj,aperture,topology,res1,res2,res3,input.df) {
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
  # Filter the datasets
  df1 = filter(input.df,res == res1)
  df2 = filter(input.df,res == res2)
  df3 = filter(input.df,res == res3)
  # Get the boundary vertices of each cell
  vertex.grid1 = dgcellstogrid(DGG1,df1$CellAddress,frame=TRUE,wrapcells=TRUE)
  vertex.grid2 = dgcellstogrid(DGG2,df2$CellAddress,frame=TRUE,wrapcells=TRUE)
  vertex.grid3 = dgcellstogrid(DGG3,df3$CellAddress,frame=TRUE,wrapcells=TRUE)
  # Here start to plot
  a = set.plot(vertex.grid1,df1,paste("Resolution Level =", toString(res1), sep=" "))
  b = set.plot(vertex.grid2,df2,paste("Resolution Level =", toString(res2), sep=" "))
  c = set.plot(vertex.grid3,df3,paste("Resolution Level =", toString(res3), sep=" "))
  com.plot = grid.arrange(a,b,c, nrow=3)
  return (com.plot)
}

street.plot("ISEA",3,"HEXAGON",25,26,27,ISEA3H.df)
street.plot("ISEA",4,"HEXAGON",21,22,23,ISEA4H.df)
street.plot("ISEA",4,"TRIANGLE",21,22,23,ISEA4T.df)
street.plot("ISEA",4,"DIAMOND",21,22,23,ISEA4D.df)

