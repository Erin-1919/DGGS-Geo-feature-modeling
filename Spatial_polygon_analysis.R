library(dggridR)
library(rgdal)
library(geosphere)
library(plotrix)
library(ggplot2)
library(gridExtra)
library(dplyr)

LC.ISEA3H = read.csv("Result/LandCoverISEA3H.csv")
LC.ISEA4H = read.csv("Result/LandCoverISEA4H.csv")
LC.ISEA4T = read.csv("Result/LandCoverISEA4T.csv")
LC.ISEA4D = read.csv("Result/LandCoverISEA4D.csv")

## Here we visualize how absolute delta area changes at different resolution
stats.sum.df1 = LC.ISEA3H %>%
  group_by(Resolution) %>% 
  summarise(DiffArea.avg = mean(DiffArea),
            DiffArea.sd = sd(DiffArea))
df.plot1 = ggplot(data=stats.sum.df1, aes(x=Resolution)) + 
  geom_point(aes(y = DiffArea.avg), size = 3)+
  geom_line(aes(y = DiffArea.avg, linetype = "a"), size = 1) +
  geom_errorbar(aes(ymin=DiffArea.avg-DiffArea.sd, ymax=DiffArea.avg+DiffArea.sd), width=0.15, size=1) +
  scale_y_continuous(name = "Absolute Delta Area (m2)",
                     breaks = seq(-5000, 10000, by = 3000),limits=c(-5000, 10000)) +
  scale_x_continuous(name = "Resolution Levels",expand = c(0, 0),
                     breaks = seq(24, 28, by = 1),limits=c(23.5, 28.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black")) +
  labs(title = "Average Delta Area of Land Cover Comparing Original and Converted \n Polygons of ISEA3H Grids at Resolution Levels from 24 to 28")

df.plot1

stats.sum.df2 = LC.ISEA4H %>%
  group_by(Resolution) %>% 
  summarise(DiffArea.avg = mean(DiffArea),
            DiffArea.sd = sd(DiffArea))
df.plot2 = ggplot(data=stats.sum.df2, aes(x=Resolution)) + 
  geom_point(aes(y = DiffArea.avg), size = 3)+
  geom_line(aes(y = DiffArea.avg, linetype = "a"), size = 1) +
  geom_errorbar(aes(ymin=DiffArea.avg-DiffArea.sd, ymax=DiffArea.avg+DiffArea.sd), width=0.15, size=1) +
  scale_y_continuous(name = "Absolute Delta Area (m2)",
                     breaks = seq(-10000, 20000, by = 5000),limits=c(-10000, 20000)) +
  scale_x_continuous(name = "Resolution Levels",expand = c(0, 0),
                     breaks = seq(18, 22, by = 1),limits=c(17.5, 22.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black")) +
  labs(title = "Average Delta Area of Land Cover Comparing Original and Converted \n Polygons of ISEA4H Grids at Resolution Levels from 24 to 28")

df.plot2

stats.sum.df3 = LC.ISEA4T %>%
  group_by(Resolution) %>% 
  summarise(DiffArea.avg = mean(DiffArea),
            DiffArea.sd = sd(DiffArea))
df.plot3 = ggplot(data=stats.sum.df3, aes(x=Resolution)) + 
  geom_point(aes(y = DiffArea.avg), size = 3)+
  geom_line(aes(y = DiffArea.avg, linetype = "a"), size = 1) +
  geom_errorbar(aes(ymin=DiffArea.avg-DiffArea.sd, ymax=DiffArea.avg+DiffArea.sd), width=0.15, size=1) +
  scale_y_continuous(name = "Absolute Delta Area (m2)",
                     breaks = seq(-10000, 28000, by = 3800),limits=c(-10000, 28000)) +
  scale_x_continuous(name = "Resolution Levels",expand = c(0, 0),
                     breaks = seq(17, 21, by = 1),limits=c(16.5, 21.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black")) +
  labs(title = "Average Delta Area of Land Cover Comparing Original and Converted \n Polygons of ISEA4T Grids at Resolution Levels from 24 to 28")

df.plot3

stats.sum.df4 = LC.ISEA4D %>%
  group_by(Resolution) %>% 
  summarise(DiffArea.avg = mean(DiffArea),
            DiffArea.sd = sd(DiffArea))
df.plot4 = ggplot(data=stats.sum.df4, aes(x=Resolution)) + 
  geom_point(aes(y = DiffArea.avg), size = 3)+
  geom_line(aes(y = DiffArea.avg, linetype = "a"), size = 1) +
  geom_errorbar(aes(ymin=DiffArea.avg-DiffArea.sd, ymax=DiffArea.avg+DiffArea.sd), width=0.15, size=1) +
  scale_y_continuous(name = "Absolute Delta Area (m2)",
                     breaks = seq(-10000, 22000, by = 3200),limits=c(-10000, 22000)) +
  scale_x_continuous(name = "Resolution Levels",expand = c(0, 0),
                     breaks = seq(18, 22, by = 1),limits=c(17.5, 22.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none", text=element_text(size = 14),plot.title=element_text(face="bold",size=14,hjust = 0.5),
        axis.title = element_text(face="bold"), axis.line = element_line(colour = "black",size = 1),
        axis.text.x=element_text(size = 12, face="bold", colour = "black"),
        axis.text.y=element_text(size = 12, face="bold", colour = "black")) +
  labs(title = "Average Delta Area of Land Cover Comparing Original and Converted \n Polygons of ISEA4D Grids at Resolution Levels from 24 to 28")

df.plot4

grid.arrange(df.plot1, df.plot2, df.plot3, df.plot4, nrow=2)
