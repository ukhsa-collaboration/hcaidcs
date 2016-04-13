library("rgdal") # librarys sp, will use proj.4 if installed
library("maptools")
library("ggplot2")
library("plyr")

setwd("H:\\spatial\\")

area_teams_sp <- readOGR(dsn = ".", layer = "AT_Eng")
area_teams_sp@data$id = rownames(area_teams_sp@data)
at_points <- fortify(area_teams_sp)
at_sp_df <- join(at_points, area_teams_sp@data, by = "id")

ggplot(at_sp_df, aes(long, lat, group = group)) + geom_polygon() +
  geom_path(colour = "white") + coord_equal()

save(at_sp_df, file = "at_sp_df.RData")