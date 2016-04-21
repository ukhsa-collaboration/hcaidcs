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

rm(list = ls())

subregions <- readOGR(dsn = ".", layer = "nhs_subregions")
subregions@data$id = rownames(subregions@data)
subregions_points <- fortify(subregions)
subregions_sp_df <- join(subregions_points, subregions@data, by = "id")

ggplot(subregions_sp_df, aes(long, lat, group = group)) + geom_polygon() +
  geom_path(colour = "white") + coord_equal()

save(subregions_sp_df, file = "subregions_sp_df.RData")

rm(list = ls())
