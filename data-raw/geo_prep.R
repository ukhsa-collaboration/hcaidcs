library("rgdal") # librarys sp, will use proj.4 if installed
library("maptools")
library("ggplot2")
library("plyr")
library("rgeos")
library("dplyr")

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

centroids_df <- data.frame(gCentroid(subregions, byid=TRUE, id = subregions@data$id))
centroids_df <- data.frame(rownames(centroids_df),centroids_df)
rownames(centroids_df) <- c(1:nrow(centroids_df))
names(centroids_df) <- c("id" ,"centroid_long", "centroid_lat")
# offset east and central mids
centroids_df$centroid_long[centroids_df$id == 6] <- centroids_df$centroid_long[centroids_df$id == 6] - (centroids_df$centroid_long[centroids_df$id == 6] * 0.05)
# Q78 (E39000030) could be more central, i.e. shifted left
centroids_df$centroid_long[centroids_df$id == 5] <- centroids_df$centroid_long[centroids_df$id == 5] - (centroids_df$centroid_long[centroids_df$id == 5] * 0.05)
# And Q70
centroids_df$centroid_long[centroids_df$id == 1] <- centroids_df$centroid_long[centroids_df$id == 1] - (centroids_df$centroid_long[centroids_df$id == 1] * 0.01)
centroids_df$centroid_lat[centroids_df$id == 1] <- centroids_df$centroid_lat[centroids_df$id == 1] - (centroids_df$centroid_lat[centroids_df$id == 1] * 0.05)
subregions_sp_df <- left_join(subregions_sp_df, centroids_df)

ggplot(subregions_sp_df, aes(long, lat, group = group, label = GSS_CD)) +
  geom_polygon() +
  geom_path(colour = "white") + coord_equal() +
  geom_text(aes(x = centroid_long, y = centroid_lat), colour = "white")

save(subregions_sp_df, file = "H:\\hcaidcs\\data\\subregions_sp_df.RData")

rm(list = ls())
