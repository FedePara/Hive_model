##############
### Scenario 1 
##############

# required libraries
library(sf)
library (ggplot2)
library(dplyr)

##############
### Import data
##############

## Read the polygon files
city_border<- st_read("Data/Newcastle borders.shp") 
ugs <- st_read("Data/Green_space dissolved.shp") 
# MasterMap topographic polygon dissolved by attributeto avoid overlapp 

## Build 1x1km grid with city extension NB: entire city territory has been considered (urban+agricultural)
# Get the bounding box of the shp
bbox <- st_bbox (city_border)
#grid of 1x1 km
grid <- st_sf(geometry = st_make_grid (bbox, cellsize = c(1000, 1000),n =, what = "polygons"))
st_crs (grid)
grid$id <- 1:224 #add column id

#Plot grid
plot(grid)
ggplot() +
  geom_sf(data = city_border) +
  geom_sf(data = grid, color = "blue", fill = NA) +
  theme_minimal()

# be sure both shp are in the same crs
st_crs(grid) == st_crs (ugs)
grid <- st_transform(grid, st_crs(ugs))

##############
### built database
##############

# clip grid with city border as mask
grid_clip <- st_intersection(grid, city_border) [,1] #select ID column only

#plot
ggplot() +
  geom_sf(data = city_border) +
  geom_sf(data = grid_clip, color = "blue", fill = NA) +
  theme_minimal()

# intersect ugs with the grid
ugs_grid <- st_intersection(ugs, grid_clip)

# Calculate clip_grid area (= area cell) and ugs area
grid_clip$area_cels <- as.vector (st_area(grid_clip))
ugs_grid$ugs_area <-as.vector(st_area(ugs_grid))

#plot
ggplot() +
  geom_sf(data = city_border) +
  geom_sf(data = grid_city, color = "blue", fill = NA) +
  theme_minimal()

#save ugs in the study area as shp and extract attribute table and save in csv
st_write (ugs_grid, "Data/ugs_data.shp")
write.csv(st_drop_geometry(ugs_grid),file= "Data/ugs_data.csv",row.names=F) 


##############
### calculate ugs coverage
##############

# import data
ugs_data <- ugs_grid
#ugs_data <- read.csv("Data/ugs_data.csv", sep=",")

#sum ugs area per cell
unique(ugs_data$secForm)
length(tot_ugs_cell)
length(unique(ugs_data$id))
length (ugs_data$ugs_area)

area_ugs_cell <- tapply(ugs_data$ugs_area, ugs_data$id, FUN=sum)

#area cell
length(grid_city$area_cels)
# Filter the data frame based on the "id" values
grid_study <- grid_clip[grid_clip$id %in% unique(ugs_data$id), ]

# Calculate the percentage of coverage
ugs_coverage <- (area_ugs_cell / grid_study$area_cels) * 100

#add ugs coverage to grid 
grid_study$ugs_coverage <- as.vector(ugs_coverage)
max(grid_study$ugs)
##############
### model
##############

hive_scenario <- grid_study

# different cc scenario 
# formula -> 100: cc= tot_ugs:hive1
hive_scenario$hive1 <- (hive_scenario$ugs_coverage*1/100) # cc=1
hive_scenario$hive2 <- (hive_scenario$ugs_coverage*4 /100) # cc=4
hive_scenario$hive3 <- (hive_scenario$ugs_coverage*7.5 /100) # cc=7.5
hive_scenario$hive4 <- (hive_scenario$ugs_coverage*10 /100) # cc=10

st_write(hive_scenario, "Output/Scenario 1", driver = "ESRI Shapefile")

Scenario1_data <- st_drop_geometry(hive_scenario)
write.csv(Scenario1_data,file= "Output/Scenario1_data.csv",row.names=F)



getwd()

