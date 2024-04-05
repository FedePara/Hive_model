##############
### Scenario 2 
##############

# required libraries
library(sf)
library (ggplot2)
library(dplyr)

##############
### Import data
##############
# Mastermap topography layer available at 
# Mastermap green space layer available at 
# local Nature reserve layer available at https://environment.data.gov.uk/explore/9364d6af-927e-4349-b4c3-dbaf7acb842d?download=true 
# SSSI layer available at
# delete building and railways 

#import ugs data for newcastle urban area 
## Read the polygon files
city_border<- st_read("Data/Newcastle borders.shp") 
ugs <- st_read("Data/ugs_dissolved.shp") 

#built nectar dataset
nectar_data <- data.frame(Habitat= c("Allotment", "Cemetery", "Garden", "NR", "OGS", "Park", "Verge"),
                          Nectar= c(7392, 1248, 8988, 1633, 960, 2235, 1473),
                          Nectar_min= c(3849, 722, 6878, 631, 315, 784, 729),
                          Nectar_max= c(11997, 2845, 17785, 3944, 2735, 4147, 3498) )
write.csv(nectar_data,file= "Data/nectar_data.csv",row.names=F)

##############
### grid 1km 
##############

#built grid
bbox <- st_bbox (ugs)
# grid 1km
grid_1km <- st_sf(geometry = st_make_grid (bbox, cellsize = c(1000, 1000),n =, what = "polygons"))
st_crs (grid) # check crs
grid_1km$id <- 1:144 #add column id
#intersect grid ugs dissolved
ugs_grid_1km <- st_intersection(ugs, grid_1km)
#calculate the area
ugs_grid_1km$area_ugs <- as.vector (st_area(ugs_grid_1km))

#reclass Mastermap ugs with same ugs class in Baldock et. al 2019
unique(as.factor(BK$Habitat))
unique(ugs_grid$priFunc)
data <- ugs_grid_1km
data <- st_drop_geometry(ugs_grid_1km)
data <- data %>%
  mutate(
    Habitat_reclas = case_when(
      priFunc == "Natural" ~ "NR",
      priFunc == "Amenity - Residential Or Business" ~ "OGS",
      priFunc == "Amenity - Transport" ~ "Verge",
      priFunc == "Public Park Or Garden" ~ "Park",
      priFunc == "Playing Field" ~ "OGS",
      priFunc == "School Grounds" ~ "OGS",
      priFunc == "Private Garden" ~ "Garden",
      priFunc == "Religious Grounds" ~ "OGS",
      priFunc == "Other Sports Facility" ~ "OGS",
      priFunc == "Cemetery"~ "Cemetery",
      priFunc == "Institutional Grounds" ~ "OGS",
      priFunc == "Play Space" ~ "OGS",
      priFunc == "Land Use Changing" ~ "OGS",
      priFunc == "Allotments Or Community Growing Spaces" ~ "Allotment",
      priFunc == "Bowling Green" ~ "OGS",
      priFunc == "Tennis Court" ~ "OGS", 
      priFunc == "Golf Course" ~ "OGS",
      TRUE ~ as.character(priFunc)))
write.csv(data, "Data/ugs_reclass.csv", row.names = F)


#merge intersected grid with nectar value
ugs_nectar_1km <- merge(data, nectar_data, by.x = "Habitat_reclas", by.y = "Habitat", all.x = FALSE, all.y = FALSE)
ugs_nectar_1km$nectar_ugs <- ugs_nectar_1km$area_ugs * ugs_nectar_1km$Nectar
ugs_nectar_1km$nectar_ugs_min <- ugs_nectar_1km$area_ugs * ugs_nectar_1km$Nectar_min
ugs_nectar_1km$nectar_ugs_max <- ugs_nectar_1km$area_ugs * ugs_nectar_1km$Nectar_max

st_write(ugs_nectar_1km,"Data/ugs_data",driver = "ESRI Shapefile")


##############
### scenario 2.1 
##############
# Scenario 2.1 London Beekeeper Association 135-230 kg year (0.369863-0.630137 kg/day)
scenario2_1km <- ugs_nectar_1km
cur1 <- c(0.369863,0.630137)
for (cur in cur1) {
  scenario2_1km[paste0("result_", cur)] <- scenario2_1km$nectar_ugs / (cur*10^9)
  scenario2_1km[paste0("min_result_", cur)] <- scenario2_1km$nectar_ugs_min / (cur*10^9)
  scenario2_1km[paste0("max_result_", cur)] <- scenario2_1km$nectar_ugs_max / (cur*10^9)
}

write.csv (scenario2_1km, "Output/scenario2_1km.csv", row.names= F)

#model 
hive_scenario2 <- data.frame (id= sort(unique(scenario2_1km$id)),
                              n_hive_0.37 = tapply (scenario2_1km$result_0.369863,scenario2_1km$id, FUN=sum),
                              n_hive_0.37_LI = tapply (scenario2_1km$min_result_0.369863,scenario2_1km$id, FUN=sum),
                              n_hive_0.37_HI = tapply (scenario2_1km$max_result_0.369863,scenario2_1km$id, FUN=sum), 
                              n_hive_0.63 = tapply (scenario2_1km$result_0.630137,scenario2_1km$id, FUN=sum),
                              n_hive_0.63_LI = tapply (scenario2_1km$min_result_0.630137,scenario2_1km$id, FUN=sum),
                              n_hive_0.63_HI = tapply (scenario2_1km$max_result_0.630137,scenario2_1km$id, FUN=sum))
#merge with grid
grid_scenario <- merge(grid_1km, hive_scenario2, by.x = "id", by.y = "id", all.x = TRUE)
st_write(grid_scenario, "Output/Scenario 2", driver = "ESRI Shapefile")


##############
### grid 3km 
##############
# built grid 3km 
grid_3km <- st_sf(geometry = st_make_grid (bbox, cellsize = c(3000, 3000),n =, what = "polygons"))
st_crs (grid) # check crs
grid_3km$id <- 1:18 #add column id

ugs_grid_3km <- st_intersection(ugs, grid_3km)
ugs_grid_3km$area_ugs <- as.vector (st_area(ugs_grid_3km))

#reclass Mastermap data according to Baldock et. al 2029 
data1 <- st_drop_geometry(ugs_grid_3km)
data1 <- data1 %>%
  mutate(
    Habitat_reclas = case_when(
      priFunc == "Natural" ~ "NR",
      priFunc == "Amenity - Residential Or Business" ~ "OGS",
      priFunc == "Amenity - Transport" ~ "Verge",
      priFunc == "Public Park Or Garden" ~ "Park",
      priFunc == "Playing Field" ~ "OGS",
      priFunc == "School Grounds" ~ "OGS",
      priFunc == "Private Garden" ~ "Garden",
      priFunc == "Religious Grounds" ~ "OGS",
      priFunc == "Other Sports Facility" ~ "OGS",
      priFunc == "Cemetery"~ "Cemetery",
      priFunc == "Institutional Grounds" ~ "OGS",
      priFunc == "Play Space" ~ "OGS",
      priFunc == "Land Use Changing" ~ "OGS",
      priFunc == "Allotments Or Community Growing Spaces" ~ "Allotment",
      priFunc == "Bowling Green" ~ "OGS",
      priFunc == "Tennis Court" ~ "OGS", 
      priFunc == "Golf Course" ~ "OGS",
      TRUE ~ as.character(priFunc)))
write.csv(data1, "Data/ugs_reclass.csv", row.names = F)

#merge intersected grid with nectar value
ugs_nectar_3km <- merge(data1, nectar_data, by.x = "Habitat_reclas", by.y = "Habitat", all.x = FALSE, all.y = FALSE)
ugs_nectar_3km$nectar_ugs <- ugs_nectar_3km$area_ugs * ugs_nectar$Nectar
ugs_nectar_3km$nectar_ugs_min <- ugs_nectar_3km$area_ugs * ugs_nectar$Nectar_min
ugs_nectar_3km$nectar_ugs_max <- ugs_nectar_3km$area_ugs * ugs_nectar$Nectar_max

scenario2_3km <- ugs_nectar_3km
cur1 <- c(0.369863,0.630137)
for (cur in cur1) {
  scenario2_3km[paste0("result_", cur)] <- scenario2_3km$nectar_ugs / (cur*10^9)
  scenario2_3km[paste0("min_result_", cur)] <- scenario2_3km$nectar_ugs_min / (cur*10^9)
  scenario2_3km[paste0("max_result_", cur)] <- scenario2_3km$nectar_ugs_max / (cur*10^9)
}



##############
# Scenario 2.2 
#Rodney et al. 2020 329-346 kg year (0.9013699-0.9479452 kg/day)
##############





