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

#reclass Mastermap ugs with same ugs class in Baldock et. al 2019
unique(as.factor(BK$Habitat))
unique(ugs_grid$priFunc)
data <- ugs_grid
ugs_grid <- ugs_grid %>%
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
st_write(ugs_grid, "Data/ugs_reclass", driver = "ESRI Shapefile")

#calculate total per habitat to compare Baldock et al
area_ugs_city <- tapply(ugs_grid$ugs_area, ugs_grid$Habitat_reclas, FUN=sum)
st_crs(ugs_grid)
area_ugs_city/10000

