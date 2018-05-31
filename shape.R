library(sf)
library(dplyr)
library(readr)

##### Extract zipcode from TOWN_zip.kml in order to combine to shp ####
# the extracted df is saved to ./spatial_data/zipcode_towncode.csv #

# zip <- st_read("./shape/TOWN_zip.kml")[,c("TOWNCODE","ZIPCODE", "COUNTYNAME","TOWNNAME")] %>% st_set_geometry(NULL) %>% arrange(ZIPCODE)
# write_csv(zip, "./spatial_data/zipcode_towncode.csv")
zip <- read_csv("spatial_data/zipcode_towncode.csv", 
    col_types = cols(ZIPCODE = col_character()))[,2:4]
sp <- st_read("./shape/TOWN_MOI_1070330.shp")

sp <- sp %>% 
    left_join(zip, by = c("COUNTYNAME","TOWNNAME")) %>%
    select(COUNTYNAME, TOWNNAME, ZIPCODE)
