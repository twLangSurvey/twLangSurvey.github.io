library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(tmap)
##### Extract zipcode from TOWN_zip.kml in order to combine to shp ####
# the extracted df is saved to ./data/zipcode_towncode.csv #

# zip <- st_read("./shape/TOWN_zip.kml")[,c("TOWNCODE","ZIPCODE", "COUNTYNAME","TOWNNAME")] %>% st_set_geometry(NULL) %>% arrange(ZIPCODE)
# write_csv(zip, "./data/zipcode_towncode.csv")
zip <- read_csv("data/zipcode_towncode.csv", 
    col_types = cols(ZIPCODE = col_character()))[,2:4]
sp <- st_read("./shape/TOWN_MOI_1070330.shp")

sp <- sp %>% 
    left_join(zip, by = c("COUNTYNAME","TOWNNAME")) %>%
    select(COUNTYNAME, TOWNNAME, TOWNENG, ZIPCODE)

sp2 <- sp %>%
    mutate(lang1 = sample(0:5, 368,replace = T),
           lang2 = sample(0:5, 368,replace = T),
           age = sample(seq(15,85,5),368,replace = T),
           fill = 1) %>%
    gather(key="lang_type", value = "fluency",
           lang1, lang2)


ani_tmap <- tm_shape(sp2)+
    tm_fill()+
    tm_style("grey")+
    tm_shape(sp2)+
    tm_fill("fluency") +
    tm_facets(by = "lang_type", along="age", 
              free.coords = F, nrow = 2, ncol = 1)

tmap_animation(ani_tmap, filename = "../web_source/out_graph/taiwan_sp.gif", delay = 70, width = 300)
#read https://github.com/Robinlovelace/geocompr/blob/master/code/09-urban-animation.R



#png(filename = "taiwan.png",width = 512, height = 512, units = "px", pointsize = 1,bg = "transparent", type = "cairo", res=600)
#svg(file = "taiwan.svg", width = 12, height = 12, onefile = TRUE,bg = "transparent")
#library(tmap)
#map <- tm_shape(sp) + tm_fill(col = "grey")+tm_layout(bg.color = "transparent",frame=F,inner.margins=0)


#dev.off() 
