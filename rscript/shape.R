library(sf)
library(dplyr)
library(readr)

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

#animation::saveGIF(for (i in colnames(sp)[c(2,3)]) plot(sp[i]),
#        movie.name = "../web_source/out_graph/taiwan_sp.gif", img.name = "plott",
#        ani.dev = function(...){png(res=130*1.2,...)})





#png(filename = "taiwan.png",width = 512, height = 512, units = "px", pointsize = 1,bg = "transparent", type = "cairo", res=600)
#svg(file = "taiwan.svg", width = 12, height = 12, onefile = TRUE,bg = "transparent")
#library(tmap)
#map <- tm_shape(sp) + tm_fill(col = "grey")+tm_layout(bg.color = "transparent",frame=F,inner.margins=0)

#save_tmap(map, filename = "taiwan.png",width = 900)

#dev.off() 
