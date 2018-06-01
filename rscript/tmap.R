library(tmap)
library(tmaptools)
library(tidyverse)

source("shape.R")

#tmaptools::palette_explorer() # choose color


#tm_shape(sp) + tm_fill(col="COUNTYNAME", title="臺灣各縣市")

map <- tm_shape(sp) + 
    tm_polygons(col="COUNTYNAME") +
    tm_layout(title = "臺灣鄉鎮市區",
              bg.color = "grey85",
              frame = FALSE,
              fontfamily = "PingFang TC")