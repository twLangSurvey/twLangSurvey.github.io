library(tmap)
library(tmaptools)
library(tidyverse)
#library(showtext)

source("shape.R")

#tmaptools::palette_explorer() # choose color


#tm_shape(sp) + tm_fill(col="COUNTYNAME", title="臺灣各縣市")


map <- tm_shape(sp) + 
    tm_fill("COUNTYNAME") +
    tm_layout(title = "臺灣鄉鎮市區",
              bg.color = "grey90", #grey85
              frame = FALSE,
              fontfamily = "Noto Sans CJK TC",
              legend.show=T,
              legend.bg.alpha=.5)
#ggplot(sp) _ geom_sf(aes(fill=colname))