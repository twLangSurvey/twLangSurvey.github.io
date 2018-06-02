library(tmap)
library(tmaptools)
library(tidyverse)
#library(showtext)

source("shape.R")

#tmaptools::palette_explorer() # choose color


#tm_shape(sp) + tm_fill(col="COUNTYNAME", title="臺灣各縣市")

#showtext.auto(enable = TRUE)
#font.add("NotoSansCJK", "NotoSansCJKtc.otf")

map <- tm_shape(sp) + 
    tm_polygons(col="COUNTYNAME") +
    tm_layout(title = "臺灣鄉鎮市區",
              bg.color = "grey85",
              frame = FALSE,
              fontfamily = "Noto Sans CJK TC")
