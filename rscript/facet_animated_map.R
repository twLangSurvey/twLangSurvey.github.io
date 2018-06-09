library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(tmap)
source("functions.R")

zip <- read_csv("data/zipcode_towncode.csv", 
    col_types = cols(ZIPCODE = col_character()))[,2:4]
survey <- readr::read_rds("./data/survey.rds")
sp <- st_read("./shape/TOWN_MOI_1070330.shp") %>% 
    left_join(zip, by = c("COUNTYNAME","TOWNNAME")) %>%
    select(COUNTYNAME, TOWNNAME, TOWNENG, ZIPCODE)

lang <- c('Mand', 'Tw', 'Hak', 'Ind', 'SEA', 'Eng')
lang_ch <- c('華語', '閩南語', '客家語', '原住民族語', '東南亞語言', '英語')
lang_idx <- paste(lang, "_speak", sep="")

survey <- survey %>%
    select(age, curr_resid, 
           lang_idx) %>%
    left_join(zip, by=c("curr_resid"="ZIPCODE")) %>%
    mutate_age_group()
survey[lang_idx] <- survey[lang_idx] %>%
    map(function(x) x >= 3)

survey_sp <- survey %>%
    gather(key="lang", value = "know", lang_idx) %>%
    group_by(lang, COUNTYNAME, TOWNNAME, age_group) %>%
    summarise(mean(know)) %>%
    left_join(sp, by="COUNTYNAME")



c <- tmaptools::get_brewer_pal("Oranges", n = 6)
library(ggplot2)
survey_sp$lang <- factor(survey_sp$lang, 
                         levels=lang_idx, labels=lang_ch)
g1 = ggplot() + 
    geom_sf(data = sp, fill = "grey70", colour=NA)+
    geom_sf(data = survey_sp, aes(fill = `mean(know)`,
                           frame = age_group),
            colour=NA) +
    scale_x_continuous(limits = c(118, 123),
                       breaks = 0)+
    scale_y_continuous(limits = c(21.8, 25.8),
                       breaks = 0) +
    scale_fill_gradient(low=c[1], high=c[6])+
    facet_grid(lang ~ .)

gganimate::gganimate(g1, ani.width=500, 
          ani.height=2000, interval = 1,
          filename="../web_source/out_graph/animated_facet_map.gif")