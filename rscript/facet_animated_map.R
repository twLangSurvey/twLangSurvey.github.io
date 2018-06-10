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
    ungroup()
survey_sp <- sp %>% 
    left_join(survey_sp, by=c("COUNTYNAME")) %>% 
    filter(!is.na(age_group)) %>%
    filter(lang != "SEA_speak" & lang != "Ind_speak")
survey_sp$lang <- factor(survey_sp$lang, 
                         levels=lang_idx, labels=lang_ch)
survey_sp$age_group <- factor(survey_sp$age_group,
                              levels = paste(seq(10,90,5),"-",seq(14,94,5),sep = "") %>% rev())
# ani_tmap <- tm_shape(sp)+
#      tm_fill()+
#      tm_shape(survey_sp)+
#      tm_fill("mean(know)") +
#      tm_facets(by = "lang", along="age_group",
#                free.coords = F, nrow = 3, ncol=2)
# 
# tmap_animation(ani_tmap, filename = "../web_source/out_graph/taiwan_sp.gif", delay = 70, width = 1000)

c <- tmaptools::get_brewer_pal("Oranges", n = 6)
library(ggplot2)

g1 = ggplot() + 
    geom_sf(data = sp, fill = "grey70", colour=NA)+
    geom_sf(data = survey_sp, aes(fill = `mean(know)`,
                           frame = age_group),
            colour=NA) +
    scale_x_continuous(limits = c(118.4, 123),#c(118.4, 123),
                       breaks = 0)+
    scale_y_continuous(limits = c(21.65, 26.22),#c(21.8, 25.4),
                       breaks = 0) +
    scale_fill_gradient(low=c[1], high=c[6])+
    facet_wrap( ~ lang ,ncol=2, strip.position="top")+
    theme(strip.text.x = element_text(size = 25, face = 'bold'),
          legend.title = element_text(size = 22),
          plot.caption = element_text(size = 22))+
    theme_define(plot_title = 35, legend_size=4.0,
                 legend_text = 20,
                 legend_posi = 'left',
                 legend_anchor = 'top')+
    labs(fill="懂該語言之比例",
         title = "年齡層：",
         caption = expression(bold("原住民族語")*"以及"*bold("東南亞語言")*"因使用比例低，難以地圖呈現而省略")
    )

gganimate::gganimate(g1, ani.width=972,
          ani.height=1000, interval = 1.7,
          filename="../web_source/out_graph/animated_facet_map.gif")