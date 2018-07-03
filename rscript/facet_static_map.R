library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(tmap)
source("functions.R")

zip <- read_csv("./data/zipcode_towncode.csv", 
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
    map(function(x) x >= 3) #Transform 6 point likert to binary

###########################################

survey_sp2 <- survey %>%
    gather(key="lang", value = "know", lang_idx) %>%
    group_by(lang, COUNTYNAME) %>% #縣市為單位, 若要鄉鎮市區, 加上 'TOWNNAME'
    summarise(mean(know)) %>% ungroup()
survey_sp2 <- sp %>% 
    left_join(survey_sp2, by=c("COUNTYNAME")) %>% #縣市為單位, 若要鄉鎮市區, 加上 'TOWNNAME'
    filter(lang != "SEA_speak" & lang != "Ind_speak")
survey_sp2$lang <- factor(survey_sp2$lang, 
                         levels=lang_idx, labels=lang_ch)

n_row <- nrow(survey)
c <- tmaptools::get_brewer_pal("Oranges", n = 8)
library(ggplot2)


pl_all <- ggplot()+
    geom_sf(data = sp, fill = "grey70", colour=NA)+
    geom_sf(data = survey_sp2, 
            aes(fill = `mean(know)`), colour=NA) +
    scale_x_continuous(limits = c(118.4, 123),#c(118.4, 123),
                       breaks = 0)+
    scale_y_continuous(limits = c(21.65, 26.22),#c(21.8, 25.4),
                       breaks = 0) +
    scale_fill_gradient(low=c[2], high=c[8])+
    facet_wrap( ~ lang ,ncol=2, strip.position="top")+
    theme_define(legend_posi = 'right',
                 legend_anchor = 'top')+
    theme(plot.caption = element_text(hjust = 0),
          plot.margin=unit(c(0,0,0,0.1),"cm"),
          legend.key = element_rect(size = 1),
          legend.key.size = unit(0.35, "cm"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          strip.text.x = element_text(size = 13, face = 'bold'),
          axis.title.x = element_text(size =10, hjust = 0))+
    labs(fill="懂該語言之比例",
         caption = expression(bold("原住民族語")*"以及"*bold("東南亞語言")*"因使用比例低，難以地圖呈現而省略"),
         x="灰色部份代表尚無資料"
         )

ggsave("../web_source/out_graph/facet_static_map.png", pl_all, dpi=500, width=7, height=7.1)