library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(tmap)
library(purrr)
library(ggplot2)
source("functions.R")

zip <- read_csv("./data/zipcode_towncode.csv", 
    col_types = cols(ZIPCODE = col_character()))[,2:4]
sp <- st_read("./shape/TOWN_MOI_1070330.shp") %>% 
    left_join(zip, by = c("COUNTYNAME","TOWNNAME")) %>%
    select(COUNTYNAME, TOWNNAME, TOWNENG, ZIPCODE)

survey <- readr::read_rds("./data/survey.rds") %>%
    select(age, gender, curr_resid, home_town) %>%
    mutate_age_group()

##### Spatial Distribution of Samples #####
survey_gb_curr_resid <- survey %>% 
    group_by(curr_resid) %>%
    summarise(n()) %>%
    mutate(type="現居地") %>%
    set_names(c("ZIPCODE", "樣本數", "type"))
survey_gb_hometown <- survey %>% 
    group_by(home_town) %>%
    summarise(n()) %>%
    mutate(type="家鄉") %>%
    set_names(c("ZIPCODE", "樣本數", "type"))
survey_distr <- rbind(survey_gb_curr_resid, 
                      survey_gb_hometown)

#sp_border <- sp %>% group_by(COUNTYNAME) %>% summarise(n())
#st_write(sp_border, dsn = "./data/COUNTY_BORDER.gpkg")

sp_border <- st_read("./data/COUNTY_BORDER.gpkg")


sp2 <- sp %>% 
    left_join(survey_distr, by="ZIPCODE") %>%
    filter(!is.na(type))

palette <- tmaptools::get_brewer_pal("YlOrRd", n = 4)

pl_samp_distr <- ggplot()+
    geom_sf(data = sp_border, fill = "grey70",
            colour="grey30", lwd = 0.1)+
    geom_sf(data= sp2, aes(fill = `樣本數`,
                           frame = type),
            colour=NA)+
    scale_x_continuous(limits = c(118.4, 123),#c(118.4, 123),
                       breaks = 0)+
    scale_y_continuous(limits = c(21.65, 26.22),#c(21.8, 25.4),
                       breaks = 0) +
    scale_fill_gradientn(colours = palette, 
                         limits=c(0, 30))+
    theme(strip.text.x = element_text(size = 22),
          legend.title = element_text(size = 25),
          plot.caption = element_text(size = 25, 
                                      hjust = 0))+
    theme_define(plot_title = 35, legend_size=2.0,
                 legend_text = 22,
                 legend_posi = 'right',
                 legend_anchor = 'top')+
    labs(fill="人數",
         title = "問卷填寫者",
         caption = "問卷填寫者地理分佈(以鄉鎮市區為單位)")
    

gganimate::gganimate(pl_samp_distr, ani.width=750,
          ani.height=800, interval = 1.7,
          filename="../web_source/out_graph/sample_spatial_distr.gif")

