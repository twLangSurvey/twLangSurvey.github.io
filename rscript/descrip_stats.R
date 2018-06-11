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

sp_border <- sp %>% group_by(COUNTYNAME) %>% 
    summarise(n())
sp2 <- sp %>% 
    left_join(survey_distr, by="ZIPCODE") %>%
    filter(!is.na(type))

breaks <- c(0, 5, 10, 30)
palette <- tmaptools::get_brewer_pal("YlOrRd", n = 6)

pl_samp_distr <- ggplot()+
    geom_sf(data = sp_border, fill = palette[1],
            colour="grey30", lwd = 0.1)+
    geom_sf(data= sp2, aes(fill = `樣本數`,
                           frame = type),
            colour=NA)+
    scale_fill_gradientn(colours = palette[-(1:3)], 
                         limits=c(0, 30))

gganimate::gganimate(pl_samp_distr, ani.width=972,
          ani.height=1000, interval = 1.7,
          filename="../web_source/out_graph/sample_spatial_distr.gif")

