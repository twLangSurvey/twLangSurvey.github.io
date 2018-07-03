source("facet_static_map.R", encoding = "utf-8")

survey_sp <- survey %>%
    gather(key="lang", value = "know", lang_idx) %>%
    group_by(lang, COUNTYNAME, age_group) %>% #縣市為單位, 若要鄉鎮市區, 加上 'TOWNNAME'
    summarise(mean(know)) %>%
    ungroup()
survey_sp <- sp %>% 
    left_join(survey_sp, by=c("COUNTYNAME")) %>% #縣市為單位, 若要鄉鎮市區, 加上 'TOWNNAME'
    filter(!is.na(age_group)) %>%
    filter(lang != "SEA_speak" & lang != "Ind_speak")
survey_sp$lang <- factor(survey_sp$lang, 
                         levels=lang_idx, labels=lang_ch)
survey_sp$age_group <- factor(survey_sp$age_group,
                              levels = paste(seq(10,90,5),"-",seq(14,94,5),sep = "") %>% rev())



g1 = ggplot() + 
    geom_sf(data = sp, fill = "grey70", colour=NA)+
    geom_sf(data = survey_sp, aes(fill = `mean(know)`,
                           frame = age_group),
            colour=NA) +
    scale_x_continuous(limits = c(118.4, 123),#c(118.4, 123),
                       breaks = 0)+
    scale_y_continuous(limits = c(21.65, 26.22),#c(21.8, 25.4),
                       breaks = 0) +
    scale_fill_gradient(low=c[2], high=c[8])+
    facet_wrap( ~ lang ,ncol=2, strip.position="top")+
    theme(strip.text.x = element_text(size = 25, face = 'bold'),
          legend.title = element_text(size = 22),
          plot.caption = element_text(size = 22, hjust = 0),
          axis.title.x = element_text(size =22, hjust = 0))+
    theme_define(plot_title = 35, legend_size=4.0,
                 legend_text = 20,
                 legend_posi = 'right',
                 legend_anchor = 'top')+
    labs(fill="懂該語言之比例",
         title = "年齡層：",
         caption = expression(bold("原住民族語")*"以及"*bold("東南亞語言")*"因使用比例低，難以地圖呈現而省略"),
         x="灰色部份代表尚無資料"
         )

gganimate::gganimate(g1, ani.width=972,
          ani.height=1000, interval = 1.4,
          filename="../web_source/out_graph/animated_facet_map.gif")