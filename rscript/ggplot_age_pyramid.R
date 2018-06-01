library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(gganimate)
theme_set(theme_bw())


df <- data.frame(
    gender = sample(c("男","女"), 100, replace = T),
    age = sample(10:90, 1000, replace = T),
    mand = sample(0:5, 1000, replace = T),
    tw = sample(0:5, 1000, replace = T),
    hk = sample(0:5, 1000, replace = T),
    abori = sample(0:5, 1000, replace = T),
    sa = sample(0:5, 1000, replace = T),
    eng = sample(0:5, 1000, replace = T)
)
df$group <- cut(df$age, breaks = seq(0.5,90.5,by=10), right = TRUE)

pl <- df %>% group_by(gender, group) %>%
    summarise(mean(mand), mean(tw), 
              mean(hk), mean(abori), 
              mean(sa), mean(eng)) %>%
    set_names(c("gender", "age_group",
                "Mandarin", "Taiwanese",
                "Hakka", "Aborigine", 
                "South_east", "English")) %>%
    gather("Mandarin", "Taiwanese","Hakka",
           "Aborigine","South_east", "English", 
           key = "lang", 
           value = "avg_fluency") %>%
ggplot(aes(x = age_group, fill = gender,
           y = ifelse(gender=="男",
                      avg_fluency, -avg_fluency),
           frame = lang)
       ) + 
    geom_bar(stat = "identity", position = "identity", width=0.7)+
    scale_y_continuous(limits = c(-5,5),
                       breaks = seq(-5,5,1), 
                       labels=abs(seq(-5,5,1))
                     ) + 
  coord_flip() + 
  #scale_fill_brewer(palette = "Set1") + 
  theme_minimal()+
    theme(axis.text.x = element_text(size=15))

gganimate(pl, ani.width=800, ani.height=350, filename="../web_source/out_graph/age_pyramid.gif")
