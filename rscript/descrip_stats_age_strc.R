library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
source("functions.R")

survey <- readr::read_rds("./data/survey.rds") %>%
    select(age, gender) %>%
    mutate_age_group() %>%
    filter(gender == "男" | gender == "女") %>%
    group_by(age_group, gender) %>% 
    summarise(n())

male_n <- survey$`n()`[survey$gender == "男"] %>% sum()
female_n <- survey$`n()`[survey$gender == "女"] %>% sum()
max_n <- survey$`n()` %>% max()
max_n = ceiling(max_n/10)*10


pl_age_stru <- ggplot(survey, aes(x = age_group,
                     fill = gender,
                     y = ifelse(gender == "男",
                                `n()`, -`n()`)
                   )
       ) +
    geom_bar(stat = "identity",
             position = "identity",
             width = 0.7) +
    coord_flip() +
    scale_y_continuous(
        limits = c(-max_n, max_n),
        breaks = seq(-max_n, max_n, 10),
        labels= abs(seq(-max_n, max_n, 10))
    ) +
    scale_fill_manual(
        values = c("#E41A1C", "#377EB8"), #get_brewer_pal("Set1", 2)
        breaks = c("女","男"),
        labels = c("女", "男")
    ) +
    theme_bw() +
    theme(axis.text = element_text(size = 17),
          title = element_text(size = 19),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 25,
                               face="bold"),
          legend.text = element_text(size = 18),
          legend.justification = "right",
          legend.position = "bottom",
          legend.box = "vertical",
          plot.caption = element_text(size = 15, 
                                      hjust = 1)
          )+
    labs(x="年齡層", y="人數", fill = "",
         title="樣本組成結構",
         caption = paste("女性：",female_n,"人", "  男性：",male_n,"人", sep = ""))