library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(gganimate)

lang_fluen <- readr::read_rds("./data/survey.rds")
na_col <- colnames(lang_fluen)

idx <- vector("numeric", 4)
idx[1] <- which(na_col=="gender")
idx[2] <- which(na_col=="age")
idx[3] <- which(na_col=="Mand_listen")
idx[4] <- which(na_col=="Eng_speak")
idx[5] <- which(na_col=="dad_Mand_speak")
idx[6] <- which(na_col=="mom_SEA_speak")

lang_fluen <- lang_fluen[,c(idx[1],idx[2], 
                            idx[3]:idx[4],
                            idx[5]:idx[6])] %>% 
    filter(gender == "男" | gender == "女")

###### Function: Select lang ethnicity ######
## Define lang ethnicity as subjects with
## 'mom who knows lang_A or dad who knows lang_A'
## Filter specific lang ethnicity and return
## subjects' age, gender, lang_speak, dad/mom_lang_speak, 
## and age_group (by base::cut)
#lang type: Mand Tw Hak Ind SEA Eng
filter_ethnic <- function(df, lang) {
    sp_lang <- vector("character", 3)
    sp_lang[1] <- paste("dad_", lang, "_speak", sep = "")
    sp_lang[2] <- paste("mom_", lang, "_speak", sep = "")
    sp_lang[3] <- paste(lang, "_speak", sep = "")
    df %>% filter(sp_lang[1] >= 3 | sp_lang[2] >= 3) %>%
        select(age, gender, sp_lang)
    df$age_group <- cut(df$age, right = F,
                    breaks = seq(10,95,by=5)
                )
    df
}

##### Animated bar plot #####

### Combine different ethnic group ###
lang <- c('Mand', 'Tw', 'Hak', 'Ind', 'SEA')
lang_ch <- c('華語', '閩南語', '客家語', '原住民族語', '東南亞語言')
ethn_list_df <- vector("list", length(lang))
for (i in seq_along(lang)){
    ethn_list_df[[i]] <- filter_ethnic(lang_fluen, 
                                       lang = lang[i]) %>%
        mutate(ethn_group = lang_ch[i]) %>%
        select(age, gender, age_group, ethn_group,
               paste(lang[i],"_speak",sep = "")) %>%
        rename(lang_fluency = paste(lang[i],"_speak",sep = ""))
}

pl <- bind_rows(ethn_list_df) %>%
    group_by(gender, ethn_group, age_group) %>%
    summarise(mean(lang_fluency)) %>%
    rename(avg_fluency = `mean(lang_fluency)`) %>%
    mutate(age_group = as.character(age_group))

age_group_idx <- paste("[", seq(10,90,5), ",", seq(15,95,5), ")", sep="")
age_group_idx2 <- paste(seq(10,90,5), "-", seq(14,94,5), sep="")
for (i in seq_along(age_group_idx)){
    pl$age_group[pl$age_group==age_group_idx[i]] <- age_group_idx2[i]
}



pl_ani_bar <- ggplot(pl, aes(x = age_group,
                     fill = gender,
                     y = ifelse(gender == "男",
                                avg_fluency, -avg_fluency),
                     frame = ethn_group)
             ) +
    geom_bar(stat = "identity",
             position = "identity",
             width = 0.7) +
    scale_y_continuous(
        limits = c(-5, 5),
        breaks = seq(-5, 5, 1),
        labels = abs(seq(-5, 5, 1))
    ) +
    coord_flip() +
    scale_fill_manual(
        values = c("#E41A1C", "#377EB8"), #get_brewer_pal("Set1", 2)
        breaks = c("女","男"),
        labels = c("女", "男")
    ) +
    labs(x="年齡層", y="流利程度", fill = "性別") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          title = element_text(size = 14),
          plot.title = element_text(size = 17,
                               face="bold"),
          legend.text = element_text(size = 13.5),
          legend.justification = c("right", "top"))

gganimate(pl_ani_bar, ani.width=800, 
          ani.height=350, interval = 1,
          filename="../web_source/out_graph/age_pyramid.gif")