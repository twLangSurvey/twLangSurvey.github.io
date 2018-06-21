library(dplyr)

survey <- readr::read_rds("./data/survey.rds")

lang <- c('Mand', 'Tw', 'Hak', 'Ind', 'SEA', 'Eng')
lang_ch <- c('華語', '閩南語', '客家語', '原住民族語', '東南亞語言', '英語')
lang_idx <- paste(lang, "_speak", sep="")

idx <- c("curr_resid", "home_town", "gender", "age", "kid_num", "edu_level", "work", "income", "work_hr", "tribe")

survey <- survey[,c(idx,lang_idx)]

lang_clust <- hclust(dist(survey[,lang_idx]))
survey <- survey %>%
    mutate(cluster = as.factor(cutree(lang_clust, k=4)))

salary <- paste0(seq(10,100,5), ",000")
salary2 <- paste(salary[-19], salary[-1], sep = " - ")
salary2 <- c("無", "1萬以下", salary2, "10萬以上")
survey$income <- factor(survey$income, levels = salary2)


library(ggplot2)
ggplot(survey, aes(x = Tw_speak, y = Hak_speak))+
    geom_point(aes(color = cluster), alpha=0.6)