library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
source("functions.R")

survey <- readr::read_rds("./data/survey.rds")

##### Local Functions #####
## ttest_filter: Add avg diff in lang ability ##
## and paired-ttest p-value to input df.      ##
## Require lang_group & age_group             ##
ttest_filter <- function(df, lang, mom_or_dad="mom") {
    age_gp <- df$age_group %>% unique()
    for (i in seq_along(age_gp)){
    	df2 <- filter(df, lang_group == lang &
    	               age_group == age_gp[i])
    	dad <- df2 %>%
    			select(paste(mom_or_dad,"_speak",sep="")) %>%
    	    unlist()
    	me <- df2 %>%
    			select(me_speak) %>% unlist()
    	t_test <- safely(t.test)
    	t_result <- t_test(me, dad, paired=T)
        if (!is.null(t_result$result)) {
            idx <- df$lang_group == lang & df$age_group == age_gp[i]
        	df[idx, 6] <- t_result$result$estimate
        	df[idx, 7] <- t_result$result$p.value
        } else print(paste("ttest failed at", lang, age_gp[i]))
    }
	return(df)
}
 
## comb_ttest: combine & summarise    ## 
## resulting dfs from 'ttest_filter'  ##
comb_ttest <- function(df, mom_or_dad) { 
    df <-ttest_filter(df, lang="Hak", mom_or_dad) %>%
        ttest_filter(lang = "Tw", mom_or_dad) %>%
        ttest_filter(lang = "Mand", mom_or_dad) %>%
        ttest_filter(lang = "Ind", mom_or_dad) %>%
        ttest_filter(lang = "Eng", mom_or_dad) %>%
        ttest_filter(lang = "SEA", mom_or_dad) %>%
        group_by(lang_group, age_group) %>%
        summarise(diff=mean(diff), p_value= mean(p_value))
}
###


lang_ch <- c('華語', '閩南語', '客家語', '原住民族語', '東南亞語言', '英語')
lang <- c('Mand', 'Tw', 'Hak', 'Ind', 'SEA', 'Eng')
lang_lev <- c('Mand'=3, 'Tw'=3, 'Hak'=3, 'Ind'=3, 'SEA'=3 ,'Eng'=0)

df_list_dad <- vector("list", 6)
df_list_mom <- vector("list", 6)

for (i in seq_along(lang)){
    df_list_dad[[i]] <- survey %>% 
        filter_ethnic1(select_by = "dad", 
                       lev=lang_lev[i],
                       lang = lang[i]) %>%
        mutate(lang_group = lang[i]) %>%
        select(age, lang_group, 
               paste("dad", lang[i], "speak", sep = "_"), 
               paste(lang[i], "speak", sep = "_")) %>%
        rename(dad_speak = paste("dad", lang[i], 
                                 "speak", sep = "_"),
               me_speak = paste(lang[i], "speak", sep = "_")
               )
    ## Repeat with mom
    df_list_mom[[i]] <- survey %>% 
        filter_ethnic1(select_by = "mom",
                       lev=lang_lev[i],
                       lang = lang[i]) %>%
        mutate(lang_group = lang[i]) %>%
        select(age, lang_group, 
               paste("mom", lang[i], "speak", sep = "_"), 
               paste(lang[i], "speak", sep = "_")) %>%
        rename(mom_speak = paste("mom", lang[i], 
                                 "speak", sep = "_"),
               me_speak = paste(lang[i], "speak", sep = "_")
               )
}

df_dad <- bind_rows(df_list_dad) %>%
    mutate_age_group() %>%
    mutate(diff = NA,
           p_value = NA)

df_mom <- bind_rows(df_list_mom) %>%
    mutate_age_group() %>%
    mutate(diff = NA,
           p_value = NA)

df_dad <- comb_ttest(df_dad, "dad") %>%
    mutate(sig = ifelse(p_value < 0.05,T, F)) %>%
    recode_lang(lang_col = "lang_group") %>%
    mutate(parent = "父親")
df_mom <- comb_ttest(df_mom, "mom") %>%
    mutate(sig = ifelse(p_value < 0.05,T, F)) %>%
    recode_lang(lang_col = "lang_group") %>%
    mutate(parent = "母親")

df_full <- rbind(df_dad, df_mom)



#### Plot dad #####
rev_age_group <- unique(df_full$age_group) %>% rev()

pl_full_diff <- df_full %>%
    ggplot(aes(x=age_group, y=diff, 
               color=lang_group)) +
    facet_grid(. ~ parent)+
    geom_segment(x=0, 
                 xend=20, 
                 y=0.0, yend=0.0, color="black")+
    geom_point(size=2.5) + 
    geom_line(aes(group = lang_group)) +
    scale_y_continuous(breaks = seq(-5,5,0.5))+
    scale_x_discrete(limits=rev_age_group)+
    labs(y="語言能力差異",
         x="年齡層", color="",
         title="跨世代語言能力差異",
         subtitle="子女 - 父母 (口說能力)",
         caption = expression("語言能力差異為"*bold("負值")*"代表子女語言口說能力"*bold("較父親或母親差")))

ggsave("../web_source/out_graph/cross_generation_lang_ability.png", plot = pl_full_diff, width=6, height = 3, dpi = 100)