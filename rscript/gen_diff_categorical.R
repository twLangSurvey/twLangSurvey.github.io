library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

survey <- readr::read_rds("./data/survey.rds")

###### Create new col's: main lang used ######
## between last gen (mom & dad) & cross gen (me & mom/dad) ##

Mand_fq<- c("幾乎全講華語", "多數講華語")
other_infq <- c("不使用華語以外的語言", "少數用此語言", "幾乎不用此語言")
no_exper <- c("沒有聽過他們對話", "沒有和他對話的經驗", "沒有和她對話的經驗")

survey <- survey %>%
    mutate(  # Find out the main lang between mom & dad
        dad_mom_main_lang = 
            case_when(dad_mom_Mand_fq %in% no_exper ~ "NA",
                      dad_mom_Mand_fq %in% Mand_fq ~ "華語",
                      dad_mom_Mand_fq == "約一半" & 
                          dad_mom_2nd_lang_fq == "約一半" ~ 
                          paste(dad_mom_2nd_lang, "、華語各半", sep=""),
                      ! dad_mom_Mand_fq %in% Mand_fq & 
                          ! dad_mom_2nd_lang_fq %in% other_infq ~ 
                          dad_mom_2nd_lang,
                      TRUE ~ "多語混用")
        ) %>%
        mutate( # Find out the main lang between me & dad
        me_dad_main_lang = 
            case_when(me_dad_Mand_fq %in% no_exper ~ "NA",
                      me_dad_Mand_fq %in% Mand_fq ~ "華語",
                      me_dad_Mand_fq == "約一半" & 
                          me_dad_2nd_lang_fq == "約一半" ~ 
                          paste(me_dad_2nd_lang, "、華語各半", sep=""),
                      ! me_dad_Mand_fq %in% Mand_fq & 
                          ! me_dad_2nd_lang_fq %in% other_infq ~ 
                          me_dad_2nd_lang,
                      TRUE ~ "多語混用")
    ) %>% 
        mutate( # Find out the main lang between me & mom
        me_mom_main_lang = 
            case_when(me_mom_Mand_fq %in% no_exper ~ "NA",
                      me_mom_Mand_fq %in% Mand_fq ~ "華語",
                      me_mom_Mand_fq == "約一半" & 
                          me_mom_2nd_lang_fq == "約一半" ~ 
                          paste(me_mom_2nd_lang, "、華語各半", sep=""),
                      ! me_mom_Mand_fq %in% Mand_fq & 
                          ! me_mom_2nd_lang_fq %in% other_infq ~ 
                          me_mom_2nd_lang,
                      TRUE ~ "多語混用")
    )


##### Percentage of (main_lang == 華語) #####
## Return 3 percentages: dad and mom, ## 
## me and mom/dad with age_range      ##
P_Mand_as_main <-
    function(df, age_range = c(16, 22)) {
        df_filtered <- df %>%
            select(-date) %>%
            filter(age >= age_range[1]) %>%
            filter(age <= age_range[2]) %>%
            select(dad_mom_main_lang,
                   me_dad_main_lang,
                   me_mom_main_lang)
        df_filtered %>%
            map(function(x)
                x == "華語") %>%
            map(mean) %>% as_data_frame() %>%
            mutate(age_group = paste(age_range[1],
                                     age_range[2],
                                     sep = "-"))
    }

##### Row bind df's created by 'P_Mand_as_main' #####
## Takes dataframe(survey) as input, 
## autogroup(auto set age_range in P_mand according to 
## age range in survey) and rbind all age_groups

rbind_df_PMand <- function(df) {
    min <- min(df$age)
    max <- max(df$age)
    
    if (round(min/10) > min/10 && 
        round(max/10) > max/10) {# 5-9 in both min, max
        min <- floor(min/10)*10 + 5
        max <- floor(max/10)*10 + 9}
    else if (round(min/10) <= min/10 &&  # 0-4 in min
              round(max/10) > max/10) {   # 5-9 in max
        min <- floor(min/10)*10
        max <- floor(max/10)*10 + 9        
    }
    else if (round(min/10) > min/10 &&   # 5-9 in min
              round(max/10) <= max/10) {  # 0-4 in max
        min <- floor(min/10)*10 + 5
        max <- floor(max/10)*10 + 4
    }
    else {         # 0-4 in both min, max
        min <- floor(min/10)*10
        max <- floor(max/10)*10 + 4}
    range <- max - min
    g_num <- floor(range/5)+1
    list_df <- vector("list", g_num) # list for storing df's
    for (i in 1:g_num){
        list_df[[i]] <- 
            P_Mand_as_main(df, c(min*i,min*i+4))
    }
    df2 <- bind_rows(list_df) # rbind df's from the list
    attr(df2, 'g_num') <- g_num
    df2
}

#### Plot age_group bar plot ####
colors <- tmaptools::get_brewer_pal("Dark2",  plot = F,
                                    n = 5)[c(2, 1, 4)]

pl_Mand_crossgen_bar <- rbind_df_PMand(survey) %>%
    gather(key = "subjects", value = "prob",-age_group) %>%
    ggplot(aes(x = age_group, y = prob)) +
    geom_bar(
        aes(fill = subjects),
        colour = "black",
        width = 0.6,
        position = "dodge",
        stat = "identity"
    ) +
    scale_fill_manual(
        values = colors,
        breaks = c("dad_mom_main_lang",
                   "me_dad_main_lang",
                   "me_mom_main_lang"),
        labels = c("父親、母親",
                   "父親、自己",
                   "母親、自己")
    ) +
    labs(
        x = "年齡層",
        y = "比例",
        fill = "對話組合",
        title = expression(bold("華語") * "作為主要溝通語言之比例")
    ) + theme_bw(base_size = 14) 
#tmaptools::palette_explorer()
# tmaptools::get_brewer_pal("Dark2", n = 6)

ggsave("../web_source/out_graph/Mand_usage_age_group.png",
       plot = pl_Mand_crossgen_bar, width=250)