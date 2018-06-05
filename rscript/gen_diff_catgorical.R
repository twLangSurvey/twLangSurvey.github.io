library(dplyr)
library(tidyr)
library(purrr)
survey <- readr::read_rds("./data/survey.rds")

###### Create new col's: main lang used ######
## between last gen (mom & dad) & cross gen (me & mom/dad) ##

Mand_fq<- c("幾乎全講華語", "多數講華語")
other_infq <- c("不使用華語以外的語言", "少數用此語言", "幾乎不用此語言")

survey <- survey %>%
    mutate(  # Find out the main lang between mom & dad
        dad_mom_main_lang = 
            case_when(dad_mom_Mand_fq %in% Mand_fq ~ "華語",
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
            case_when(me_dad_Mand_fq %in% Mand_fq ~ "華語",
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
            case_when(me_mom_Mand_fq %in% Mand_fq ~ "華語",
                      me_mom_Mand_fq == "約一半" & 
                          me_mom_2nd_lang_fq == "約一半" ~ 
                          paste(me_mom_2nd_lang, "、華語各半", sep=""),
                      ! me_mom_Mand_fq %in% Mand_fq & 
                          ! me_mom_2nd_lang_fq %in% other_infq ~ 
                          me_mom_2nd_lang,
                      TRUE ~ "多語混用")
    )

##### Percentage of (main_lang == 華語) #####
## Return 3 percentages: dad_mom, me and mom/dad with age_range ##
P_Mand_as_main <-  function(df, age_range = c(16, 22)){
    df_filtered <- df %>%
        select(-date) %>%
        filter(age >= age_range[1] && age_range <= age_range[2]) %>%
        select(dad_mom_main_lang, me_dad_main_lang, me_mom_main_lang)
    df_filtered %>% map(function(x) x == "華語") %>%
        map(mean)
    }