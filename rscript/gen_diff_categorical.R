library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

survey <- readr::read_rds("./data/survey.rds") %>%
    filter(m_guard_identity != "無") %>%
    filter(f_guard_identity != "無")

###### Create new col's: main lang used ######
## between last gen (mom & dad) & cross gen (me & mom/dad) ##

# 幾乎全用 多數使用 約一半 少數使用 幾乎不用
langs <- c("Mand", "Tw", "Hak", "Ind", "SEA", "Other")
subj <- c("dad_mom", "me_dad", "me_mom")
ls <- vector("list", 3)
for (i in seq_along(subj)) ls[[i]] <- paste(subj[i], langs, "fq", sep = "_")
idx <- unlist(ls)
idx2 <- vector("integer", length(idx))
for (i in seq_along(idx)) idx2[i] <- which(colnames(survey)== idx[i])
survey[,idx2] <- map(survey[,idx2], 
                     function(x) factor(x, levels = c("幾乎不用", "少數使用", "約一半", "多數使用","幾乎全用")))


### Find out the Dominant Language Used by 
# dad_mom, me_mom, me_dad (in list ls[1], ls[2], ls[3], respectively)
max_idx <- matrix(nrow = nrow(survey), ncol = length(ls))
for(j in 1:nrow(survey)){
    for (i in seq_along(ls)) {
        max <- survey[j, ls[[i]]] %>% rowwise() %>% 
            as.integer() %>% max
        temp_idx <- which(as.integer(survey[j, ls[[i]]]) == 
                              max)
        ifelse(length(temp_idx) >= 2, 
               max_idx[j, i] <- temp_idx[length(temp_idx)],
               max_idx[j, i] <- temp_idx)
    }
}
survey <- survey %>%
    mutate(  # Find out the main lang between mom & dad
        dad_mom_main_lang = 
            case_when(max_idx[,1] == 1 ~ "華語",
                      max_idx[,1] == 2 ~ "閩南語",
                      max_idx[,1] == 3 ~ "客家語",
                      max_idx[,1] == 4 ~ "原住民族語",
                      max_idx[,1] == 5 ~ "東南亞語言",
                      max_idx[,1] == 6 ~ "其它語言")
        )%>%
        mutate( # Find out the main lang between me & dad
        me_dad_main_lang = 
            case_when(max_idx[,2] == 1 ~ "華語",
                      max_idx[,2] == 2 ~ "閩南語",
                      max_idx[,2] == 3 ~ "客家語",
                      max_idx[,2] == 4 ~ "原住民族語",
                      max_idx[,2] == 5 ~ "東南亞語言",
                      max_idx[,2] == 6 ~ "其它語言")
        )%>%
        mutate( # Find out the main lang between me & mom
        me_mom_main_lang = 
            case_when(max_idx[,3] == 1 ~ "華語",
                      max_idx[,3] == 2 ~ "閩南語",
                      max_idx[,3] == 3 ~ "客家語",
                      max_idx[,3] == 4 ~ "原住民族語",
                      max_idx[,3] == 5 ~ "東南亞語言",
                      max_idx[,3] == 6 ~ "其它語言")
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
    min <- min(df$age) + 0.001 # round(0.5) == 0
    max <- max(df$age) + 0.001 # round(0.5001) == 1
    if (round(min/10) > min/10 && 
        round(max/10) > max/10) {# 5-9 in both min, max
        min <- floor(min/10)*10 + 5
        max <- floor(max/10)*10 + 9}
    else if (round(min/10) <= min/10 &&  # 0-4 in min
              round(max/10) > max/10) {  # 5-9 in max
        min <- floor(min/10)*10
        max <- floor(max/10)*10 + 9        
    }
    else if (round(min/10) > min/10 &&   # 5-9 in min
              round(max/10) <= max/10) { # 0-4 in max
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
            P_Mand_as_main(df, c(min+(i-1)*5,min+(i-1)*5+4))
    }
    df2 <- bind_rows(list_df) # rbind df's from the list
    attr(df2, 'g_num') <- c(g_num, max, min)
    df2
}

#### Plot age_group bar plot ####
colors <- scales::hue_pal()(3)

rev_age_group <- rbind_df_PMand(survey)$age_group %>%
    unique() %>% rev()
pl_Mand_crossgen_bar <- rbind_df_PMand(survey) %>%
    gather(key = "subjects", value = "prob",
           -age_group) %>%
    arrange(desc(age_group)) %>%
    ggplot(aes(x = age_group, y = prob)) +
    geom_bar(
        aes(fill = subjects),
        colour = "black",
        width = 0.6,
        position = "dodge",
        stat = "identity"
    ) +
    scale_fill_manual(values = c("#00BA38", "#619CFF", "#F8766D"),
                      breaks = c("me_mom_main_lang",
                                 "me_dad_main_lang",
                                 "dad_mom_main_lang"),
                      labels = c("母親、自己",
                                 "父親、自己",
                                 "父親、母親")
    ) +
    labs(
        x = "年齡層",
        y = "比例",
        fill = "對話組合",
        title = expression(bold("華語") * "作為主要溝通語言之比例")
    ) + #theme_bw(base_size = 14)+
    theme_bw()+
    scale_x_discrete(limits=rev_age_group)
#tmaptools::palette_explorer()
# tmaptools::get_brewer_pal("Dark2", n = 6)

ggsave("../web_source/out_graph/Mand_usage_age_group.png",
       plot = pl_Mand_crossgen_bar, width=6, height = 3, dpi = 100)
