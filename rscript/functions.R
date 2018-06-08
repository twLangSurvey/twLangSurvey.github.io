library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)

#### Create variable "age_group" ####
## Set Group Range by argument 'range'  ##
## Require a numeric column named 'age' ## 
mutate_age_group <- function(df, range=5){
    df$age_group <- cut(df$age, right = F,
                    breaks = seq(10,95,by=range)) %>%
        as.character()
    
    idx1 <- paste("[", seq(10,90,range), ",", 
              seq(10+range,90+range, range), ")", 
              sep="")
    idx2 <- paste(seq(10,90, range), "-", 
              seq(9+range,89+range, range), sep="")
    
    for (i in seq_along(idx1)){
        df$age_group[df$age_group == idx1[i]] <-
            idx2[i]
    }
    df <- df %>% 
        select(age, age_group, everything())
    return(df)
}

#### Filter Ethnicity 1 ####
## Unilateral and seperated gender
filter_ethnic1 <- 
    function(df, select_by="mom", lang="Mand", lev=3) {
    sp_lang <- vector("character", 2)
    sp_lang[1] <- paste("dad_", lang, "_speak", sep = "")
    sp_lang[2] <- paste("mom_", lang, "_speak", sep = "")

    if (select_by == "mom") {
        idx <- 2
        filter_guard <- "f_guard_identity"
    } else {
        idx <- 1
        filter_guard <- "m_guard_identity"
    }
    row_idx <- df[[sp_lang[idx]]] >= lev
    df <- df %>%
        filter(filter_guard != "無") 
    df <- df[row_idx,]
    return(df)
    }

#### Filter Ethnicity 2 ####
## Bilateral. More general: Mom or Dad ##

filter_ethnic2 <- function(df, lang="Mand", lev=3) {
    sp_lang1 <- paste("dad_", lang, "_speak", sep = "")
    sp_lang2 <- paste("mom_", lang, "_speak", sep = "")
    idx <- (df[[sp_lang1]] >= lev | df[[sp_lang2]] >= lev)
    df <- df %>% filter(m_guard_identity != "無" | 
                   f_guard_identity != "無")
    df <- df[idx,]
    return(df)
}

##### Recode lang from En to Ch #####
recode_lang <- function(df, lang_col){
    lang_ch <- c('華語', '閩南語', '客家語', '原住民族語', '東南亞語言', '英語')
    lang <- c('Mand', 'Tw', 'Hak', 'Ind', 'SEA', 'Eng')
    for (i in seq_along(lang)){
        idx <- df[[lang_col]] == lang[i]
        df[[lang_col]][idx] <- lang_ch[i]
    }
    return(df)
}





##### Ploting ######
theme_define <- function(title =14, axis_size=12,
                  plot_title=14, sub_title=12,
                  legend_text=13, axis_title_size=14,
                  legend_posi = c("right", "top")){
    theme(title = element_text(size = title),
          axis.title = element_text(size = axis_size),
          axis.text = element_text(size = axis_size),
          plot.title = element_text(size = plot_title,
                               face="bold"),
          plot.subtitle = element_text(size = sub_title),
          legend.text = element_text(size = legend_text),
          legend.justification = legend_posi)
}