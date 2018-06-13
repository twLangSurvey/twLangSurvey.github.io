library(magrittr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

###### Read Data From google sheet ######
survey <- readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSRfcBk6Y0F73kAB6K1jkEr2g8ewZ_XMI7JYdEFpyKEb81WyWOOYOt4ECRm16bltFgs8i-sIR18Apig/pub?gid=0&single=true&output=csv", col_names = T)
# https://docs.google.com/spreadsheets/d/1JM9eqIiWt4uTa7CXKdYsSyXJIPKTpk_fVGSdK0O4gys/edit?usp=sharing

var_eng_ch <- readr::read_tsv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQU_SO82bnHAavH-qJM-VL8T2RPlFS5Uuzy4Pu9lsOYZjJ9tnSThx0tZs3bQDcIY7EEABDC8JKpfMcD/pub?gid=0&single=true&output=tsv", col_names = F)
# https://docs.google.com/spreadsheets/d/1NbalBJPAPIzGwpVDI9fTf0q8GpCBWhVep_9aD5uQKy4/edit?usp=sharing


###### Clean: split columns ######
## To make var.s with year format and  ##         
## var.s with 'post; city; town' clean ##

survey <- survey %>%
    set_names(var_eng_ch$X1) %>%
    filter(!is.na(date)) %>%
    separate(birth_year,
             into = c("birth_year", "jk1"),
             sep = 4) %>% # separate at position 4
    separate(curr_resid_since,
             into = c("curr_resid_since", "jk2"),
             sep = 4) %>% # separate at position 4
    separate(curr_resid,
             c("curr_resid", "jk3"),
             sep = "; ",
             extra = "drop") %>%
    separate(home_town,
             c("home_town", "jk4"),
             sep = "; ",
             extra = "drop") %>%
    select(-jk1, -jk2, -jk3, -jk4)


###### Recode lang ability/used freq   ######
## Convert points 1-6 to points 0-5 ##

index <- which(survey[1,] %in% 1:6) # find columns with likert scales (6 points) 

sub_1_likert <- function(x) as.numeric(x)-1

survey[, index] <- 
    survey[, index] %>%
    map(sub_1_likert)

###### Mutate columns ######

survey$date <- survey$date %>% 
    strptime("%m/%d/%Y %H:%M:%S",
             tz ="Asia/Taipei") %>% 
    as_datetime(tz="Asia/Taipei") %>%
    as_date()

survey <- survey %>%
    mutate(birth_year = year(Sys.Date()) - as.numeric(birth_year)) %>%
    rename(age = birth_year) %>%
    mutate(curr_resid_since = curr_resid_since %>% as.integer())

###### Replace with new value #######
survey$first_lang[survey$first_lang == "華語(國語)"] <- "華語"

###### Save cleaned data #######
readr::write_rds(survey, "./data/survey.rds")
readr::write_csv(survey, "./data/survey.csv")