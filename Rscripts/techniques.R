library(dplyr)

############ How to find out languages more fluent than chinese ############
df <- data_frame(
    x1 = sample(0:5, 10, replace = T), #lang1 ch
    x2 = sample(0:5, 10, replace = T), #lang2 tw
    x3 = sample(0:5, 10, replace = T)  #lang3 hk
    )

df <- df %>%
    mutate(x2_domin = if_else(x2 >= x1, T, F),
           x3_domin = if_else(x3 >= x1, T, F))

##################################################################
library(dplyr)

############ How to find out the most fluent language ############
df <- data_frame(
    x1 = rnorm(10), #lang1
    x2 = rnorm(10), #lang2
    x3 = rnorm(10)  #lang3
    )

df <- df %>%
    rowwise() %>%
    mutate(best_val = max(x1, x2, x3)) %>%
    ungroup() %>%
    mutate(best = if_else(x1 == best_val, "x1",
                          if_else(x2 == best_val, "x2",
                                  "x3")
                          )
           ) %>%
    select(-best_val)
##################################################################