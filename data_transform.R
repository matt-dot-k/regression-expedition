library(tidyverse)

gfp_rankings <- read.table("./data/gfp_rankings.txt", sep = ",", header = TRUE) %>%
    arrange(name) %>%
    rename(country = name) %>%
    select(country, power_index) %>%
    as_tibble()

gfp_vars <- read.csv("./data/gfp_vars.csv", sep = ",", header = TRUE) %>%
    select(-country, -country_code)

names(gfp_vars) <- gsub("\\.", "_", names(gfp_vars)) %>%
    str_to_lower()

firepower <- cbind(gfp_rankings, gfp_vars) %>%
    as_tibble()

write.csv(firepower, "./data/firepower.csv")