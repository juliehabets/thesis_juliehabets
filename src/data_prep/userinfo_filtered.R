library(readr)
library(dplyr)
library(lubridate)

# load data 
userinfo_1k <- read_csv("../../data/userinfo_1k.csv")

# filter for gender NA
#userinfo_filtered <- userinfo_1k %>% filter(!is.na(gender))

# remove unneccessary columns
#userinfo_filtered <- userinfo_filtered[-c(3:5)]

# write to csv
#write.csv(userinfo_filtered, file = "../../gen/temp/userinfo_filtered.csv")

userinfo_filtered <- semi_join(userinfo_1k, users_1month, by = "userid")
