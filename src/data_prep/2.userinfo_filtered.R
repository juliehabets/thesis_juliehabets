library(data.table)
library(dplyr)
library(lubridate)

# load data 
userinfo_1k <- fread("../../data/userinfo_1k.csv")
users_1month <- fread("../../gen/temp/users_1month.csv")

# filter for gender NA
#userinfo_filtered <- userinfo_1k %>% filter(!is.na(gender))

# remove unneccessary columns
#userinfo_filtered <- userinfo_filtered[-c(3:5)]


userinfo_filtered <- semi_join(userinfo_1k, users_1month, by = "userid")

# write to csv
write.csv(userinfo_filtered, file = "../../gen/temp/userinfo_filtered.csv")