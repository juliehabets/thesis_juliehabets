library(data.table)
library(dplyr)

users_1month <- fread("../../gen/temp/users_1month_allmod.csv", select = c(2:11))

# times listened to 
tlt <- users_1month %>% count(artist)

# number of users
nou <- users_1month[, c(1,3)] %>% distinct()
nou <- nou %>% count(artist)
