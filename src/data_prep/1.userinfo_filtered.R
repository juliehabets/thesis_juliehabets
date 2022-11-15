library(data.table)
library(dplyr)
library(lubridate)

# load data 
userinfo_1k <- fread("../../data/userinfo_1k.csv")
users_1month <- fread("../../gen/temp/users_1month.csv")

# semi join with users_1month to filter out the users that are not in our interest
userinfo_filtered <- semi_join(userinfo_1k, users_1month, by = "userid")
userinfo_filtered <- userinfo_filtered[, -1]

# write to csv
write.csv(userinfo_filtered, file = "../../gen/temp/userinfo_filtered.csv")
