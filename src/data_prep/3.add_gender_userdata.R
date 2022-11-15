library(data.table)
library(dplyr)

# load data
users_1month <- fread("../../gen/temp/users_1month.csv")
users_1month <- users_1month[,-1]
userinfo_filtered <- fread("../../gen/temp/userinfo_filtered.csv")

############
#ADD GENDER#
############
users_1month <- merge(users_1month, userinfo_filtered, by = "userid")
users_1month <- users_1month[-c(2, 12, 14:16)]

# write to csv
write.csv(users_1month, file = "../../gen/temp/users_1month.csv")