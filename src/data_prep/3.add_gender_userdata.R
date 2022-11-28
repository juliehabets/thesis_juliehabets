library(data.table)
library(dplyr)

# load data
users_1month <- fread("../../gen/temp/users_1month.csv", select = c(2:11))
userinfo_filtered <- fread("../../gen/temp/userinfo_filtered.csv", select = c(2:3))

############
#ADD GENDER#
############
users_1month <- merge(users_1month, userinfo_filtered, by = "userid")

# write to csv
write.csv(users_1month, file = "../../gen/temp/users_1month.csv")
