library(data.table)
library(dplyr)

users_1month <- fread("../../gen/temp/users_1month_allmod.csv", select = c(2:11))

# times listened to 
ts <- users_1month %>% count(artist)

# mean & variance
mean <- mean(ts$n)
variance <- var(ts$n)

# write to csv
write.csv(ts, "../../gen/temp/ts.csv")
