library(data.table)
library(dplyr)

users_1month <- fread("../../gen/temp/users_1month_allmod.csv", select = c(2:11))

# times listened to 
tlt <- users_1month %>% count(artist)

# mean & variance
mean <- mean(tlt$n)
variance <- var(tlt$n)

# write to csv
write.csv(tlt, "../../gen/temp/tlt.csv")