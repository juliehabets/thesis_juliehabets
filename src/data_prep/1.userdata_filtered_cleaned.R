library(data.table)
library(dplyr)
library(lubridate)

#load data
userdata_1k <- fread("../../data/userdata_1k.csv")
userinfo_filtered <- fread("../../gen/temp/userinfo_filtered.csv")

#remove columns 
userdata_1k <- userdata_1k[-c(1)]

###################
#DATE MANIPULATION#
###################

# timestamp to dates
userdata_1k$date <- as.Date(userdata_1k$timestamp)

# dates to total time timestamps
userdata_1k <- userdata_1k %>% group_by(userid) %>% mutate(total_days = max(date)-min(date))

# days numeric
userdata_1k$total_days_num <- as.numeric(userdata_1k$total_days, unit = "days")

# months numeric
userdata_1k$months <- time_length(userdata_1k$total_days, unit = "months")

# rearrange colums
userdata_1k <- userdata_1k[, c(1, 2, 7:10, 3:6)]

#################
#FILTER 1 MONTHS#
#################

userdata_1k_1month <- userdata_1k %>% filter(months >= 1)

#####################
#DETERMINE DATERANGE#
#####################

# check for users with 6 months total listening time
users_1m <- userdata_1k_1month %>% filter(months < 2)
users_1m <- users_1m %>% group_by(userid) %>% mutate(mindate = min(date))
users_1m <- users_1m %>% group_by(userid) %>% mutate(maxdate = max(date))

table(users_1m$mindate)
table(users_1m$maxdate)
# So, the minimum dates are ranging all in the March 2009. Max dates are all in May 2009, except one who is in the end of April 2009. 
# Conclusion: dataset in April 2009 as this is a month where all have listened

# remove dataset for clean environment
rm(users_1m)

##################
#FILTER DATERANGE#
##################

users_1month <- userdata_1k_1month %>% filter(date >= "2009-04-01" & date <= "2009-04-30")

# semi join with userinfo_filtered to obtain only users with valid gender values
# users_1month <- semi_join(users_1month, userinfo_filtered, by = "userid") --> rm or check again


############
#ADD GENDER#
############
users_1month <- merge(users_1month, userinfo_filtered, by = "userid")
users_1month <- users_1month[-c(11, 13:15)]

# write to csv
write.csv(users_1month, file = "../../gen/temp/users_1month.csv")
