library(data.table)
library(dplyr)
library(anytime)
library(ggplot2)

# load data 
users_1month <- fread("../../gen/temp/users_1month.csv", select = c(2, 4, 9, 11))
userinfo <- fread("../../gen/temp/userinfo_filtered.csv")
userdata_1k <- fread("../../data/userdata_1k.csv")
artists_labels <-fread("../../gen/temp/artists_labels.csv")

###########
#USER INFO#
###########

# check values gender 
table(userinfo$gender)
# % of NA
(71/764)*100

# check NA's for other variables
sum(userinfo$registered == "")
sum(userinfo$country == "")
sum(is.na(userinfo$age))

# age has too many NAs to prove valuable insights 

max_age <- max(userinfo$age, na.rm = T)
min_age <- min(userinfo$age, na.rm = T)
mean_age <- mean(userinfo$age, na.rm = T)

# checking out countries
# defining continents 
sa <- c("Argentina", "Brazil", "Chile", "Colombia", "Mexico", "Netherlands Antilles", "Nicaragua", "Peru",
        "Trinidad and Tobago", "Venezuela")
asia <- c("British Indian Ocean Territory", "China", "India", "Israel", "Japan", "Korea, Democratic People's Republic of",
          "Singapore", "Thailand", "Turkey")
eur <- c("Armenia", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia",
         "Czech Republic", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
         "Italy", "Latvia", "Lithuania", "Macedonia", "Malta", "Netherlands", "Norway", "Poland", "Portugal",
         "Romania", "Russian Federation", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
         "United Kingdom")
oce <- c("Australia", "New Zealand", "Northern Mariana Islands")
na <- c("Canada", "United States", "United States Minor Outlying Islands")
afr <- c("Congo, the Democratic Republic of the", "Cote D'Ivoire", "Morocco", "Tunisia", "Zimbabwe")

# applying continents 
sa <- userinfo %>% filter(country %in% sa)
asia <- userinfo %>% filter(country %in% asia)
eur <- userinfo %>% filter(country %in% eur)
oce <- userinfo %>% filter(country %in% oce)
na <- userinfo %>% filter(country %in% na)
afr <- userinfo %>% filter(country %in% afr)

# grouping 
western <- rbind(eur, oce, na)
non_western <- rbind(asia, sa, afr)


# signup dates
userinfo$registered <- anydate(userinfo$registered)

min_date <- min(userinfo$registered, na.rm = TRUE)
max_date <- max(userinfo$registered, na.rm = TRUE)
mean_date <- mean(userinfo$registered, na.rm = TRUE)

registered_strange <- userinfo %>% filter(registered > "2009-04-30")

# 3 strange registered dates, replace values with NA 
userinfo$registered[which(userinfo$userid %in% registered_strange$userid)] <- NA
rm(registered_strange)

###############
#USERS 1 MONTH#
###############

# check the amount of unique users
length(unique(users_1month$userid))

# unique artists in data set
length(unique(users_1month$artist))

# unique tracks in data set 
length(unique(users_1month$track_name))

# check min & max dates
min(users_1month$date)
max(users_1month$date)

# min & max date correspond to the time frame of the sample 

# check which artists are listened to the most
most_popular_artists <- as.data.frame(sort(table(users_1month$artist), decreasing = TRUE)[1:50])
(50/34191)*100

# check out depeche mode listeners
depechemode <- users_1month %>% filter(artist == "Depeche Mode")

# how many users?
length(unique(depechemode$userid))

# most popular tracks
sort(table(depechemode$track_name), decreasing = TRUE)[1:20]

###########
#USER DATA#
###########

userdata_1k <- userdata_1k[-c(1)]

# timestamp to dates
userdata_1k$date <- as.Date(userdata_1k$timestamp)

# dates to total time timestamps
userdata_1k <- userdata_1k %>% group_by(userid) %>% mutate(total_days = max(date)-min(date))

# days numeric
userdata_1k$total_days_num <- as.numeric(userdata_1k$total_days, unit = "days")

# trim down data
userdata_user_totaldays <- userdata_1k[, -c(1,3:9)]
userdata_user_totaldays <- userdata_user_totaldays[!duplicated(userdata_user_totaldays), ]

# filter impossible values
userdata_user_totaldays <- userdata_user_totaldays %>% filter(total_days_num <= 1541)
ggplot(userdata_user_totaldays, aes(total_days_num)) + 
  geom_histogram(color = "#bed6ff", fill = "#bed6ff", binwidth = 30) + 
  theme_light() +
  labs(x = "Total listening history in days", y = "Number of users")+
  theme(text = element_text(size = 12.5, family = "serif"))
ggsave("../../gen/output/total_listening_history_hist.png")


##############
#DISCOGS DATA#
##############
artists_labels <- artists_labels[, -c(1,4)]

length(unique(artists_labels$artistid))
length(unique(artists_labels$track_name))
length(unique(artists_labels$labels))
