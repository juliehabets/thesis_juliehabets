library(data.table)
library(dplyr)
library(anytime)
library(ggplot2)
library(stringr)

# load data 
users_1month <- fread("../../gen/temp/users_1month_allmod.csv", select = c(2:11))
userinfo <- fread("../../gen/temp/userinfo_filtered.csv", select = c(2:6))
userdata_1k <- fread("../../data/userdata_1k.csv")
artists_labels <-fread("../../gen/temp/artists_labels.csv")
artists <- fread("../../data/discogs_artists.csv", sep = "\t", select = c(1:3))
tracks <- fread("../../data/discogs_tracks.csv", sep = "\t", select = c(1, 4, 5))
artist_remuneration <- fread("../../gen/temp/artist_remuneration_final_inclna.csv", select = c(2:7))
tlt <- fread("../../gen/temp/tlt.csv", select = c(2:3))

###########
#USER INFO#
###########

# check values gender 
table(userinfo$gender)
# % of NA
(71/775)*100

# bar chart
pcdf <- c(288, 416)

pie(pcdf, labels = c("Female", "Male"), col = c("#bed6ff", "#FFE8BE"), main = "Division of Gender", family = "serif")

# check NA's for other variables
sum(userinfo$registered == "")
sum(userinfo$country == "")
sum(is.na(userinfo$age))

# age has too many NAs to prove valuable insights 

max_age <- max(userinfo$age, na.rm = T)
min_age <- min(userinfo$age, na.rm = T)
mean_age <- mean(userinfo$age, na.rm = T)

# bar chart
ages <- userinfo %>% filter(!(is.na(age)))
ages <- ages[ , 3]
ggplot(ages, aes(x = age)) + geom_bar(color = "#bed6ff", fill = "#bed6ff") + theme_light() + labs(x = "User Age", y = "Number of users")+ theme(text = element_text(size = 12.5, family = "serif"))
  
  
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

# pie chart
pcc <- c(4, 37, 412, 193, 21, 54)
pie(pcc, labels = c("Africa", "Asia", "Europe", "Northern America", "Oceania", "Southern America"), col = c("#C3A56C", "#FFE8BE", "#506B99", "#bed6ff", "#FFA900", "#80AFFF"), main = "Division of User Origin", family = "serif")


# signup dates
userinfo$registered <- anydate(userinfo$registered)

min_date <- min(userinfo$registered, na.rm = TRUE)
max_date <- max(userinfo$registered, na.rm = TRUE)
mean_date <- mean(userinfo$registered, na.rm = TRUE)

registered_strange <- userinfo %>% filter(registered > "2009-04-30")

# 3 strange registered dates, replace values with NA 
userinfo$registered[which(userinfo$userid %in% registered_strange$userid)] <- NA
rm(registered_strange)

# line chart
ggplot(userinfo %>% count(registered), aes(x = registered)) + geom_histogram(color = "#bed6ff", fill = "#bed6ff", binwidth = 30) + theme_light() + labs(x = "Register Date", y = "Number of users")+ theme(text = element_text(size = 12.5, family = "serif"))
 


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

##########
##labels##
##########

# NA
sum(is.na(users_1month$label_type))
(22498/601676)*100
length(unique(users_1month$label))

# which kind of artists are NA
na_label <- users_1month %>% filter(is.na(label))
sum(str_count(na_label$artist, "dj"))
sum(str_count(na_label$artist, "orchestra"))
sum(str_count(na_label$artist, "symphony"))
sum(str_count(na_label$artist, "choir"))
sum(str_count(na_label$artist, "philharmonia"))
na_label_count <- na_label %>% count(label)
summary(na_label_count)
sum(na_label_count$n == 1)
sum(na_label_count$n == 2)

# which users listen to NA labels
length(unique(na_label$userid))
na_label_users <- userinfo %>% filter(userid %in% na_label$userid)
table(na_label_users$gender)
male <- (329/601)*100
table(na_label_users$age)
table(na_label_users$country)

# label info 
# user-level
labels <- data.frame(users_1month$label)
labels <- labels %>% count(users_1month.label) %>% filter(!is.na(users_1month.label))
labels_artist <- users_1month[, c(3, 7:9)] %>% distinct() %>% count(label) %>% filter(!(is.na(label)))

# % of major labels/independent labels
major_labels <- users_1month %>% filter(label_type == 1)
(232240/601676)*100
length(unique(major_labels$label))0
indie_labels <- users_1month %>% filter(label_type == 0)
length(unique(indie_labels$label))
(346938/601676)*100
(22498/601676)*100

# pie chart
pcc <- c(22498, 232240, 346938)
pie(pcc, labels = c("NA - 3.74%", "Major label - 38.60%", "Independent label - 57.66%"), col = c("#bed6ff", "#FFE8BE", "#506B99"), family = "serif")

# artists under label types
major_labels_artists <- major_labels[, 3] %>% distinct()
indie_labels_artists <- indie_labels[, 3] %>% distinct()
na_labels_artists <- na_label %>% count(artist)
(6407/31735)*100
(17287/31735)*100
(8041/31735)*100

# pie chart
pcc <- c(8041, 6407, 17287)
pie(pcc, labels = c("NA - 25.34%", "Major label - 20.19%", "Independent label - 54.47%"), col = c("#bed6ff", "#FFE8BE", "#506B99"), family = "serif")

########################
##artist sanity checks##
########################

# check which artists are listened to the most
most_popular_artists <- as.data.frame(sort(table(users_1month$artist), decreasing = TRUE)[1:50])

# check out justice listeners
justice <- users_1month %>% filter(artist == "justice")
justice <- merge(justice, userinfo, by = "userid")
table(justice$country)

# how many users?
length(unique(justice$userid))

justice_users <-unique(justice$userid)
justice_users <- userinfo %>% filter(userid %in% justice_users)
table(justice_users$country)

# most popular tracks
sort(table(justice$track_name), decreasing = TRUE)[1:10]

#######################
##track sanity checks##
#######################

# check which artists are listened to the most
most_popular_tracks <- rename(count(users_1month, artist, track_name), Freq = n)
user_000865 <- users_1month %>% filter(userid == "user_000865")
user_000865_count <-rename(count(user_000865 , artist, track_name), Freq = n)
length(unique(user_000865$artist))
length(unique(user_000865$track_name))

#####################
#ARTIST REMUNERATION#
#####################

artist_remuneration <- merge(artist_remuneration, tlt, by = "artist")
names(artist_remuneration)[7] <- "tlt"

#############
##ratio fem##
#############

mean_rf <- mean(artist_remuneration$ratiofem)
variance_rf <- var(artist_remuneration$ratiofem)
summary(artist_remuneration$ratiofem)

# plot
ggplot(artist_remuneration, aes(x = ratiofem)) + geom_histogram(color = "#bed6ff", fill = "#bed6ff", bins = 15) + theme_light() + labs(x = "Ratio female", y = "Number of occurrences")+ theme(text = element_text(size = 12.5, family = "serif"))

#######
##tlt##
#######

mean_tlt <- mean(artist_remuneration$tlt)
variance_tlt <- var(artist_remuneration$tlt)

# plot
ggplot(artist_remuneration, aes(x = tlt)) + geom_histogram(color = "#bed6ff", fill = "#bed6ff", bins = 100) + theme_light() + labs(x = "Times listened to", y = "Number of occurrences")+ theme(text = element_text(size = 12.5, family = "serif"))


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
