library(data.table)
library(dplyr)

# load data 
match_tracks <- fread("../../gen/temp/match_tracks_join_inner.csv")
match_artists <- fread("../../gen/temp/match_artists.csv")
users_1month <- fread("../../gen/temp/users_1month.csv")
artists_labels <- fread("../../gen/temp/artists_labels.csv")

# clean datasets
match_tracks <- match_tracks[, -c(1,2,4)]
names(match_tracks)[c(2)] <- c("track_name")

##########
#TO LOWER#
##########

artists_labels$track_name <- tolower(artists_labels$track_name)
artists_labels$artist  <- tolower(artists_labels$artist)
artists_labels$realname <- tolower(artists_labels$realname)

match_artists <- match_artists[-1, -c(1,3)]
names(match_artists)[c(1,2)] <- c("artist","label")
users_1month_trackartist <- users_1month[, -c(1:3, 5:8, 12:17)]

users_1month_trackartist$track_name <- tolower(users_1month_trackartist$track_name)
users_1month_trackartist$artist  <- tolower(users_1month_trackartist$artist)

match_artists$artist  <- tolower(match_artists$artist)

match_tracks$track_name <- tolower(match_tracks$track_name)
match_tracks$artist  <- tolower(match_tracks$artist)

users_1month$track_name <- tolower(users_1month$track_name)
users_1month$artist  <- tolower(users_1month$artist)

# no duplicate rows
users_1month_trackartist <- users_1month_trackartist[!duplicated(users_1month_trackartist), ]

###############
#MATCH ARTISTS#
###############

users_artists <- inner_join(users_1month_trackartist, match_artists, by = "artist")

# clean
users_artists <- users_artists[!duplicated(users_artists), ]

# check NAs
#users_artists_na <- users_artists %>% filter(is.na(label))
#users_artists_nona <- users_artists %>% filter(!is.na(label))
#na_check <- users_artists_na %>% filter(!(artist %in% users_artists_nona$artist))

# none of the artist NA's are in the no NA subset
#rm(na_check)

# in case of multiple labels per song
#users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% count(label)
#users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% mutate(max_n = max(n))

# keeping only most popular label
#users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% filter(n == max_n)

# keeping only 1 label per artist in case when the max counts are the same
#users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% slice(n=1)
#users_artists_nona <- users_artists_nona[, 1:3]

# bind datasets together
#users_artists <- rbind(users_artists_nona, users_artists_na)
#users_artists <- users_artists[!duplicated(users_artists), ]

##############
#MATCH TRACKS#
##############

users_tracks <- inner_join(users_1month_trackartist, match_tracks, by = "track_name")

# clean
users_tracks <- users_tracks[, -5]
names(users_tracks)[c(3,5)] <- c("artist", "label")
users_tracks <- users_tracks[!duplicated(users_tracks), ]

# check NAs
#na <- users_tracks %>% filter(is.na(label))
#no_na <- users_tracks %>% filter(!(is.na(label)))
#na_check <- na %>% filter(artist %in% no_na$artist)

# Out of 77820 NAs in labels from users_tracks, 53844 are from artists that are represented through other songs in the dataset
# Unique artists in NA dataset: 23716, unique artists in no NA dataset: 20449 and unique artists in the check list: 9973

# impute NAs
#label_artist_count <- no_na %>% group_by(artist) %>% count(label)
#label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))

# keeping only most popular label
#label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)

# keeping only 1 label per artist in case when the max counts are the same
#label_artist_count <- label_artist_count %>% group_by(artist) %>% slice(n=1)
#label_artist_count <- label_artist_count[, 1:2]

# impute NAs 
#na_check <- merge(na_check, label_artist_count, by = "artist")
#na_check <- na_check[, -3]
#names(na_check)[c(3)] <- c( "label")

# rejoining
#users_tracks <- rbind(no_na, na, na_check)
#users_tracks <- users_tracks[!duplicated(users_tracks), ]

#############
# TRY FIX ###
#############

total_label <- rbind(users_artists, users_tracks) ## DEZE GEBRUIK IK VOOR LABELMATCHFINAL!!!!!!!!!!! ZO ALS IE NU IS


# find unmatching rows
no_tracks <- anti_join(users_1month_trackartist, match_tracks, by = "track_name")
no_tracks_yes_artists <- no_tracks %>% filter(artist %in% match_artists$artist)
no_tracks_yes_artists2 <- no_tracks %>% filter(artist %in% match_tracks$artist)
# join together 
full_tracks <- rbind(no_tracks_yes_artists, no_tracks_yes_artists2)
full_tracks <- full_tracks[!duplicated(full_tracks), ]

# so now i have matched many of the tracks that cannot be matched to tracks in the match_tracks data with artists that are either in the match_
# track already or who are in the match_artists file 

# impute NAs
label_artist_count <- users_tracks %>% group_by(artist) %>% count(label)
label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))
label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)
label_artist_count_1 <- label_artist_count %>% group_by(artist) %>% filter(n == 1) %>% slice(n=1)
label_artist_count_n <- label_artist_count %>% group_by(artist) %>% filter(n >1)
label_artist_count <- rbind(label_artist_count_1, label_artist_count_n)
label_artist_count <- label_artist_count[, 1:2]

full_no_tracks <- merge(full_tracks, label_artist_count, by = "artist")

users_artists_notracks <- inner_join(users_1month_trackartist, full_no_tracks, by = c("track_name", "artist"))

users_tracks <- rbind(users_tracks, users_artists_notracks)

# so now i can use full_no_tracks to match labels to artists that are in other datasets


#######################
#COMBINE BOTH DATASETS#
#######################

total_label <- rbind(users_tracks, users_artists)
total_label <- total_label[!duplicated(total_label), ]

total_label <- left_join(users_1month, total_label, by = c("track_name", "artist"))

total_label <- total_label[, -c(1,3,5:8, 12, 13, 15:17)]

sum(is.na(total_label$label))

#write to csv
write.csv(total_label, "../../gen/temp/total_label.csv")

# Add datasets together
#total_label <- users_tracks
# filling in the label NAs from all datasets created 
#total_label$label[is.na(total_label$label)] <- users_artists$label[match(total_label$artist,users_artists$artist)][which(is.na(total_label$label))]
#total_label$label[is.na(total_label$label)] <- users_artists$label[match(total_label$track_name,users_artists$track_name)][which(is.na(total_label$label))]
#total_label$label[is.na(total_label$label)] <- no_na$label[match(total_label$track_name,no_na$track_name)][which(is.na(total_label$label))]
#total_label$label[is.na(total_label$label)] <- no_na$label[match(total_label$artist,no_na$artist)][which(is.na(total_label$label))]
#total_label$label[is.na(total_label$label)] <- users_artists_nona$label[match(total_label$track_name,users_artists_nona$track_name)][which(is.na(total_label$label))]
#total_label$label[is.na(total_label$label)] <- users_artists_nona$label[match(total_label$artist,users_artists_nona$artist)][which(is.na(total_label$label))]


# checking NAs 
na <- total_label %>% filter(is.na(label))
no_na <- total_label %>% filter(!(is.na(label)))
length(unique(na$artist))
length(unique(no_na$artist))
# in total 3950 unique artists & 5754 unique tracks that have NA for label

# trying with sample first

filter_tracks_sample <- head(filter_tracks, 10)
filter_tracks_sample$track_name <- tolower(filter_tracks_sample$track_name)
filter_tracks_sample$artist <- tolower(filter_tracks_sample$artist)

filter_tracks_label_test <- left_join(filter_tracks_sample, artists_labels, by = c("track_name", "artist"))
filter_tracks_label_inner <- inner_join(filter_tracks_sample, artists_labels, by = c("track_name", "artist"))

# left is better than inner

# real deal
filter_tracks <- na %>% filter(track_name %in% artists_labels$track_name)
filter_tracks <- left_join(filter_tracks, artists_labels, by = c("track_name", "artist"))

filter_tracks <- filter_tracks[, -c(1, 8:10)]
#write to csv
write.csv(filter_tracks, "../../gen/temp/filter_tracks.csv")

#########
#ARTISTS#
#########

filter_artists <- na %>% filter(artist %in% artists_labels$artist | artist %in% artists_labels$realname) 
filter_artists <- filter_artists %>% filter(!(artist %in% filter_tracks$artist))

filter_artists <- left_join(filter_artists, artists_labels, by = "artist") %>% distinct(.keep_all = TRUE)
filter_artists <- filter_artists[, -c(7:9, 11)]
names(filter_artists)[c(2)] <- c("track_name")

# reduce to only artists, artist MBID & label
artists_final <- filter_artists[, c(3,4,8)]
artists_final <- artists_final[!duplicated(artists_final), ]

tracks_final <- filter_tracks[, c(3,4,8)]
tracks_final <- tracks_final[!duplicated(tracks_final), ]

no_na_final <- no_na[, c(3,4,7)]
no_na_final <- no_na_final[!duplicated(no_na_final), ]
names(no_na_final)[3] <- "labels"

label_final <- rbind(no_na_final, tracks_final, artists_final)
label_final <- label_final[!duplicated(label_final), ]

# write to csv
write.csv(label_final, "../../gen/temp/label_final.csv")

label_final_final <- label_final[, -1]

# keeping only the first 5 labels
label_final_test <- label_final %>% group_by(artist) %>% count(labels)
label_final_test <- label_final_test %>% group_by(artist) %>% mutate(max_n = max(n))
label_final_test <- label_final_test %>% group_by(artist) %>% filter(!(is.na(labels)))
label_final_test <- label_final_test %>% group_by(artist) %>% filter(n == max_n)

label_final_test_1 <- label_final_test %>% group_by(artist) %>% filter(n == 1) # %>% slice(n = 5)
label_final_test_n <- label_final_test %>% group_by(artist) %>% filter(n >1)
label_final_test <- rbind(label_final_test_1, label_final_test_n)
label_final_test <- label_final_test[, 1:2]

# remove NA values 
# total_label <- total_label %>% filter(!is.na(label))

# to data
u1m <- left_join(users_1month, label_final_final, by =  "artist")

# extract MBID of NA's 
na_u1m <- u1m %>% filter(is.na(labels))
na_u1m <- na_u1m[, c(9:11)]
na_u1m <- na_u1m[!duplicated(na_u1m), ]
na_u1m$track_MBID[na_u1m$track_MBID == ""] <- NA
na_u1m$track_MBID[na_u1m$track_MBID == ""] <- NA

na_u1m_artistMBID <- na_u1m %>% filter(!(is.na(artist_MBID)))
na_u1m_artistMBID <- na_u1m_artistMBID[, -3]
na_u1m_artistMBID <- na_u1m_artistMBID[!duplicated(na_u1m_artistMBID), ]

na_u1m_trackMBID <- na_u1m %>% filter(!(is.na(track_MBID)))
na_u1m_trackMBID <- na_u1m_trackMBID[, -1]
na_u1m_trackMBID <- na_u1m_trackMBID[!duplicated(na_u1m_trackMBID), ]

# write to csv
write.csv(na_u1m_artistMBID, "../../gen/temp/na_u1m_artistMBID.csv")
write.csv(na_u1m_trackMBID, "../../gen/temp/na_u1m_trackMBID.csv")
