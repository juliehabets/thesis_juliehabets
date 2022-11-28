library(data.table)
library(dplyr)

# load data 
match_tracks <- fread("../../gen/temp/match_tracks_join_inner.csv", select = c(3:5))
match_artists <- fread("../../gen/temp/match_artists.csv", select = c(2,4))
users_1month <- fread("../../gen/temp/users_1month.csv")
artists_labels <- fread("../../gen/temp/artists_labels.csv", select = c(2:6))

# clean datasets
names(match_tracks)[c(2,3)] <- c("track_name", "label")
match_artists <- match_artists[-1,]
names(match_artists)[c(1,2)] <- c("artist","label")
users_1month_trackartist <- users_1month[, -c(1:7, 12)]

##########
#TO LOWER#
##########

artists_labels$track_name <- tolower(artists_labels$track_name)
artists_labels$artist  <- tolower(artists_labels$artist)
artists_labels$realname <- tolower(artists_labels$realname)

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

##############
#MATCH TRACKS#
##############

users_tracks <- inner_join(users_1month_trackartist, match_tracks, by = "track_name")

# clean
users_tracks <- users_tracks[, -5]
names(users_tracks)[c(2,5)] <- c("artist", "label")
users_tracks <- users_tracks[!duplicated(users_tracks), ]

####################
#FURTHER PROCESSING#
####################

# find unmatching rows
no_tracks <- anti_join(users_1month_trackartist, match_tracks, by = "track_name")
no_tracks_yes_artists <- no_tracks %>% filter(artist %in% match_artists$artist)
no_tracks_yes_artists2 <- no_tracks %>% filter(artist %in% match_tracks$artist)
# join together 
full_tracks <- rbind(no_tracks_yes_artists, no_tracks_yes_artists2)
full_tracks <- full_tracks[!duplicated(full_tracks), ]

# impute NAs
label_artist_count <- users_tracks %>% group_by(artist) %>% count(label)
label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))
label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)
label_artist_count_1 <- label_artist_count %>% group_by(artist) %>% filter(n == 1) %>% slice(n=1)
label_artist_count_n <- label_artist_count %>% group_by(artist) %>% filter(n >1)
label_artist_count <- rbind(label_artist_count_1, label_artist_count_n)
label_artist_count <- label_artist_count[, 1:2]

full_no_tracks <- merge(full_tracks, label_artist_count, by = "artist")

# join dataset
users_artists_notracks <- inner_join(users_1month_trackartist, full_no_tracks, by = c("track_name", "artist"))
users_artists_notracks <- users_artists_notracks[, -c(5,6)]
names(users_artists_notracks)[c(1,3)] <- c("artist_MBID", "track_MBID")

# add together
users_tracks <- rbind(users_tracks, users_artists_notracks)

#######################
#COMBINE BOTH DATASETS#
#######################

total_label <- rbind(users_tracks, users_artists)
total_label <- total_label[!duplicated(total_label), ]

total_label <- left_join(users_1month, total_label, by = c("track_name", "artist"))

total_label <- total_label[, -c(1,3:7, 13:14)]
names(total_label)[c(2,4)] <- c("artist_MBID", "track_MBID")

sum(is.na(total_label$label))

# checking NAs 
na <- total_label %>% filter(is.na(label))
no_na <- total_label %>% filter(!(is.na(label)))

filter_tracks <- na %>% filter(track_name %in% artists_labels$track_name)
filter_tracks <- left_join(filter_tracks, artists_labels, by = c("track_name", "artist"))

filter_tracks <- filter_tracks[, -c(7:9)]


#########
#ARTISTS#
#########

filter_artists <- na %>% filter(artist %in% artists_labels$artist | artist %in% artists_labels$realname) 
filter_artists <- filter_artists %>% filter(!(artist %in% filter_tracks$artist))

filter_artists <- left_join(filter_artists, artists_labels, by = "artist") %>% distinct(.keep_all = TRUE)
filter_artists <- filter_artists[, -c(7:10)]
names(filter_artists)[c(5)] <- c("track_name")

# reduce to only artists, artist MBID & label
artists_final <- filter_artists[, c(2,3,7)]
artists_final <- artists_final[!duplicated(artists_final), ]

tracks_final <- filter_tracks[, c(2,3,7)]
tracks_final <- tracks_final[!duplicated(tracks_final), ]

no_na_final <- no_na[, c(2, 3,7)]
no_na_final <- no_na_final[!duplicated(no_na_final), ]
names(no_na_final)[3] <- "labels"

label_final <- rbind(no_na_final, tracks_final, artists_final)
label_final <- label_final[!duplicated(label_final), ]

# write to csv
write.csv(label_final, "../../gen/temp/label_final.csv")

label_final_final <- label_final[, -1]

#################
#PROJECT TO DATA#
#################

u1m <- left_join(users_1month, label_final_final, by =  "artist")

# extract MBID of NA's 
na_u1m <- u1m %>% filter(is.na(labels))
na_u1m <- na_u1m[, c(8:10)]
na_u1m <- na_u1m[!duplicated(na_u1m), ]
na_u1m$track_MBID[na_u1m$track_MBID == ""] <- NA
na_u1m$artist_MBID[na_u1m$artist_MBID == ""] <- NA

na_u1m_artistMBID <- na_u1m %>% filter(!(is.na(artist_MBID)))
na_u1m_artistMBID <- na_u1m_artistMBID[, -3]
na_u1m_artistMBID <- na_u1m_artistMBID[!duplicated(na_u1m_artistMBID), ]

na_u1m_trackMBID <- na_u1m %>% filter(!(is.na(track_MBID)))
na_u1m_trackMBID <- na_u1m_trackMBID[, -1]
na_u1m_trackMBID <- na_u1m_trackMBID[!duplicated(na_u1m_trackMBID), ]

# write to csv
write.csv(na_u1m_artistMBID, "../../gen/temp/na_u1m_artistMBID.csv")
write.csv(na_u1m_trackMBID, "../../gen/temp/na_u1m_trackMBID.csv")

