library(data.table)
library(dplyr)

# load data 
match_tracks <- fread("../../gen/temp/match_tracks_join_inner.csv", select = c(3:5))
match_artists <- fread("../../gen/temp/match_artists.csv", select = c(2,4))
users_1month <- fread("../../gen/temp/users_1month.csv")
artists_labels <- fread("../../gen/temp/artists_labels.csv", select = c(2:6))

# clean datasets
names(match_tracks)[c(2,3)] <- c("track_name", "label")
match_artists <- match_artists[-1, ]
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

###############
#JOIN TOGETHER#
###############

total_label <- rbind(users_artists, users_tracks)

#write to csv
write.csv(total_label, "../../gen/temp/total_label.csv")
