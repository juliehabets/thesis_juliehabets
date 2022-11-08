library(data.table)
library(dplyr)

# load data 
match_tracks <- fread("../../gen/temp/match_tracks.csv")
match_artists <- fread("../../gen/temp/match_artists.csv")
users_1month <- fread("../../gen/temp/users_1month.csv")

# clean datasets
match_tracks <- match_tracks[-1, -1]
names(match_tracks)[c(1,2,3)] <- c("artist", "track_name", "label")
match_artists <- match_artists[-1, -1]
names(match_artists)[c(1,2,3)] <- c("artist","artist_realname", "label")
users_1month <- users_1month[, -c(1, 3:8, 10)]
# no duplicate rows
users_1month$users_1month <- users_1month[!duplicated(users_1month), ]

# joining tracks
users_1month_tracks <- full_join(users_1month, match_tracks, by = "track_name")
users_1month_tracks <- users_1month_tracks[!duplicated(users_1month_tracks), ]

# clean
users_1month_tracks <- users_1month_tracks[, -5]
names(users_1month_tracks)[2] <- c("artist")

# joining artists
users_1month_tracks_artists <- full_join(users_1month_tracks, match_artists, by = "artist")
