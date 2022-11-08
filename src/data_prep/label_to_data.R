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
users_tracks <- full_join(users_1month, match_tracks, by = "track_name")

# clean
users_tracks <- users_tracks[, -c(1,4,5)]
names(users_tracks)[1] <- c("artist")
users_tracks <- users_tracks[!duplicated(users_tracks), ]

# joining artists
users_artists <- full_join(users_1month, match_artists, by = "artist")

# clean
users_artists <- users_artists[, -c(1,4)]
users_artists <- users_artists[!duplicated(users_artists), ]

# check NAs
na <- users_tracks %>% filter(is.na(label))
na <- na %>% filter(!(artist %in% users_tracks$artist))

rm(na)
# NAs in labels from users_tracks are all from artists that are represented through other songs

# Add datasets together
total_label <- users_tracks
total_label$label[is.na(total_label$label)] <- users_artists$label[match(total_label$artist,users_artists$artist)][which(is.na(total_label$label))]

# clean
total_label <- total_label[!duplicated(total_label), ]

# checking NAs 
na <- total_label %>% filter(is.na(label))
na <- na %>% filter(!(artist %in% users_artists$artist))

rm(na)
# NAs in labels are all from artists that are represented through other songs

# remove NA values 
total_label <- total_label %>% filter(!is.na(label))

#write to csv
write.csv(total_label, "../../gen/temp/total_label.csv")
