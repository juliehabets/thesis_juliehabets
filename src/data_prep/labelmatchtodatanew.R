library(dplyr)
library(data.table)

# load dataset
artist_trackname <- fread("../../gen/temp/users_1month.csv", select = c(4, 10))
artists <- fread("../../data/discogs_artists.csv", sep = "\t", select = c(1:3), quote = "")
tracks <- fread("../../data/discogs_tracks.csv", sep = "\t", select = c(1, 4, 5), quote = "")
artists_label_notrack <- fread("../../gen/temp/artists_labels_notrack.csv")

# unique values in artist & tracks
artists_unique_largedf <- unique(artists$artistname)
artists_unique_largedf_realname  <- unique(artists$realname)

tracks_unique_largedf <- unique(tracks$trackname)

#################
#Matching tracks#
#################

match_tracks <- artist_trackname %>% filter(track_name %in% tracks_unique_largedf) %>% distinct(track_name, .keep_all = TRUE)
names(match_tracks)[1] <- "trackname"
names(match_tracks)[2] <- "artist"

artists_id_data <- artists %>% distinct(artistid, .keep_all = TRUE)
names(artists_id_data)[2] <- "artist"
artists_id_data$artist <- replace(artists_id_data$artist, artists_id_data$artist == "4 Hero", "4Hero")
artists_id_data <- full_join(artists_id_data, match_tracks, by = "artist")
names(artists_id_data)[4] <- "trackname"
artists_id_data <- artists_id_data[!duplicated(artists_id_data), ]

tracks_unique <- tracks %>% group_by(artistid) %>% distinct(trackname, .keep_all = TRUE)
artists_id_data <- artists_id_data[, -3]

# matching datasets 
match_tracks_join_inner <- inner_join(artists_id_data, tracks_unique, by = c("trackname", "artistid"))
match_tracks_join_full <- full_join(artists_id_data, tracks_unique, by = c("trackname", "artistid"))

# remove duplicates 
match_tracks_join_inner <- match_tracks_join_inner[!duplicated(match_tracks_join_inner), ]
match_tracks_join_full <- match_tracks_join_full[!duplicated(match_tracks_join_full), ]

# write to csv
write.csv(match_tracks_join_inner, "../../gen/temp/match_tracks_join_inner.csv")
write.csv(match_tracks_join_full, "../../gen/temp/match_tracks_join_full.csv")

##################
#Matching artists#
##################
names(artist_trackname)[1] <- "trackname"

match_artists <- artist_trackname %>% filter(!(trackname %in% tracks_unique_largedf))
match_artists <- subset(match_artists, select = -c(1)) %>% distinct()

unique_artist_matchtracks <- unique(match_tracks_join_inner$artist)
match_artists <- match_artists %>% filter(!(artist %in% unique_artist_matchtracks))

match_artists1 <- match_artists %>% filter(artist %in% artists_unique_largedf)
match_realname <- match_artists %>% filter(artist %in% artists_unique_largedf_realname)
match_realname <- match_realname %>% filter(!(artist %in% match_artists1$artist))

match_artists <- rbind(match_artists1, match_realname)

# add labels 
match_artists <- inner_join(match_artists, artists_label_notrack, by = "artist")
match_artists <- match_artists[, -c(2,3)]

# remove duplicates
match_artists <- match_artists[!duplicated(match_artists), ]

# write to csv
write.csv(match_artists, "../../gen/temp/match_artists.csv") 
