library(dplyr)
library(data.table)

# load dataset
artist_trackname <- fread("../../gen/temp/users_1month.csv", select = c(9, 11))
artists <- fread("../../data/discogs_artists.csv", sep = "\t", select = c(1:3))
tracks <- fread("../../data/discogs_tracks.csv", sep = "\t", select = c(1, 4, 5))

# unique values in artist & tracks
artists_unique_largedf <- unique(artists$artistname)
artists_unique_largedf_realname  <- unique(artists$realname)

tracks_unique_largedf <- unique(tracks$trackname)

# matching tracks 
match_tracks <- artist_trackname %>% filter(track_name %in% tracks_unique_largedf) %>% distinct(track_name, .keep_all = TRUE)
names(match_tracks)[2] <- "trackname"

match_tracks <- inner_join(match_tracks, tracks, by = "trackname")
match_tracks <- match_tracks[, -3]
match_tracks <- match_tracks[!duplicated(match_tracks), ]

# write to csv
write.csv(match_tracks, "../../gen/temp/match_tracks.csv")

# matching for artists
match_artists <- artist_trackname %>% filter(!(track_name %in% tracks_unique_largedf))
match_artists <- subset(match_artists, select = -c(2)) %>% distinct()

#write.csv(match_artists, "../../gen/temp/match_artists.csv") 
#match_artists <- fread("../../gen/temp/match_artists.csv")

match_artists <- match_artists[-1,]

match_artists1 <- match_artists %>% filter(V2 %in% artists_unique_largedf)
match_realname <- match_artists %>% filter(V2 %in% artists_unique_largedf_realname)
match_realname <- match_realname %>% filter(!(V2 %in% match_artists1$V2))

match_artists <- rbind(match_artists1, match_realname)

match_artists <- match_artists[, -1]
names(match_artists)[1] <- "artistname"

# add labels 
match_artists <- inner_join(match_artists, artists_labels_notrack, by = "artistname")
match_artists <- match_artists[, -c(2,3)]

# write to csv
write.csv(match_artists, "../../gen/temp/match_artists.csv") 
