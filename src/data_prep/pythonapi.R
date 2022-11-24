library(data.table)
library(dplyr)

artist_label_MBID <- fread("finartist_label_mbid2.csv")
track_label_MBID <- fread("fintrack_label_mbid.csv")

# clean
artist_label_MBID <- artist_label_MBID[-1,-c(1,4)]
names(artist_label_MBID)[c(1,2,3)] <- c("artist", "artist_MBID", "label")

track_label_MBID <- track_label_MBID[-1,-c(1,4)]
names(track_label_MBID)[c(1,2,3)] <- c("track_name", "track_MBID", "label")
