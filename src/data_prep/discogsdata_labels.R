library(data.table)
library(dplyr)

# load datasets
artists <- fread("../../data/discogs_artists.csv", sep = "\t", select = c(1:3))
tracks <- fread("../../data/discogs_tracks.csv", sep = "\t", select = c(1, 4, 5))

# merge datasets 
artists_labels <- merge(artists, tracks, by = "artistid")
names(artists_labels)[4] <- "track_name"
names(artists_labels)[2] <- "artist"

# write to csv
write.csv(artists_labels, "../../gen/temp/artists_labels.csv")

# without tracks 
artists_labels_notrack <- artists_labels[,-4]
artists_labels_notrack <- distinct(artists_labels_notrack, artists_labels_notrack$labels, .keep_all = TRUE)
artists_labels_notrack <- subset(artists_labels_notrack, select = -c(`artists_labels_notrack$labels`))

# write to csv
write.csv(artists_labels_notrack, "../../gen/temp/artists_labels_notrack.csv")
