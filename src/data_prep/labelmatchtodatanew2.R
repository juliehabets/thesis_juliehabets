library(data.table)
library(dplyr)

tracks <- fread("../../data/discogs_tracks.csv", sep = "\t", select = c(1, 4, 5), quote = "")
artists <- fread("../../data/discogs_artists.csv", sep = "\t", select = c(1:3), quote = "")
artist_trackname <- fread("../../gen/temp/users_1month.csv", select = c(4, 9:11))

artist_tracksname_test <- head(artist_trackname, 100)

names(artist_tracksname_test)[1] <- "trackname"
names(artist_trackname)[1] <- "trackname"

inner_match <- inner_join(artist_tracksname_test, tracks, by = "trackname")


# serious
artists_id_data <- artists %>% distinct(artistid, .keep_all = TRUE)
names(artists_id_data)[2] <- "artist"
artists_id_data$artist <- replace(artists_id_data$artist, artists_id_data$artist == "4 Hero", "4Hero")
artists_id_data <- full_join(artists_id_data, tracks, by = "artistid")
names(artists_id_data)[4] <- "trackname"
artists_id_data <- artists_id_data[!duplicated(artists_id_data), ]

matching_trakcs <- inner_join(artist_trackname, tracks, by = "trackname")


# ignore above 
artists_labels <- fread("../../gen/temp/artists_labels.csv")
artists_labels$artist <- replace(artists_labels$artist, artists_labels$artist == "4 Hero", "4Hero")

artist_trackname$artist <- tolower(artist_trackname$artist)
artist_trackname$track_name <- tolower(artist_trackname$track_name)
artists_labels$artist <- tolower(artists_labels$artist)
artists_labels$realname <- tolower(artists_labels$realname)
artists_labels$track_name <- tolower(artists_labels$track_name)

artist_trackname_label <- left_join(artist_trackname, artists_labels, by = c("track_name", "artist"))

na <- artist_trackname_label %>% filter(is.na(labels))
no_na <- artist_trackname_label %>% filter(!(is.na(labels)))
na <- na[, 1:4]

label_artist_count <- no_na %>% group_by(artist) %>% count(labels)
label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))

# keeping only most popular label
label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)

# keeping only 1 label per artist in case when the max counts are the same
label_artist_count <- label_artist_count %>% group_by(artist) %>% slice(n=1)
label_artist_count <- label_artist_count[, 1:2]

# impute NAs 
na_impute <- left_join(na, label_artist_count, by = "artist")

na_2 <- na_impute %>% filter(is.na(labels))
na_2 <- na_2[!duplicated(na_2), ]

na_2 <- na_2 %>% filter(!(artist %in% artists_labels))

# rejoining
#users_tracks <- rbind(no_na, na, na_check)
# check later
#na_a <- na %>% filter(artist %in% artists_labels$artist)
#na_t <- na %>% filter(track_name %in% artists_labels$track_name)
