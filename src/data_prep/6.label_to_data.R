library(data.table)
library(dplyr)

# load data 
match_tracks_join_full <- fread("../../gen/temp/match_tracks_join_full.csv")
match_tracks_join_inner <- fread("../../gen/temp/match_tracks_join_inner.csv")
match_artists <- fread("../../gen/temp/match_artists.csv")
users_1month <- fread("../../gen/temp/users_1month.csv")

# clean datasets --> IPV JOIN FULL DOE JOIN INNER
match_tracks_join_full <- match_tracks_join_full[, -1]
match_tracks_join_inner <- match_tracks_join_inner[, -c(1,2,4)]
names(match_tracks_join_inner)[c(2)] <- c("track_name")
names(match_tracks_join_full)[c(2)] <- c("track_name")

match_artists <- match_artists[-1, -c(1,3)]
names(match_artists)[c(1,2)] <- c("artist","label")
users_1month <- users_1month[, -c(1:3, 5:9, 11:17)]

# no duplicate rows
users_1month <- users_1month[!duplicated(users_1month), ]

###############
#MATCH ARTISTS#
###############

users_artists <- inner_join(users_1month, match_artists, by = "artist")

# clean
users_artists <- users_artists[!duplicated(users_artists), ]

# check NAs
users_artists_na <- users_artists %>% filter(is.na(label))
users_artists_nona <- users_artists %>% filter(!is.na(label))
na_check <- users_artists_na %>% filter(!(artist %in% users_artists_nona$artist))

# none of the artist NA's are in the no NA subset
#rm(na_check)

# in case of multiple labels per song
users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% count(label)
users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% mutate(max_n = max(n))

# keeping only most popular label
users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% filter(n == max_n)

# keeping only 1 label per artist in case when the max counts are the same
users_artists_nona <- users_artists_nona %>% group_by(artist, track_name) %>% slice(n=1)
users_artists_nona <- users_artists_nona[, 1:3]

# bind datasets together
users_artists <- rbind(users_artists_nona, users_artists_na)
users_artists <- users_artists[!duplicated(users_artists), ]

##############
#MATCH TRACKS#
##############

users_tracks <- inner_join(users_1month, match_tracks_join_inner, by = "track_name")

# clean
users_tracks <- users_tracks[, -3]
names(users_tracks)[c(2,3)] <- c("artist", "label")
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

total_label <- rbind(users_artists, users_tracks)


# find unmatching rows
no_tracks <- anti_join(users_1month, match_tracks_join_inner, by = "track_name")
no_tracks_yes_artists <- no_tracks %>% filter(artist %in% match_artists$artist)
no_tracks_yes_artists2 <- no_tracks %>% filter(artist %in% match_tracks_join_inner$artist)
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

users_artists_notracks <- inner_join(users_1month, full_no_tracks, by = c("track_name", "artist"))

users_tracks <- rbind(users_tracks, users_artists_notracks)

# so now i can use full_no_tracks to match labels to artists that are in other datasets


#######################
#COMBINE BOTH DATASETS#
#######################
######### HEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEERE SOMETHING GOES WRONG)x
# Add datasets together
total_label <- users_tracks
# filling in the label NAs from all datasets created 
total_label$label[is.na(total_label$label)] <- users_artists$label[match(total_label$artist,users_artists$artist)][which(is.na(total_label$label))]
total_label$label[is.na(total_label$label)] <- users_artists$label[match(total_label$track_name,users_artists$track_name)][which(is.na(total_label$label))]
total_label$label[is.na(total_label$label)] <- no_na$label[match(total_label$track_name,no_na$track_name)][which(is.na(total_label$label))]
total_label$label[is.na(total_label$label)] <- no_na$label[match(total_label$artist,no_na$artist)][which(is.na(total_label$label))]
total_label$label[is.na(total_label$label)] <- users_artists_nona$label[match(total_label$track_name,users_artists_nona$track_name)][which(is.na(total_label$label))]
total_label$label[is.na(total_label$label)] <- users_artists_nona$label[match(total_label$artist,users_artists_nona$artist)][which(is.na(total_label$label))]

# clean
total_label <- total_label[!duplicated(total_label), ]

# checking NAs 
na <- total_label %>% filter(is.na(label))
# in total 3950 unique artists & 5754 unique tracks that have NA for label

# remove NA values 
# total_label <- total_label %>% filter(!is.na(label))

#write to csv
write.csv(total_label, "../../gen/temp/total_label.csv")
