library(data.table)
library(dplyr)

users_1month <- fread("../../gen/temp/users_1month.csv", select = c(2,4,9,10,11,14))
total_label <- fread("../../gen/temp/total_label.csv")
artist_label_MBID <- fread("finartist_label_mbid2.csv")
track_label_MBID <- fread("fintrack_label_mbid.csv")

# clean the loaded files
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3,4,5,6,7)] <- c("userid", "track_name", "artist_MBID", "artist", "track_MBID", "gender", "label")

artist_label_MBID <- artist_label_MBID[-1,-c(1,4)]
names(artist_label_MBID)[c(1,2,3)] <- c("artist", "artist_MBID", "label")

track_label_MBID <- track_label_MBID[-1,-c(1,4)]
names(track_label_MBID)[c(1,2,3)] <- c("track_name", "track_MBID", "label")

# lower the artists & tracknames to facilitate matching
users_1month$track_name <- tolower(users_1month$track_name)
users_1month$artist  <- tolower(users_1month$artist)

# subset to facilitate matching
users_1month_trackartist <- users_1month[, c(2:5)]

# match
label_match <- users_1month_trackartist[, 3]
label_match <- label_match[!duplicated(label_match), ]

label_match <- inner_join(label_match, total_label, by = "artist") 
label_match <- full_join(users_1month_trackartist, label_match)

na <- label_match %>% filter(is.na(label))
no_na <- label_match %>% filter(!is.na(label))

# impute the label for artists that are already present through other songs in the sample
na_in_no_na <- na %>% filter(artist %in% no_na$artist)
#no_na_in_na <- no_na %>% filter(artist %in% na$artist)
#no_na_in_na <- no_na_in_na[, -c(1,4)]
#no_na_in_na <- no_na_in_na[!duplicated(no_na_in_na), ]
# join with na_in_no_na
na_in_no_na <- na_in_no_na[, -5]

# in case of multiple labels per song
label_artist_count <- no_na %>% group_by(artist) %>% count(label)
label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))
label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)
label_artist_count_1 <- label_artist_count %>% group_by(artist) %>% filter(n == 1) %>% slice(n=1)
label_artist_count_n <- label_artist_count %>% group_by(artist) %>% filter(n >1)
label_artist_count <- rbind(label_artist_count_1, label_artist_count_n)
label_artist_count <- label_artist_count[, 1:2]

no_na_in_na <- label_artist_count %>% filter(artist %in% na$artist)
no_na_in_na <- no_na_in_na[!duplicated(no_na_in_na), ]
na_in_no_na <- full_join(na_in_no_na, no_na_in_na, by = c("artist"))

# join in full dataset
na_no_in_no_na <- na %>% filter(!(artist %in% no_na$artist))
label_match <- rbind(no_na, na_in_no_na, na_no_in_no_na, fill = TRUE)

# isolate NAs again
na <- label_match %>% filter(is.na(label))
na <- na[, -5]

# try to join with the artist_label_MBID dataset
na <- full_join(na, artist_label_MBID, by = "artist_MBID")
na <- na[, -5]
names(na)[3] <- "artist"

# bind datasets together again
no_na <- label_match %>% filter(!(is.na(label)))
label_match <- rbind(no_na, na)

# isolate NAs again
na <- na <- label_match %>% filter(is.na(label))
na <- na[, -5]

# join with track_label_MBID
na <- full_join(na, track_label_MBID, by = "track_MBID")
na <- na[, -5]
names(na)[1] <- "track_name"

# bind datasets together again
no_na <- label_match %>% filter(!(is.na(label)))
label_match <- rbind(no_na, na)

# check the remaining nas
na <- label_match %>% filter(is.na(label))
no_na <- label_match %>% filter(!(is.na(label)))

# impute once again
na_in_no_na <- na %>% filter(artist %in% no_na$artist)

# in case of multiple labels per song
label_artist_count <- no_na %>% group_by(artist) %>% count(label)
label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))
label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)
label_artist_count_1 <- label_artist_count %>% group_by(artist) %>% filter(n == 1) %>% slice(n=1)
label_artist_count_n <- label_artist_count %>% group_by(artist) %>% filter(n >1)
label_artist_count <- rbind(label_artist_count_1, label_artist_count_n)
label_artist_count <- label_artist_count[, 1:2]

no_na_in_na <- label_artist_count %>% filter(artist %in% na$artist)
#no_na_in_na <- no_na_in_na[, -c(1,4)]
no_na_in_na <- no_na_in_na[!duplicated(no_na_in_na), ]

# join with na_in_no_na
na_in_no_na <- na_in_no_na[, -5]
na_in_no_na <- full_join(na_in_no_na, no_na_in_na, by = c("artist"))

# join in full dataset
na_no_in_no_na <- na %>% filter(!(artist %in% no_na$artist))
label_match <- rbind(no_na, na_in_no_na, na_no_in_no_na)

label_match <- label_match[!duplicated(label_match), ]

# check final NA count
sum(is.na(label_match$label))
na <- label_match %>% filter(is.na(label))
length(unique(na$artist))
# so in total, still 20110 NAs, with 12053 unique artists missing

# final merge
users_1month <- full_join(users_1month, label_match, by = c("track_name", "artist", "track_MBID", "artist_MBID"))

# write to csv
write.csv(users_1month, "../../gen/temp/users_1month_complete.csv")
