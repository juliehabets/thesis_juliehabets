library(data.table)
library(dplyr)

users_1month <- fread("../../gen/temp/users_1month.csv", select = c(2,4,9,10,11,14))

# lower 
users_1month$track_name <- tolower(users_1month$track_name)
users_1month$artist  <- tolower(users_1month$artist)

# match
test_artist <- users_1month_trackartist[, 3]
test_artist <- test_artist[!duplicated(test_artist), ]

test_artist <- inner_join(test_artist, total_label, by = "artist") 
test_artist <- full_join(users_1month_trackartist, test_artist)

na <- test_artist %>% filter(is.na(label))
no_na <- test_artist %>% filter(!is.na(label))

# impute the label for artists that are already present through other songs in the sample
na_in_no_na <- na %>% filter(artist %in% no_na$artist)
no_na_in_na <- no_na %>% filter(artist %in% na$artist)
no_na_in_na <- no_na_in_na[, -c(1,4)]
no_na_in_na <- no_na_in_na[!duplicated(no_na_in_na), ]
# join with na_in_no_na
na_in_no_na <- na_in_no_na[, -5]
na_in_no_na <- full_join(na_in_no_na, no_na_in_na, by = c("artist"))
na_in_no_na <- na_in_no_na[, -5]
names(na_in_no_na)[2] <- "artist_MBID"
# join in full dataset
na_no_in_no_na <- na %>% filter(!(artist %in% no_na$artist))
test_artist <- rbind(no_na, na_in_no_na, na_no_in_no_na, fill = TRUE)


# isolate NAs again
na <- test_artist %>% filter(is.na(label))
na <- na[, -5]

# try to join with the artist_label_MBID dataset
na <- full_join(na, artist_label_MBID, by = "artist_MBID")
na <- na[, -5]
names(na)[3] <- "artist"

# bind datasets together again
no_na <- test_artist %>% filter(!(is.na(label)))
test_artist <- rbind(no_na, na)

# isolate NAs again
na <- na <- test_artist %>% filter(is.na(label))
na <- na[, -5]

# join with track_label_MBID
na <- full_join(na, track_label_MBID, by = "track_MBID")
na <- na[, -5]
names(na)[1] <- "track_name"

# bind datasets together again
no_na <- test_artist %>% filter(!(is.na(label)))
test_artist <- rbind(no_na, na)

# check the remaining nas
na <- test_artist %>% filter(is.na(label))
no_na <- test_artist %>% filter(!(is.na(label)))

# impute once again
na_in_no_na <- na %>% filter(artist %in% no_na$artist)
no_na_in_na <- no_na %>% filter(artist %in% na$artist)
no_na_in_na <- no_na_in_na[, -c(1,4)]
no_na_in_na <- no_na_in_na[!duplicated(no_na_in_na), ]

# join with na_in_no_na
na_in_no_na <- na_in_no_na[, -5]
na_in_no_na <- full_join(na_in_no_na, no_na_in_na, by = c("artist"))
na_in_no_na <- na_in_no_na[, -5]
names(na_in_no_na)[2] <- "artist_MBID"

# join in full dataset
na_no_in_no_na <- na %>% filter(!(artist %in% no_na$artist))
test_artist <- rbind(no_na, na_in_no_na, na_no_in_no_na)

# check final NA count
sum(is.na(test_artist$label))
na <- test_artist %>% filter(is.na(label))
length(unique(na$artist))
# so in total, still 20110 NAs, with 12053 unique artists missing


# final merge
final <- full_join(users_1month, test_artist, by = c("track_name", "artist", "track_MBID"))
