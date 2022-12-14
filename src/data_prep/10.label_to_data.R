library(data.table)
library(dplyr)

users_1month <- fread("../../gen/temp/users_1month.csv", select = c(2, 8:12))

total_label <- fread("../../gen/temp/total_label.csv")
artist_label_MBID <- fread("../../gen/temp/finartist_label_mbid.csv")
track_label_MBID <- fread("../../gen/temp/fintrack_label_mbid.csv")

# clean the loaded files
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3,4,5)] <- c("artist_MBID", "artist", "track_MBID", "track_name", "label")

artist_label_MBID <- artist_label_MBID[-1,-c(1,4)]
names(artist_label_MBID)[c(1,2,3)] <- c("artist", "artist_MBID", "label")

track_label_MBID <- track_label_MBID[-1,-c(1,4)]
names(track_label_MBID)[c(1,2,3)] <- c("track_name", "track_MBID", "label")

# lower the artists & tracknames to facilitate matching
users_1month$track_name <- tolower(users_1month$track_name)
users_1month$artist  <- tolower(users_1month$artist)
users_1month$artist <- replace(users_1month$artist, users_1month$artist == "pussycat dolls", "the pussycat dolls")

# subset to facilitate matching
users_1month_trackartist <- users_1month[, c(2:5)]

# match
label_match <- users_1month_trackartist[, 2]
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
names(na)[2] <- "artist"

# bind datasets together again
no_na <- label_match %>% filter(!(is.na(label)))
label_match <- rbind(no_na, na)

# isolate NAs again
na <- na <- label_match %>% filter(is.na(label))
na <- na[, -5]

# join with track_label_MBID
na <- full_join(na, track_label_MBID, by = "track_MBID")
na <- na[, -5]
names(na)[4] <- "track_name"

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
# so in total, still 198448 NAs, with 11811 unique artists missing

na_check <- na %>% count(artist)
# According to the count above, artists up until 20 streams in the total sample with label NA are added as well via information on Discogs. 

na$label[na$artist == "nick catchdubs"] <- "Fool's Gold Records"
na$label[na$artist == "a. r. rahman"] <- "Sony Music India"
na$label[na$artist == "スピッツ"] <- "Universal Music Group"
na$label[na$artist == "владимир фельцман"] <- "Sony Classical Records"
na$label[na$artist == "владимир давидович ашкенази"] <- "Decca Records"
na$label[na$artist == "the cut up boys"] <- "Phonetic Recordings"
na$label[na$artist == "valve"] <- "Ipecac Recordings"
na$label[na$artist == "j. bizness"] <- "Mello Music Group"
na$label[na$artist == "the hood internet"] <- "Decon"
na$label[na$artist == "georges delerue"] <- "Sunflower Records"
na$label[na$artist == "雅-miyavi-"] <- "EMI Music Japan"
na$label[na$artist == "daniel garcia"] <- "Poprock Records"
na$label[na$artist == "andy c"] <- "RAM Records"
na$label[na$artist == "super cat"] <- "Preiser Records"
na$label[na$artist == "daniel kehlmann"] <- "Columbia Records"
na$label[na$artist == "jaleco sound team"] <- "Pony Canyon"
na$label[na$artist == "alfred brendel"] <- "Philips Classics Records"
na$label[na$artist == "margot"] <- "Full Time Hobby"
na$label[na$artist == "pow(d)er pussy"] <- "Pflichtkauf"
na$label[na$artist == "squer"] <- "Square Records"
na$label[na$artist == "youthmovies"] <- "Try Harder Records"
na$label[na$artist == "cesária évora"] <- "Lusafrica"
na$label[na$artist == "hefner"] <- "Too Pure"
na$label[na$artist == "the metal hearts"] <- "Suicide Squeeze Records"
na$label[na$artist == "冨田恵一・武藤星・阿部純"] <- "King Records"
na$label[na$artist == "budapest festival orchestra"] <- "Channel Classics Records"
na$label[na$artist == "víctor jara"] <- "Warner Music Group"
na$label[na$artist == "kermes, simone; marcon, andrea; venice baroque orchestra"] <- "Sony Classical Records"
na$label[na$artist == "arthur rubinstein philharmonic orchestra"] <- "Danacord"
na$label[na$artist == "dzubrivo "] <- "Menart Records"
na$label[na$artist == "p•a•l"] <- "Ant-Zen"

no_na <- label_match %>% filter(!(is.na(label)))

label_match <- rbind(na, no_na)

# final merge
users_1month <- full_join(users_1month, label_match, by = c("track_name", "artist", "track_MBID", "artist_MBID"))

# write to csv
write.csv(users_1month, "../../gen/temp/users_1month_complete.csv")