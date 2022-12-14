#release_total <- load("../../data/release_total2.RData")
# with na & no_na from 10.label_to_data.R

release_total <- release_total2[, c(3,10)]

# remove parentheses () that were around the digits 
release_total$artist_name <- str_replace_all(release_total$artist_name, "\\([:digit:]\\)", "")

# remove leading and trailing spaces 
release_total$artist_name <- str_trim(release_total$artist_name)

# remove double spaces
release_total$artist_name <- str_squish(release_total$artist_name)

names(release_total)[1:2] <- c("artist", "label")

release_total$artist <- str_to_lower(release_total$artist)

# 1 label per artist
label_artist_count <- release_total %>% group_by(artist) %>% count(label)
label_artist_count <- label_artist_count %>% group_by(artist) %>% mutate(max_n = max(n))
label_artist_count <- label_artist_count %>% group_by(artist) %>% filter(n == max_n)
label_artist_count_1 <- label_artist_count %>% group_by(artist) %>% filter(n == 1) %>% slice(n=1)
label_artist_count_n <- label_artist_count %>% group_by(artist) %>% filter(n >1)
label_artist_count <- rbind(label_artist_count_1, label_artist_count_n)
label_artist_count <- label_artist_count[, 1:2]

# na file
na <- na %>% distinct()
na_try <- na[, -5]
na_try <- inner_join(na_try, label_artist_count, by = "artist")

na_1 <- na %>% filter(!(artist %in% na_try$artist))
na_1 <- rbind(na_1, na_try)

label_match <- rbind(na_1, no_na)

# final merge
users_1month <- full_join(users_1month, label_match, by = c("track_name", "artist", "track_MBID", "artist_MBID"))