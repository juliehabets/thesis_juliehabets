library(data.table)
library(dplyr)
library(musicMetadata)
library(tidyr)
library(stringr)

# load dataset
total_label <- fread("../../gen/temp/users_1month_complete_clean.csv")

# clean
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3,4,5,6,7)] <- c("userid", "track_name", "artist_MBID", "artist", "track_MBID", "gender", "label")
#total_label <- total_label[, c(2,1,3)]

# subset the non-NA labels so that the NA labels will not be counted as independent labels
labels <- total_label %>% filter(!(is.na(label)))
labels <- unique(labels$label)

# classify labels
classified_labels <- data.frame(label=labels, parent_label = classify_labels(labels, concatenate = T))

total_label <- full_join(total_label, classified_labels, by = "label")

# rewrite the labels into their classification
total_label$parent_label[total_label$parent_label == ''] <- "independent"
total_label$parent_label[(total_label$parent_label == 'warner')] <- "major"
total_label$parent_label[(total_label$parent_label == 'sony')] <- "major"
total_label$parent_label[(total_label$parent_label == 'universal')] <- "major"
total_label$parent_label[(total_label$parent_label == 'universal,sony')] <- "major"
total_label$parent_label[(total_label$parent_label == 'warner,sony')] <- "major"
total_label$parent_label[(total_label$parent_label == 'warner,universal')] <- "major"

# recoding the label type classification
total_label$label_type [total_label$parent_label == "major"] <- 1
total_label$label_type [total_label$parent_label == "independent"] <- 0

# per artist
total_label_artist <- total_label[, c(4,7,8,9)]
total_label_artist <- distinct(total_label_artist)

# write to csv
write.csv(total_label, "../../gen/temp/label_classification.csv")
write.csv(classified_labels, "../../gen/temp/classified_labels.csv")
write.csv(total_label_artist, "../../gen/temp/label_classification_per_artist.csv")

# after checking, these were some record labels put down correctly so do it again.

# load dataset
total_label_corrected <- fread("../../gen/temp/users_1month_complete_clean.csv")

# clean
total_label_corrected <- total_label_corrected[-1, -1]
names(total_label_corrected)[c(1,2,3,4,5,6,7)] <- c("userid", "track_name", "artist_MBID", "artist", "track_MBID", "gender", "label")

# corrections
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Jive", "Zomba Records")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "jive", "Zomba Records")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Apple Records1\\b", "Apple Records | EMI Electrola")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Purple Records\\b", "Purple Records | Universal Music Group")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Taste Media", "Taste Media | Warner Music")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Epic\\b", "Epic | Sony BMG")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Dare Records", "Dare Records | Warner Bros. Records")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Lava$", "Lava | Universal Music Group")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Labels | Virgin", "Virgin")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Global Music", "Global Music | Universal Music Group")
total_label_corrected$label <- str_replace_all(total_label_corrected$label, "Hollywood Records", "Hollywood Records | Universal Music Group")

# subset the non-NA labels so that the NA labels will not be counted as independent labels
labels <- total_label_corrected %>% filter(!(is.na(label)))
labels <- unique(labels$label)

# classify labels
classified_labels <- data.frame(label=labels, parent_label = classify_labels(labels, concatenate = T))

total_label_corrected <- full_join(total_label_corrected, classified_labels, by = "label")

# rewrite the labels into their classification
total_label_corrected$parent_label[total_label_corrected$parent_label == ''] <- "independent"
total_label_corrected$parent_label[(total_label_corrected$parent_label == 'warner')] <- "major"
total_label_corrected$parent_label[(total_label_corrected$parent_label == 'sony')] <- "major"
total_label_corrected$parent_label[(total_label_corrected$parent_label == 'universal')] <- "major"
total_label_corrected$parent_label[(total_label_corrected$parent_label == 'universal,sony')] <- "major"
total_label_corrected$parent_label[(total_label_corrected$parent_label == 'warner,sony')] <- "major"
total_label_corrected$parent_label[(total_label_corrected$parent_label == 'warner,universal')] <- "major"

# recoding the label type classification
total_label_corrected$label_type [total_label_corrected$parent_label == "major"] <- 1
total_label_corrected$label_type [total_label_corrected$parent_label == "independent"] <- 0

# most occuring label type per artist
label_per_artist <- total_label_corrected %>% group_by(artist) %>% count(label_type)

label_per_artist <- label_per_artist %>% group_by(artist) %>% mutate(max_n = max(n))
label_per_artist <- label_per_artist %>% group_by(artist) %>% filter(n == max_n)
label_per_artist_1 <- label_per_artist %>% group_by(artist) %>% filter(n == 1) %>% slice(n=1)
label_per_artist_n <- label_per_artist %>% group_by(artist) %>% filter(n >1) %>% arrange(desc(n)) %>% slice(n=1)
label_per_artist <- rbind(label_per_artist_1, label_per_artist_n)
label_per_artist <- label_per_artist[, -c(3,4)]

total_label_corrected <- total_label_corrected[, -9]
total_label_corrected <- full_join(total_label_corrected, label_per_artist, by = "artist")

# write to csv
write.csv(total_label_corrected, "../../gen/temp/users_1month_classified.csv")
