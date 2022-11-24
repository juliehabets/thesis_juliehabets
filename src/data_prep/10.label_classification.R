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

# reduce the number of labels per song 
#total_label_red <- total_label %>% group_by(artist) %>% count(label)
#total_label_red <- total_label_red %>% group_by(artist) %>% mutate(max_n = max(n))

# keeping only most popular label
#total_label_red <- total_label_red %>% group_by(artist) %>% filter(n == max_n)

# keeping only 1 label per artist in case when the max counts are the same
#total_label_red <- total_label_red %>% group_by(artist, track_name) %>% slice(n=1)
#total_label <- total_label[, 1:3]

# write to csv
write.csv(total_label, "../../gen/temp/label_classification.csv")
write.csv(classified_labels, "../../gen/temp/classified_labels.csv")
#write.csv(total_label_red, "../../gen/temp/total_label_red.csv")

# try fixing
try <- total_label %>% filter(artist == "lady gaga")
#try$track_name <-str_replace(try$track_name, " \\s*\\([^\\)]+\\)", "")
#try$track_name <-str_replace(try$track_name, " \\s*\\([^\\)]+\\)", "")
try <- try[!duplicated(try), ]
try <- try %>% count(parent_label)
try$parent_label[try$parent_label == ''] <- "indie"
try$parent_label[try$parent_label == 'universal'] <- "major"
try <- try %>% pivot_wider(names_from = "parent_label", values_from = "n")
#try <- try %>% mutate(across(everything(), .fns = ~replace_na(.,0))) 
#try <- try %>% mutate(total_major = sum(`warner,sony` , `warner,universal`, `universal,sony`, warner, sony, universal))
#try <- try[, -c(3:8)]
try <- try %>% mutate(ratio = major/(indie+major))
#try_indie <- try %>% filter(ratio < 0.5)


# try total label
total_try <- total_label
total_try$parent_label[total_try$parent_label == ''] <- "indie"
total_try$parent_label[!(total_try$parent_label == '')] <- "major"
total_try <- total_try %>% group_by(track_name) %>% count(parent_label)
total_try <- total_try %>% pivot_wider(names_from = "parent_label", values_from = "n")
try <- try %>% mutate(across(everything(), .fns = ~replace_na(.,0))) 
try <- try %>% mutate(total_major = sum(`warner,sony` , `warner,universal`, `universal,sony`, warner, sony, universal))