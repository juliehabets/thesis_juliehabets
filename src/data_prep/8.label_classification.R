library(data.table)
library(dplyr)
library(musicMetadata)

# load dataset
total_label <- fread("../../gen/temp/total_label_clean.csv")

# clean
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3)] <- c("artist", "track_name", "label")

# classify 
labels <- unique(total_label$label)

classified_labels <- data.frame(label=labels, parent_label = classify_labels(labels, concatenate = T))

total_label <- full_join(total_label, classified_labels, by = "label")
total_label <- total_label[!duplicated(total_label), ]

# reduce the number of labels per song 
total_label_red <- total_label %>% group_by(artist, track_name) %>% count(label)
total_label_red <- total_label_red %>% group_by(artist, track_name) %>% mutate(max_n = max(n))

# keeping only most popular label
total_label_red <- total_label_red %>% group_by(artist, track_name) %>% filter(n == max_n)

# keeping only 1 label per artist in case when the max counts are the same
total_label_red <- total_label_red %>% group_by(artist, track_name) %>% slice(n=1)
#total_label <- total_label[, 1:3]

# write to csv
write.csv(total_label, "../../gen/temp/label_classification.csv")
write.csv(classified_labels, "../../gen/temp/classified_labels.csv")
