library(data.table)
library(dplyr)
library(musicMetadata)

# load dataset
total_label <- fread("../../gen/temp/total_label_clean.csv")

# clean
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3)] <- c("artist", "track_name", "label")

# classify 
#total_label <- total_label %>% mutate(classification = classify_labels(total_label$label), concatenate = TRUE)
labels <- unique(total_label$label)

classified_labels <- data.frame(label=labels, parent_label = classify_labels(labels, concatenate = T))

total_label <- full_join(total_label, classified_labels, by = "label")
total_label <- total_label[!duplicated(total_label), ]

# write to csv
write.csv(total_label, "../../gen/temp/label_classification.csv")
