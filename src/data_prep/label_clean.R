library(data.table)
library(dplyr)
library(stringr)

# load dataset
total_label <- fread("../../gen/temp/total_label.csv")

# clean
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3)] <- c("artist", "track_name", "label")

# remove digits 
total_label$label <- str_replace_all(total_label$label, "[:digit:]", "")

# remove parentheses () that were around the digits 
total_label$label <- str_replace_all(total_label$label, "\\(\\)", "")

# remove leading and trailing spaces 
total_label$label <- str_trim(total_label$label)

# write to csv
write.csv(total_label, "../../gen/temp/total_label_clean.csv")
