library(data.table)
library(dplyr)
library(stringr)

# load dataset
total_label <- fread("../../gen/temp/users_1month_complete.csv")

# clean
total_label <- total_label[-1, -1]
names(total_label)[c(1,2,3,4,5,6,7)] <- c("userid", "artist_MBID", "artist", "track_MBID", "track_name","gender", "label")

# remove digits 
total_label$label <- str_replace_all(total_label$label, "[:digit:]", "")

# remove parentheses () that were around the digits 
total_label$label <- str_replace_all(total_label$label, "\\(\\)", "")

# add spaces around | 
total_label$label <- str_replace_all(total_label$label, "\\|", " \\| ")

# remove leading and trailing spaces 
total_label$label <- str_trim(total_label$label)

# remove double spaces
total_label$label <- str_squish(total_label$label)

# write to csv
write.csv(total_label, "../../gen/temp/users_1month_complete_clean.csv")
