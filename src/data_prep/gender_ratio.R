library(dplyr)
library(data.table)
library(tidyr)

# import dataset 
users_1month <- fread("../../gen/temp/users_1month_classified.csv")

# create separate dataset with only artist, userid & gender
gender <- users_1month[, -c(1, 3:4, 6, 8:10)]

# split dataset per user
gender_split <- split(gender, gender$userid)

# function to assign user gender to each artist
gender_artist_per_user <- function(l){
  df <- as.data.frame(l)
  distinct(df, df[2], .keep_all = TRUE)
}

# apply function to all users 
gender_artist <- lapply(gender_split, gender_artist_per_user)

# turn list into data frame
gender_artist <- do.call(rbind.data.frame, gender_artist)
row.names(gender_artist) <- NULL
gender_artist <- gender_artist[-c(1)]

# count times of gender per artist & pivot wider
gender_artist <- 
  gender_artist %>% 
  group_by(artist, gender) %>%
  summarise(total_count=n(), .groups = 'drop')

gender_artist$gender[(gender_artist$gender) == ''] <- 'none'

gender_artist <- gender_artist %>% 
  group_by(artist) %>% pivot_wider(names_from = gender,values_from = total_count)

# recoding NA values to 0 
gender_artist[is.na(gender_artist)] <- 0

# coding the gender ratios 
gender_artist <-
  gender_artist %>% 
  mutate(ratiofem = f/(f+m+none),
         ratiomale = m/(f+m+none))

# write to csv
write.csv(gender_artist, file = "../../gen/temp/gender_ratio_artist.csv")
