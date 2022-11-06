library(dplyr)
library(readr)

# import dataset 
users_1month <- read_csv("../../gen/temp/users_1month.csv")

# create separate dataset
gender <- users_1month[-c(2, 4:5)]

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
  summarise(total_count=n(), .groups = 'drop') %>% 
  pivot_wider(names_from = gender,values_from = total_count)

# recoding column to make mutation easier
names(gender_artist)[names(gender_artist) == 'NA'] <- 'none'

# recoding NA values to 0 
gender_artist[is.na(gender_artist)] <- 0

# coding the gender ratios 
gender_artist <-
  gender_artist %>% 
  mutate(ratiofem = f/(f+m+none),
         ratiomale = m/(f+m+none))

# write to csv
write.csv(gender_artist, file = "../../gen/temp/gender_ratio_artist.csv")