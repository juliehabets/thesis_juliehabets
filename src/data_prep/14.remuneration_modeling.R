library(dplyr)
library(data.table)
library(DescTools)
library(tidyr)
library(magrittr)
library(ineq)

# load data 
users_1month <- fread("../../gen/temp/users_1month_allmod.csv")
gender_ratio <- fread("../../gen/temp/gender_ratio_artist.csv")

# removing columns for better overview (userid, trackname, artist, gender,label_type, femratio))
users_1month <- users_1month[, c(2, 4, 6, 7, 10, 11)] %>% filter(!(is.na(label_type)))

# removing columns for better overview (artist & femratio)
gender_ratio <- gender_ratio[, c(2,6)]


##########
#PRO RATA#
##########

users_PR <- 
  users_1month %>% 
  group_by(artist) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

total_revenue = length(unique(users_1month$userid))*10*0.7

users_PR <- 
  users_PR %>%
  mutate(revenue_PR = freq*total_revenue)

users_PR <- merge(users_PR, users_1month, by = "artist")

users_PR <- users_PR[, c(1,3,4)] %>% distinct()

##############
#USER-CENTRIC#
##############
users_split <- split(users_1month, users_1month$userid)

# revenue per user function
revenue_per_user <- function(l){
  df <- as.data.frame(l)
  
  df %>% group_by(df[2]) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n/sum(n)) %>% 
    mutate(revenue_UC = freq * 10 * 0.7) %>% 
    ungroup()
}

# apply to every list in users_UC_split
users_UC_split <- lapply(users_split, revenue_per_user)

# turn list into data frame
unlist_UC_split <- do.call(rbind.data.frame, users_UC_split)
row.names(unlist_UC_split) <- NULL

# aggregating the data to artist level  
users_UC <- unlist_UC_split %>% aggregate(revenue_UC ~ artist, sum)
users_UC <- merge(users_UC, users_1month, by = "artist")
users_UC <- users_UC[, -c(3:7)] %>% distinct()


#####################
#ARTIST GROWTH MODEL#
#####################

users_AGM <- users_PR %>% mutate(decile = ntile(-freq, 10))
names(users_AGM)[3] <- "revenue_AGM"

# taxing decile 1 & 2
dec12 <- users_AGM %>% filter(decile <= 2)
dec12$revenue_AGM <- dec12$revenue_AGM*0.9

# giving back to decile 3-6
dec3456 <- users_AGM %>% filter(decile %in% (3:6))
rev_cut <- sum(dec12$revenue_AGM)*0.1

# checking unique artists
length(unique(dec3456$artist))
artists_dec3456 <- 9478 # CHECK THIS IF DATASET CHANGES 

# extra revenue per artist
extra <- rev_cut/artists_dec3456

dec3456$revenue_AGM <- dec3456$revenue_AGM+extra

# creating dataset decile 7-10
dec78910 <- users_AGM %>% filter(decile %in% (7:10))

# merging datasets together
users_AGM <- rbind(dec12, dec3456, dec78910)
users_AGM <- users_AGM[, -c(2,4)] %>% distinct()

################
#MERGE TOGETHER#
################

# merging all the models together
users_PR <- users_PR[, -2]
artist_remuneration_final <- merge((merge(users_PR, users_UC, by = "artist")), users_AGM, by = "artist")
artist_info <- users_1month[, c(2,5)] %>% distinct()
artist_info <- merge(artist_info, gender_ratio, by = "artist")

artist_remuneration_final <- merge(artist_info, artist_remuneration_final, by = "artist")

# merging with factors
users_PR$model <- as.factor("PR")
users_UC$model <- as.factor("UC")
users_AGM$model <- as.factor("AGM")
names(users_PR)[2] <- "revenue"
names(users_UC)[2] <- "revenue"
names(users_AGM)[2] <- "revenue"

artist_remuneration_factors <- rbind(users_PR, users_UC, users_AGM)
artist_remuneration_factors <- merge(artist_info, artist_remuneration_factors, by = "artist")

# write to csv
write.csv(artist_remuneration_final, "../../gen/temp/artist_remuneration_final_exclna.csv")
write.csv(artist_remuneration_factors, "../../gen/temp/artist_remuneration_factors_exclna.csv")

