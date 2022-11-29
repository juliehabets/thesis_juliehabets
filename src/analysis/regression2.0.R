library(dplyr)
library(data.table)
library(DescTools)
library(tidyr)
library(magrittr)
library(ineq)
library(stringr)

# load data 
users_1month <- fread("../../gen/temp/users_1month_allmod.csv")
gender_ratio <- fread("../../gen/temp/gender_ratio_artist.csv")

# removing columns for better overview (userid, trackname, artist, gender,label_type, femratio))
users_1month <- users_1month[, c(2, 4, 6, 7, 10, 11)]
# removing columns for better overview (artist & femratio)
gender_ratio <- gender_ratio[, c(2,6)]

# Pro rata 
users_PR <- 
  users_1month %>% 
  group_by(artist) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

total_revenue = length(unique(users_1month$userid))*9.99

users_PR <- 
  users_PR %>%
  mutate(revenue = freq*total_revenue)

users_PR <- merge(users_PR, users_1month, by = "artist")

users_PR <- users_PR[, c(1,4:5)] %>% distinct() %>% mutate(model = "PR")


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
    mutate(revenue = freq * 9.99) %>% 
    ungroup()
}

# apply to every list in users_UC_split
users_UC_split <- lapply(users_split, revenue_per_user)

# turn list into data frame
unlist_UC_split <- do.call(rbind.data.frame, users_UC_split)
unlist_UC_split <- data.frame(names = row.names(unlist_UC_split), unlist_UC_split)
unlist_UC_split$names <- str_replace_all(unlist_UC_split$names, "\\.[[:digit:]]", "")
names(unlist_UC_split)[1] <- "userid"
users_UC <- unlist_UC_split %>% aggregate(revenue ~ artist + userid, sum)

users_UC <- users_UC %>% distinct() %>% mutate(model = "UC")

#####################
#ARTIST GROWTH MODEL#
#####################
users_AGM <- 
  users_1month %>% 
  group_by(artist) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

total_revenue = length(unique(users_1month$userid))*9.99

users_AGM <- 
  users_AGM %>%
  mutate(revenue= freq*total_revenue)

users_AGM <- merge(users_AGM, users_1month, by = "artist")

users_AGM <- users_AGM[, c(1,3:5)] %>% distinct()

users_AGM <- users_AGM %>% mutate(decile = ntile(-freq, 10))

# taxing decile 1 & 2
dec12 <- users_AGM %>% filter(decile <= 2)
dec12$revenue <- dec12$revenue*0.9

# giving back to decile 3-6
dec3456 <- users_AGM %>% filter(decile %in% (3:6))
rev_cut <- sum(dec12$revenue)*0.1

# checking unique artists
length(unique(dec3456$artist))
artists_dec3456 <- 5598 # CHECK THIS IF DATASET CHANGES 

# extra revenue per artist
extra <- rev_cut/artists_dec3456

dec3456$revenue <- dec3456$revenue+extra

# creating dataset decile 7-10
dec78910 <- users_AGM %>% filter(decile %in% (7:10))

# merging datasets together
users_AGM <- rbind(dec12, dec3456, dec78910)
users_AGM <- users_AGM[, -c(2,5)] %>% distinct() %>% mutate(model = "AGM")

# merging the models
users_PR <- users_PR[, -2]
artist_remuneration_final <- rbind(users_PR, users_UC, users_AGM) %>% distinct()

# merging all 
artist_info <- users_1month[, c(2,5,6)] %>% distinct()

artist_remuneration_final <- merge( artist_remuneration_final, artist_info, by = "artist", allow.cartesian=TRUE)


###############################
#REGRESSION MODEL PREPARATIONS#
###############################
model_data <- artist_remuneration_final
model_data <- merge(model_data, tlt, by = "artist")
names(model_data)[7] <- "tlt"

#log of revenue & tlt
model_data$revenue <- log(model_data$revenue)
model_data$tlt <- log(model_data$tlt)

# models as factors
model_data$model <- as.factor(model_data$model)
model_data$model <- relevel(model_data$model, "PR")

write.csv(model_data, "../../gen/temp/model_data.csv")
saveRDS(model_data, "../../gen/temp/model_data.RDS")

# regression model design
model_data <- readRDS("../../gen/temp/model_data.RDS")
model_data <- model_data[, -1]

lm1 <- lm(revenue ~  model * label_type + model * ratiofem + tlt + userid, model_data); summary(lm1)

plot(lm1, 1)
