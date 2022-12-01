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

artist_label <- users_1month[, c(2,5)] %>% distinct()
artist_info <- merge(artist_label, gender_ratio, by = "artist")

##########
#PRO RATA#
##########

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

users_PR <- users_PR[, c(1,4:5)]
users_PR_model <- merge(users_PR, artist_info, by = "artist") %>% distinct()

users_PR$model <- "PR"
users_PR <- users_PR %>% distinct()

#preparing data 
#users_PR_model$revenue <- log(users_PR_model$revenue)
#users_PR_model <- merge(users_PR_model, tlt, by = "artist")
#names(users_PR_model)[6] <- "tlt"
#users_PR_model$tlt <- log(users_PR_model$tlt)

lmpr <- lm(log(revenue) ~ label_type * ratiofem + log(tlt) + userid, users_PR_model); summary(lmpr)
plot(lmpr, 1)

# polynomial model?

# multi level linear model: 
lmpr <- lmer(log(revenue) ~ label_type * ratiofem + log(tlt) + (1 | userid), users_PR_model); summary(lmpr)

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
unlist_UC_split$names <- str_replace_all(unlist_UC_split$names, "\\.[[:digit:]][[:digit:]][[:digit:]][[:digit:]]", "")
unlist_UC_split$names <- str_replace_all(unlist_UC_split$names, "\\.[[:digit:]][[:digit:]][[:digit:]]", "")
unlist_UC_split$names <- str_replace_all(unlist_UC_split$names, "\\.[[:digit:]][[:digit:]]", "")
unlist_UC_split$names <- str_replace_all(unlist_UC_split$names, "\\.[[:digit:]]", "")
names(unlist_UC_split)[1] <- "userid"
unlist_UC_split$userid <- str_trim(unlist_UC_split$userid)
users_UC <- unlist_UC_split %>% aggregate(revenue ~ artist + userid, sum)

#users_UC_model <- merge(users_UC, artist_info, by = "artist") %>% distinct()

#preparing data 
#users_UC_model$revenue <- log(users_UC_model$revenue)
#users_UC_model <- merge(users_UC_model, tlt, by = "artist")
#names(users_UC_model)[6] <- "tlt"
#users_PR_model$tlt <- log(users_PR_model$tlt)

#lmuc <- lm(log(revenue) ~ label_type * ratiofem + log(tlt) + userid, users_UC_model); summary(lmuc)
#plot(lmuc, 1)


# further
users_UC$model <- "UC"

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

users_AGM_model <- merge(users_AGM, artist_info, by = "artist") %>% distinct()

#preparing data 
#users_UC_model$revenue <- log(users_UC_model$revenue)
users_AGM_model <- merge(users_AGM_model, tlt, by = "artist")
names(users_AGM_model)[6] <- "tlt"
#users_PR_model$tlt <- log(users_PR_model$tlt)

lmagm <- lm(log(revenue) ~ label_type * ratiofem + log(tlt) + userid, users_AGM_model); summary(lmagm)
plot(lmagm, 1)





# merging the models
artist_remuneration_final <- rbind(users_PR, users_UC, users_AGM) %>% distinct()

# merging all 
artist_label <- users_1month[, c(2,5)] %>% distinct()
artist_info <- merge(artist_label, gender_ratio, by = "artist")

artist_remuneration_final <- left_join(artist_remuneration_final, artist_info, by = "artist", allow.cartesian=TRUE) %>% distinct() 

###############################
#REGRESSION MODEL PREPARATIONS#
###############################
model_data <- artist_remuneration_final
model_data <- merge(model_data, tlt, by = "artist")
names(model_data)[7] <- "tlt"

#log of revenue & tlt
#model_data$revenue <- log(model_data$revenue)
#model_data$tlt <- log(model_data$tlt)

# models as factors
model_data$model <- as.factor(model_data$model)
model_data$model <- relevel(model_data$model, "PR")
#label_type as factor
model_data$label_type <- as.factor(model_data$label_type)

write.csv(model_data, "../../gen/temp/model_data.csv")
saveRDS(model_data, "../../gen/temp/model_data.RDS")

# regression model design
model_data <- readRDS("../../gen/temp/model_data.RDS")
#model_data <- model_data[, -1] %>% distinct()

lm1 <- lm(log(revenue) ~  model * label_type + model * ratiofem + log(tlt) + userid, model_data); summary(lm1)

plot(lm1, 1)
plot(lm1, 3)

model_data <- merge(model_data, nou, by = "artist")
names(model_data)[8] <- "nou"

lm2 <- lm(log(revenue) ~  model * label_type + model * ratiofem + log(tlt) + log(nou) + userid, model_data); summary(lm2)

plot(lm2, 1)
# basically the same as above

# trying all separate
PR_model <- model_data %>% filter(model == "PR")
PR_model <- PR_model[, -4]


lmPR <- lm(log(revenue) ~  label_type + ratiofem + log(tlt), PR_model); summary(lmPR)
plot(lmPR, 1)

# usercentric
UC_model <- model_data %>% filter(model == "UC")
UC_model <- UC_model[, -4]


lmUC <- lm(log(revenue) ~  label_type * ratiofem + log(tlt) + userid, UC_model); summary(lmUC)
plot(lmUC, 1)

# AGM
AGM_model <- model_data %>% filter(model == "AGM")
AGM_model <- AGM_model[, -4]


lmAGM <- lm(log(revenue) ~  label_type * ratiofem + log(tlt) + userid, AGM_model); summary(lmAGM)
plot(lmAGM, 1)


####### plot the first thing i had
lm3 <- lm(log(revenue) ~  model * label_type + model * ratiofem + log(tlt), model_data); summary(lm3)
plot(lm3, 1)

model_log <- model_data
model_log$revenue <- log(model_log$revenue)
model_log$tlt <- log(model_log$tlt)
lm4 <- lm(revenue ~  model * label_type + model * ratiofem + tlt, model_log); summary(lm4)
plot(lm4, 1)


model_log$label_type <- as.numeric(model_log$label_type)
lm5 <- lm(revenue ~  model * label_type + model * ratiofem + tlt, model_log); summary(lm5)
plot(lm5, 1)

lm6 <- lm(revenue ~  model * label_type + model * ratiofem + tlt + userid, model_log); summary(lm6)
plot(lm6, 1)

lm7 <- lm(revenue ~  model * label_type + model * ratiofem, model_log); summary(lm7)
plot(lm7, 1)




#### LM 6 polynomail model

set.seed(20)
lm6 <- lm(revenue ~  poly(model * label_type + model * ratiofem + tlt + userid, model_log)); summary(lm6)