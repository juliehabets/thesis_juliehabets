library(data.table)
library(dplyr)
library(stringr)

# regression model design
model_data <- readRDS("../../gen/temp/model_data.RDS")
#model_data <- model_data[, -1] %>% distinct()
# remove leading and trailing spaces 
model_data$userid <- str_trim(model_data$userid)

# trytry
# Multi level linear model 
lm1 <- lmer(revenue ~  model * label_type + model * ratiofem + tlt + (1 | userid), model_data); summary(lm1)

plot(lm1)

plot(lm1, 1)
model_data$userid <- as.factor(model_data$userid)
model_data$artist <- as.factor(model_data$artist)
model_data$label_type <- as.factor(model_data$label_type)

lm2 <- lm(revenue ~ model*label_type + model*ratiofem + tlt + userid, model_data); summary(lm2)


##############
#USER-CENTRIC#
##############
users_split <- split(users_1month, users_1month$userid)

# revenue per user function
revenue_per_user <- function(l){
  df <- as.data.frame(l)
  
  df %>% group_by(df[3]) %>% 
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
unlist_UC_split2 <- unlist_UC_split
unlist_UC_split2$names <- str_replace_all(unlist_UC_split$names, "\\.[[:digit:]][[:digit:]][[:digit:]][[:digit:]]", "")
unlist_UC_split2$names <- str_replace_all(unlist_UC_split2$names, "\\.[[:digit:]][[:digit:]][[:digit:]]", "")
unlist_UC_split2$names <- str_replace_all(unlist_UC_split2$names, "\\.[[:digit:]][[:digit:]]", "")
unlist_UC_split2$names <- str_replace_all(unlist_UC_split2$names, "\\.[[:digit:]]", "")
names(unlist_UC_split)[1] <- "userid"
unlist_UC_split$userid <- str_trim(unlist_UC_split$userid)
users_UC <- unlist_UC_split %>% aggregate(revenue ~ artist + userid, sum)

users_UC_model <- merge(users_UC, artist_info, by = "artist") %>% distinct()

#preparing data 
#users_UC_model$revenue <- log(users_UC_model$revenue)
users_UC_model <- merge(users_UC_model, tlt, by = "artist")
names(users_UC_model)[6] <- "tlt"
#users_PR_model$tlt <- log(users_PR_model$tlt)

lmuc <- lm(log(revenue) ~ label_type * ratiofem + log(tlt) + userid, users_UC_model); summary(lmuc)
plot(lmuc, 1)
