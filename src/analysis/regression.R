library(data.table)
library(dplyr)
library(DescTools)
library(broom)
library(ggplot2)

# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final.csv", select = c(2:7))
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))

# factor vars
remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

# Gini coefficients
gini_PR <- Gini(remuneration$revenue_PR, na.rm = FALSE)
gini_UC <- Gini(remuneration$revenue_UC, na.rm = FALSE)
gini_AGM <- Gini(remuneration$revenue_AGM, na.rm = FALSE)

# DUURT LANG Gini_PR <- Gini(remuneration$revenue_PR, conf.level = 0.95)

lr1 <- lm(revenue ~ model, remuneration_factors); summary(lr1)
lr2 <- lm(revenue ~ model * label_type, remuneration_factors); summary(lr2)
lr3 <- lm(revenue ~ model * ratiofem, remuneration_factors); summary(lr3)
lr4 <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors); summary(lr4)

lr4_res1 <- augment(lr4)
ggplot(lr4_res1, aes(.resid)) + geom_histogram(aes(y = ..density..), binwidth = 5) + stat_function(fun = dnorm, args = list(mean = mean(lr4_res1$.resid), sd = sd(lr4_res1$.resid)), color="red", size=2)
print(lr4_res1)

# log
remuneration_factors$revenue_log <- log(remuneration_factors$revenue)

lr4log <- lm(revenue_log ~ model * label_type + model * ratiofem, remuneration_factors); summary(lr4log)

# Distribution of residuals of revenue 
plot(remuneration_factors$revenue, dnorm(remuneration_factors$revenue, mean(remuneration_factors$revenue), sd(remuneration_factors$revenue)))
plot(remuneration_factors$revenue_log, dnorm(remuneration_factors$revenue_log, mean(remuneration_factors$revenue_log), sd(remuneration_factors$revenue_log)))

# so log, which is logical as revenue cannot be negative and thus, log(x) is a lognormal distribution 

# plotting others 
plot(remuneration_factors$ratiofem, dnorm(remuneration_factors$ratiofem, mean(remuneration_factors$ratiofem), sd(remuneration_factors$ratiofem)))



########
#NETJES#
########

# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final.csv", select = c(2:7))
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))

# factor vars
remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

# Gini coefficients
gini_PR <- Gini(remuneration$revenue_PR, na.rm = FALSE)
gini_UC <- Gini(remuneration$revenue_UC, na.rm = FALSE)
gini_AGM <- Gini(remuneration$revenue_AGM, na.rm = FALSE)

# estimating the model
mlm <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors); summary(mlm)
mlm_res <- augment(mlm)

##########################
#Estimating the log model#
##########################

# recoding the revenue variable 
remuneration_factors$revenue <- log(remuneration_factors$revenue)


# estimating model
mlm_log <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors); summary(mlm_log)
mlm_log_res <- augment(mlm_log)

#######################
#Checking independence#
#######################

# with log model
ggplot(mlm_log_res, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept=0,linetype="dashed", color = "red")

plot(mlm_log, 1)
autoplot(mlm_log) # with ggfortify library 

# with not log model 
ggplot(mlm_res, aes(x = .fitted, y = .resid)) + geom_point()  + geom_hline(yintercept=0,linetype="dashed", color = "red")

plot(mlm, 1)
autoplot(mlm) # with ggfortify library  


##################
#Homoscedasticity#
##################

# with log
plot(mlm_log, 3) # --> ok

# without log
plot(mlm, 3) # --> violated

# f the residuals are spread randomly and the see a horizontal line with equally (randomly) spread points, 
# then the assumption is fulfilled.

###########
#Normality#
###########

# with log
plot(mlm_log, 2) #--> can assume normality

# plotting the residuals

ggplot(mlm_log_res, aes(.resid)) + geom_histogram(aes(y = ..density..), binwidth = 5) + stat_function(fun = dnorm, args = list(mean = mean(mlm_log_res$.resid), sd = sd(mlm_log_res$.resid)), color="red", size=2)

# Distribution of residuals of revenue (same as above but less pretty)
plot(remuneration_factors$revenue_log, dnorm(remuneration_factors$revenue_log, mean(remuneration_factors$revenue_log), sd(remuneration_factors$revenue_log)))

# so log, which is logical as revenue cannot be negative and thus, log(x) is a lognormal distribution 

#without log
plot(mlm, 2) # --> we CANNOT ASSUME NORMALITY

# plotting the residuals
ggplot(mlm_res, aes(.resid)) + geom_histogram(aes(y = ..density..), binwidth = 5) + stat_function(fun = dnorm, args = list(mean = mean(mlm_res$.resid), sd = sd(mlm_res$.resid)), color="red", size=2)

# Distribution of residuals of revenue (saeme as above but less pretty)
plot(remuneration_factors$revenue, dnorm(remuneration_factors$revenue, mean(remuneration_factors$revenue), sd(remuneration_factors$revenue)))

####################
#Checking linearity#
####################

# with log model
ggplot(mlm_log_res, aes(x = .fitted, y = .resid)) + geom_point() + geom_smooth()

plot(mlm_log$fitted.values, mlm_log$model$BMI)

# Ideally we want a horizontal line around zero

#####################
#CHECK ALL WITH PLOT#
#####################

plot(mlm_res)


##############
#COLLINEARITY#
##############

car::vif(mlm_log, type = "predictor")







###########################
#COVARIATE TRYINGS????????#
###########################

cov <- merge(remuneration_factors, tlt, by = "artist")
names(cov)[7] <- "tlt"
cov  <- merge(cov, nou, by = "artist")
names(cov)[8] <- "nou"

mlm_cov_log <- lm(revenue_log ~ model * label_type + model * ratiofem + tlt + nou, cov); summary(mlm_cov_log)
# R SQUARED IS AT LEAST A BIT BETTER? 
mlm_cov_log_res <- augment(mlm_cov_log)

ggplot(mlm_cov_log_res, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept=0,linetype="dashed", color = "red")

plot(mlm_cov_log_res, 1)

#### TRYING 1 LM
lm1 <- lm(revenue_log ~ model, remuneration_factors); summary(lm1)
lm1_res <- augment(lm1)
plot(lm1_res, 1)

# cov log valeus
cov$tlt_log <- log(cov$tlt)
cov$nou_log <- log(cov$nou)
mlm_2_log <- lm(revenue_log ~ model*label_type*ratiofem + tlt_log + nou_log, cov); summary(mlm_2_log)


# cov log
cov_log <- cov[, -c(4,7,8)]
mlm_cov_log2 <- lm(revenue_log ~ model * label_type + model * ratiofem + tlt_log + nou_log, cov_log); summary(mlm_cov_log2)

mlm_cov_log_res2 <- augment(mlm_cov_log2)

ggplot(mlm_cov_log_res2, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept=0,linetype="dashed", color = "red")

plot(mlm_cov_log2, 1)


###### CHECKING WITH FEMRATIO AS A FACTOR
cov_log <- cov_log %>% mutate(ratiofem = )










######## CHECK CHECK

lm1 <- lm(revenue_log ~  model * label_type + model * ratiofem + tlt_log, cov_log); summary(lm1)
lm2 <- lm(revenue_log ~ model * label_type + model * ratiofem + tlt, cov); summary(lm2) #rsquared is way lower
lm3 <- lm(revenue_log ~ model * label_type + model * ratiofem + nou, cov); summary(lm3) #r2 ^ lm2, lower lm1
lm4 <- lm(revenue_log ~ model * label_type + model * ratiofem + nou_log, cov_log); summary(lm4)#r2 ^ lm2, lm3, lower lm1
lm5 <- lm(revenue_log ~ model * label_type + model * ratiofem + tlt_log + nou_log, cov_log); summary(lm5) # only a teeny bit R2 added
lm6 <- lm(revenue_log ~ model * label_type + model * ratiofem + tlt_log + nou, cov); summary(lm6)# only a teeny bit R2 added# only a teeny bit R2 added
lm7 <- lm(revenue ~ model * label_type + model * ratiofem + tlt_log, cov); summary(lm7) # bad
lm8 <- lm(revenue~ model * label_type + model * ratiofem, remuneration_factors); summary(lm8) #original model
lm1_res <- augment(lm1)

plot(lm1, 1)
plot(lm8, 1)

# so when adding the covariate, there is a decreasing residual trend in scatterplot
# the original model has a pattern like: IOIOI IOI IOI

# what happens when we do only 2 types of remuneration model? 

cov_log_2mods <- cov_log %>% filter(model %in% c("PR", "AGM"))

lm9 <- lm(revenue_log ~  model * label_type + model * ratiofem + tlt_log, cov_log_2mods); summary(lm9)
plot(lm9, 1)
# this is very weird a plot, two stripes with upwards trend 

# checking when not having three per artist 
lm10 <- lm(revenue_PR ~ revenue_UC * label_type + revenue_UC * ratiofem , remuneration); summary(lm10)
plot(lm10, 1)


# create a sample with a random division of artists 
artists_df <- remuneration[, 1] %>% distinct()
artists <- as.vector(artists_df$artist)
34420/3
set.seed(5)
sampleartistsPR <-sample(artists, 11474, replace = FALSE)  
sampleartistsPR  <- data.frame(sampleartistsPR) %>% mutate(sample = "PR")
names(sampleartistsPR)[1] <- "artist"

artists <- anti_join(artists_df, sampleartistsPR, by = "artist")
artists_vector <- as.vector(artists$artist)

set.seed(5)
sampleartistsUC <-sample(artists_vector, 11473, replace = FALSE)  
sampleartistsUC  <- data.frame(sampleartistsUC) %>% mutate(sample = "UC")
names(sampleartistsUC)[1] <- "artist"

total_sample <- rbind(sampleartistsPR, sampleartistsUC)

sampleartistsAGM <- anti_join(artists_df, total_sample, by = "artist") %>% mutate(sample = "AGM")

total_sample <- rbind(total_sample, sampleartistsAGM)

# Pro rata 
users_1month <- fread("../../gen/temp/users_1month_allmod.csv")
users_1month <- users_1month[, c(2, 3, 5, 7, 10, 11)]
users_1month <- users_1month %>% filter(!(is.na(artist)))

users_1m_PR <- users_1month %>% filter(artist %in% sampleartistsPR$artist)
  
users_PR <- 
  users_1m_PR %>% 
  group_by(artist) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

total_revenue = length(unique(users_1m_PR$userid))*9.99

users_PR <- 
  users_PR %>%
  mutate(revenue = freq*total_revenue)

users_PR <- merge(users_PR, users_1m_PR, by = "artist")

users_PR <- users_PR[, c(1,3,4)] %>% distinct() %>% mutate(model = "PR")

gini_PR <- Gini(users_PR$revenue)


##############
#USER-CENTRIC#
##############
users_1m_UC <- users_1month %>% filter(artist %in% sampleartistsUC$artist)
users_split <- split(users_1m_UC, users_1m_UC$userid)

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
row.names(unlist_UC_split) <- NULL

# aggregating the data to artist level  
users_UC <- unlist_UC_split %>% aggregate(revenue ~ artist, sum)
users_UC <- merge(users_UC, users_1m_UC, by = "artist")
users_UC <- users_UC[, -c(3:7)] %>% distinct() %>% mutate(model = "UC")

# gini user-centric
gini_UC <- Gini(users_UC$revenue)

#####################
#ARTIST GROWTH MODEL#
#####################
users_1m_AGM <- users_1month %>% filter(artist %in% sampleartistsAGM$artist)

users_AGM <- 
  users_1m_AGM %>% 
  group_by(artist) %>%
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

total_revenue = length(unique(users_1m_AGM$userid))*9.99

users_AGM <- 
  users_AGM %>%
  mutate(revenue= freq*total_revenue)

users_AGM <- merge(users_AGM, users_1m_AGM, by = "artist")

users_AGM <- users_AGM[, c(1,3,4)] %>% distinct()

users_AGM <- users_AGM %>% mutate(decile = ntile(-freq, 10))

# taxing decile 1 & 2
dec12 <- users_AGM %>% filter(decile <= 2)
dec12$revenue <- dec12$revenue*0.9

# giving back to decile 3-6
dec3456 <- users_AGM %>% filter(decile %in% (3:6))
rev_cut <- sum(dec12$revenue)*0.1

# checking unique artists
length(unique(dec3456$artist))
artists_dec3456 <- 4584 # CHECK THIS IF DATASET CHANGES 

# extra revenue per artist
extra <- rev_cut/artists_dec3456

dec3456$revenue <- dec3456$revenue+extra

# creating dataset decile 7-10
dec78910 <- users_AGM %>% filter(decile %in% (7:10))

# merging datasets together
users_AGM <- rbind(dec12, dec3456, dec78910)
users_AGM <- users_AGM[, -c(2,4)] %>% distinct() %>% mutate(model = "AGM")

# gini AGM
gini_AGM <- Gini(users_AGM$revenue)

# merging the models
users_PR <- users_PR[, -2]
artist_remuneration_final <- rbind(users_PR, users_UC, users_AGM)

# merging all 
gender_ratio <- fread("../../gen/temp/gender_ratio_artist.csv", select = c(2,6))
artist_info <- users_1month[, c(3, 5,6)] %>% distinct()

artist_remuneration_final <- merge(artist_info, artist_remuneration_final, by = "artist")

# MODEL ESTIMATION
log <- artist_remuneration_final
log$revenue <- log(log$revenue)
log$model <- as.factor(log$model)
log$model <- relevel(log$model, "PR")

log <- merge(log, tlt, by = "artist")
names(log)[6] <- "tlt"
log$tlt <- log(log$tlt)
log  <- merge(log, nou, by = "artist")
names(log)[7] <- "nou"

lm10 <- lm(revenue ~  model * label_type + model * ratiofem + tlt, log); summary(lm10)
plot(lm10, 1)

# THIS ALSO HAS THE DOWNWARDS > SHAPE