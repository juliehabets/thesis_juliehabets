library(dplyr)
library(data.table)
library(DescTools)
library(tidyr)
library(magrittr)
library(ineq)

# load data 
users_1month <- fread("../../gen/temp/users_1month.csv")

# removing columns for better overview
users_1month <- users_1month[c(2, 8:12)]

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

# gini pro rata 
gini_PR <- Gini(users_PR$revenue)
# lorenz curve pro rata
plot(Lc(users_PR$revenue), col = "blue", lwd = 2, main = "Lorenz Curve Pro Rata Model", 
     xlab = "cumulative % of artists", ylab = "cumulative % of income")
text(x = 0.12, y = 0.9, "Gini = 0.89", cex = 1.1)

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
row.names(unlist_UC_split) <- NULL

# aggregating the data to artist level  
users_UC <- unlist_UC_split %>% aggregate(revenue ~ artist, sum)

# gini user-centric
gini_UC <- Gini(users_UC$revenue)
# lorenz curve user-centric
plot(Lc(users_UC$revenue), col = "red", lwd = 2, main = "Lorenz Curve User-Centric Model", 
     xlab = "cumulative % of artists", ylab = "cumulative % of income")
text(x = 0.12, y = 0.9, "Gini = 0.91", cex = 1.1)

#####################
#ARTIST GROWTH MODEL#
#####################

users_AGM <- users_PR %>% mutate(decile = ntile(-freq, 10))

# taxing decile 1 & 2
dec12 <- users_AGM %>% filter(decile <= 2)
dec12$revenue <- dec12$revenue*0.9

# giving back to decile 3-6
dec3456 <- users_AGM %>% filter(decile %in% (3:6))
rev_cut <- sum(dec12$revenue)*0.1

# checking unique artists
length(unique(dec3456$artist))
artists_dec3456 <- 13676 # CHECK THIS IS DATASET CHANGES 

# extra revenue per artist
extra <- rev_cut/artists_dec3456

dec3456$revenue <- dec3456$revenue+extra

# creating dataset decile 7-10
dec78910 <- users_AGM %>% filter(decile %in% (7:10))

# merging datasets together
users_AGM <- rbind(dec12, dec3456, dec78910)

# gini AGM
gini_AGM <- Gini(users_AGM$revenue)

# lorenz curve AGM
plot(Lc(users_AGM$revenue), col = "green", lwd = 2, main = "Lorenz Curve AGM model", 
     xlab = "cumulative % of artists", ylab = "cumulative % of income")
text(x = 0.12, y = 0.9, "Gini = 0.76", cex = 1.1)

# overlaying the lorenz curves
plot(Lc(users_PR$revenue), col = 'blue')
lines(Lc(users_UC$revenue), col = 'red')
lines (Lc(users_AGM$revenue), col = 'green')
legend("topleft", c("Pro Rata", "User-Centric", "AGM"), fill = c("blue", "red", "green"))

# AGM model 
#AGM_model <- function(artist, decile, revenue){
#  if (decile <= 2){
#    revenue * 0.9
#  } else if (decile %in% (3:6)){
#    revenue + extra
 # } else {
  #  revenue + 0
#  }
#}

# applying
#AGM_test <- mapply(AGM_model(artist, decile, revenue), artist_decile+revenue, dec12$artist, dec12$decile, dec12$revenue)
