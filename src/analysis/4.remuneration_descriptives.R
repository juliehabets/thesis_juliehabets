library(data.table)
library(dplyr)
library(DescTools)
library(ggplot2)

# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final_exclna.csv", select = c(2:7))
remuneration_spread <- fread("../../gen/temp/artist_remuneration_factors_exclna.csv", select = c(2:6))
remuneration_spread$model <- as.factor(remuneration_spread$model)
remuneration_spread$model <- relevel(remuneration_spread$model, c("PR"))

# transforming revenue to log 
remuneration$revenue_PR_log <- log(remuneration$revenue_PR)
remuneration$revenue_UC_log <- log(remuneration$revenue_UC)
remuneration$revenue_AGM_log <- log(remuneration$revenue_AGM)

remuneration_spread$revenue_log <- log(remuneration_spread$revenue)

#######################################
#MOST AND LEAST PAID ARTISTS PER MODEL#
#######################################
remuneration_PR <- remuneration[, c(1:4)]
most_PR <- remuneration_PR[order(remuneration_PR$revenue_PR, decreasing = TRUE), ][1:10]
least_PR <- remuneration_PR[order(remuneration_PR$revenue_PR, decreasing = FALSE), ][1:10]

remuneration_UC <- remuneration[, c(1:3,5)]
most_UC <- remuneration_UC[order(remuneration_UC$revenue_UC, decreasing = TRUE), ][1:10]
least_UC <- remuneration_UC[order(remuneration_UC$revenue_UC, decreasing = FALSE), ][1:10]

remuneration_AGM <- remuneration[, c(1:3,6)]
most_AGM <- remuneration_AGM[order(remuneration_AGM$revenue_AGM, decreasing = TRUE), ][1:10]
least_AGM <- remuneration_AGM[order(remuneration_AGM$revenue_AGM, decreasing = FALSE), ][1:10]

#################################
#PLOTTING THE REVENUE ALLOCATION#
#################################

ggplot(remuneration_spread,
       aes(x = model, y = revenue_log)) +
  geom_boxplot(fill = "#bed6ff") +
  theme_light() + 
  labs(x = "Remuneration models", y = "log(Revenue)") + 
  theme(text = element_text(size = 12, family = "serif"))
ggsave("../../gen/output/boxplot_remmodels_revenue.png")

# as you can see, with AGM & PR there is an outlier. 
# remove this outlier
remuneration_spread_no_outlier <- remuneration_spread %>% filter(!(artist == "ramirez"))

# plot boxplot again
ggplot(remuneration_spread_no_outlier,
       aes(x = model, y = revenue_log)) +
  geom_boxplot(fill = "#bed6ff") +
  theme_light() + 
  labs(x = "Remuneration models", y = "log(Revenue)") + 
  theme(text = element_text(size = 12, family = "serif"))
ggsave("../../gen/output/boxplot_remmodels_revenue_nooutliers.png")

# violion plot
ggplot(remuneration_spread,  aes(x = model, y = revenue_log)) +
  geom_violin(scale = "count", fill = "#bed6ff") +
  theme_light() + 
  labs(x = "Remuneration models", y = "log(Revenue)") + 
  theme(text = element_text(size = 12, family = "serif"))
ggsave("../../gen/output/violinplot_remmodels_revenue.png")


########################
#DESCRIPTIVE STATISTICS#
########################

# pro rata
mean_PR <- mean(remuneration$revenue_PR)
median_PR <- median(remuneration$revenue_PR)
min_PR <- min(remuneration$revenue_PR)
max_PR <- max(remuneration$revenue_PR)
firstqPR <- quantile(remuneration$revenue_PR, 0.25)
thirdqPR <- quantile(remuneration$revenue_PR, 0.75)

# user-centric
mean_UC <- mean(remuneration$revenue_UC)
median_UC <- median(remuneration$revenue_UC)
min_UC <- min(remuneration$revenue_UC)
max_UC <- max(remuneration$revenue_UC)
firstqUC <- quantile(remuneration$revenue_UC, 0.25)
thirdqUC <- quantile(remuneration$revenue_UC, 0.75)

# artist growth model
mean_AGM <- mean(remuneration$revenue_AGM)
median_AGM <- median(remuneration$revenue_AGM)
min_AGM <- min(remuneration$revenue_AGM)
max_AGM <- max(remuneration$revenue_AGM)
firstqAGM <- quantile(remuneration$revenue_AGM, 0.25)
thirdqAGM <- quantile(remuneration$revenue_AGM, 0.75)

