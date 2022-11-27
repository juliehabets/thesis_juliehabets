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

# DUURT KAULO LANG Gini_PR <- Gini(remuneration$revenue_PR, conf.level = 0.95)

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

