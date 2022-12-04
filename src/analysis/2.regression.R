library(data.table)
library(dplyr)
library(broom)
library(ggplot2)
library(stringr)
library(jtools)
library(car)
library(fixest)
library(Hmisc)

# load data
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))

# factor vars
remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

# revenue as log?
plot(remuneration_factors$revenue, dnorm(remuneration_factors$revenue, mean(remuneration_factors$revenue), sd(remuneration_factors$revenue)))
remuneration_factors$revenue <- log(remuneration_factors$revenue)
plot(remuneration_factors$revenue, dnorm(remuneration_factors$revenue, mean(remuneration_factors$revenue), sd(remuneration_factors$revenue)))

# estimating the model
mlm <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors); summary(mlm)
mlm_res <- augment(mlm)

# estimating model incl covariates
remuneration_factors <- merge(remuneration_factors, tlt, by = "artist")
names(remuneration_factors)[6] <- "tlt"

# tlt as log? 
plot(remuneration_factors$tlt, dnorm(remuneration_factors$tlt, mean(remuneration_factors$tlt), sd(remuneration_factors$tlt)))
remuneration_factors$tlt <- log(remuneration_factors$tlt)

# estimating model + cov
mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors); summary(mlm)

# estimating model + cov
mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt + nou, remuneration_factors); summary(mlm)

#no interactions
mlm_noint <- lm(revenue ~ model + label_type + ratiofem + tlt, remuneration_factors); summary(mlm_noint)
plot(mlm_noint, 1)

mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors); summary(mlm)
plot(mlm, 1)

# estimating polynomial model
mlm_pol <- lm(revenue ~ model * label_type + model * ratiofem + tlt + (tlt^3) + (model^3) + (label_type^3) + (ratiofem^3), remuneration_factors); summary(mlm)
plot(mlm_pol, 1)
# adding polynomial variables does not help much with linearity 


# fixed effects
mlm_f <- feols(revenue ~ model + label_type + ratiofem | artist,
               data = remuneration_factors,
               cluster = ~ {artist} )

print(tidy(mlm_f, se='cluster', conf.int=TRUE))


############################################
#FINAL REGRESSION MODELS WHICH WILL BE USED#
############################################

mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors); summary(mlm)
mlm_res <- augment(mlm)

effect_plot(mlm, pred = model, interval = TRUE, plot.points = TRUE, jitter = 0.5)

#####################################
#CHECKING THE REGRESSION ASSUMPTIONS#
#####################################

# independence
plot(mlm, 1)
durbinWatsonTest(mlm)
# p > 0.05, so the errors are not autocorralted, which means that we have not violated the independence assumption with time-based dependence


# equality of variance (homoscedasticity)
plot(mlm, 3)
ncvTest(mlm)
bptest(mlm)
# p < 0.05 in both tests so data is not homoscedastistic

# normality
ggplot(mlm_res, aes(.resid)) + geom_histogram(aes(y = ..density..), binwidth = 0.5) + stat_function(fun = dnorm, args = list(mean = mean(mlm_res$.resid), sd = sd(mlm_res$.resid)), color="red", size=2)

# linearity 
plot(mlm, 1)


# plottig the observed versus the predicted values
plot(mlm$fitted.values, mlm$model$revenue)


############################
#CHECKING MULTICOLLINEARITY#
############################

# correlation matrix --> do this with the continuous IVs 
rfcont <- remuneration_factors[, c(2,3,6)]
res <- rcorr(as.matrix(rfcont))
res

vif(mlm, type = "predictor")

# all VIFS are close to 1 which is good. Also, the correlation matrix looks good

#####################
#STATISTICAL TESTING#
#####################
# t test with revenue itself 
test_pr_uc <- t.test(remuneration$revenue_PR_log, remuneration$revenue_UC_log, alternative="two.sided", conf.level=0.95, paired = TRUE)
test_pr_uc

test_pr_agm <- t.test(remuneration$revenue_PR_log, remuneration$revenue_AGM_log,alternative="two.sided", conf.level=0.95, paired = TRUE)
test_pr_agm

test_agm_uc <- t.test(remuneration$revenue_AGM_log, remuneration$revenue_UC_log,alternative="two.sided", conf.level=0.95, paired = TRUE)
test_agm_uc

