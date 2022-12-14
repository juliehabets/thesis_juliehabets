library(data.table)
library(dplyr)
library(broom)
library(ggplot2)
library(stringr)
library(jtools)
library(car)
library(fixest)
library(Hmisc)
library(lmtest)
library(miceadds)


# load data
remuneration_factors_inclna <- fread("../../gen/temp/artist_remuneration_factors_inclna.csv", select = c(2:6))
tlt <- fread("../../gen/temp/tlt.csv", select = c(2:3))

# factor vars
remuneration_factors_inclna$model <- as.factor(remuneration_factors_inclna$model)
remuneration_factors_inclna$model <- relevel(remuneration_factors_inclna$model, "PR")

# revenue as log?
plot(remuneration_factors_inclna$revenue, dnorm(remuneration_factors_inclna$revenue, mean(remuneration_factors_inclna$revenue), sd(remuneration_factors_inclna$revenue)))
remuneration_factors_inclna$revenue <- log(remuneration_factors_inclna$revenue)
plot(remuneration_factors_inclna$revenue, dnorm(remuneration_factors_inclna$revenue, mean(remuneration_factors_inclna$revenue), sd(remuneration_factors_inclna$revenue)))

# estimating the model
mlm <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors_inclna); summary(mlm)
mlm_res <- augment(mlm)

# estimating model incl covariates
remuneration_factors_inclna <- merge(remuneration_factors_inclna, tlt, by = "artist")
names(remuneration_factors_inclna)[6] <- "tlt"

# mean centering ?
remuneration_factors_inclna$tlt_mc <- scale(remuneration_factors_inclna$tlt, center = T, scale = F)
plot(remuneration_factors_inclna$tlt_mc, dnorm(remuneration_factors_inclna$tlt_mc, mean(remuneration_factors_inclna$tlt_mc), sd(remuneration_factors_inclna$tlt_mc)))
mlm_inclna <- mlm_inclna <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt_mc, cluster = 'artist', data = remuneration_factors_inclna); summary(mlm_inclna)

# sqrt? 
remuneration_factors_inclna$tlt_sqrt <- sqrt(remuneration_factors_inclna$tlt)
plot(remuneration_factors_inclna$tlt_sqrt, dnorm(remuneration_factors_inclna$tlt_sqrt, mean(remuneration_factors_inclna$tlt_sqrt), sd(remuneration_factors_inclna$tlt_sqrt)))
mlm_inclna <- mlm_inclna <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt_mc, cluster = 'artist', data = remuneration_factors_inclna); summary(mlm_inclna)

# tlt as log? 
plot(remuneration_factors_inclna$tlt, dnorm(remuneration_factors_inclna$tlt, mean(remuneration_factors_inclna$tlt), sd(remuneration_factors_inclna$tlt)))
remuneration_factors_inclna$tlt <- log(remuneration_factors_inclna$tlt)
plot(remuneration_factors_inclna$tlt, dnorm(remuneration_factors_inclna$tlt, mean(remuneration_factors_inclna$tlt), sd(remuneration_factors_inclna$tlt)))

# estimating model + cov
mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_inclna); summary(mlm)

mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_inclna); summary(mlm)
plot(mlm, 1)

# estimating polynomial model
mlm_pol <- lm(revenue ~ model * label_type + model * ratiofem + tlt + (tlt^3) + (model^3) + (label_type^3) + (ratiofem^3), remuneration_factors_inclna); summary(mlm)
plot(mlm_pol, 1)
# adding polynomial variables does not help much with linearity 


# fixed effects
mlm_f <- feols(revenue ~ model + label_type + ratiofem | artist,
               data = remuneration_factors_inclna,
               cluster = ~ {artist} )

print(tidy(mlm_f, se='cluster', conf.int=TRUE))


############################################
#FINAL REGRESSION MODELS WHICH WILL BE USED#
############################################

mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_inclna); summary(mlm)
mlm_res <- augment(mlm)

mlm_inclna <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt, cluster = 'artist', data = remuneration_factors_inclna); summary(mlm_inclna)

#mlm <- lm_robust(revenue ~ model * label_type + model * ratiofem + tlt, clusters = artist, data = remuneration_factors_inclna); summary(mlm)
mlm_res_inclna <- augment(mlm_inclna$lm_res)
ggplot(mlm_res, aes(x = .fitted, y = .resid, color = artist)) + geom_point()

effect_plot(mlm, pred = model, interval = TRUE, plot.points = TRUE, jitter = 0.5)

#trying sandwich and lmtest
library(sandwich)
library(lmtest)

mlmcoeffs_cl <- coeftest(mlm, vcov = vcovCL, cluster = ~artist)

mlmcis <- coefci(mlm, parm = coi_indices, vcov = vcovCL, cluster = ~artist)
mlm_cl <- coeftest(mlm, vcov = vcovCL, type = "HC1", cluster = ~ artist)
tidy(mlm_cl, conf.int = TRUE)

plot(mlm_cl)

# trying fixest
library(fixest)
model_data <- readRDS("../../gen/temp/model_data.RDS")

lmf <- feols(revenue ~ model+ label_type + ratiofem + tlt
             + model:label_type
             + model:ratiofem
             |
               artist + userid,
             data = model_data,
             cluster = ~ {artist})
print(tidy(lmf, se='cluster', conf.int=TRUE))

lmf2 <- feols(log(revenue) ~ model+ label_type + ratiofem + log(tlt)
              + model:label_type
              + model:ratiofem
              |
                userid,
              data = model_data, cluster = ~ {artist})
print(tidy(lmf2, se='cluster', conf.int=TRUE))

lmf3 <- feols(log(revenue) ~ model+ label_type + ratiofem + log(tlt)
              + model:label_type
              + model:ratiofem
              |
                artist,
              data = model_data, cluster = ~ {artist})
print(tidy(lmf3, se='cluster', conf.int=TRUE))
plot(x= lmf3$fitted.values, y = lmf3$residuals)

plot(lmf2$residuals)
plot(x= lmf2$fitted.values, y = lmf2$residuals)
plot(lmf2, 1)

lmf2_res <- augment(lmf)

#####################################
#CHECKING THE REGRESSION ASSUMPTIONS#
#####################################

# independence
plot(mlm$lm_res, 1)
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
rfcont <- remuneration_factors_inclna[, c(2,3,6)]
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

