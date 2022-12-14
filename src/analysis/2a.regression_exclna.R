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
remuneration_factors_exclna <- fread("../../gen/temp/artist_remuneration_factors_exclna.csv", select = c(2:6))
tlt <- fread("../../gen/temp/tlt.csv", select = c(2:3))

# factor vars
remuneration_factors_exclna$model <- as.factor(remuneration_factors_exclna$model)
remuneration_factors_exclna$model <- relevel(remuneration_factors_exclna$model, "PR")

# revenue as log?
plot(remuneration_factors_exclna$revenue, dnorm(remuneration_factors_exclna$revenue, mean(remuneration_factors_exclna$revenue), sd(remuneration_factors_exclna$revenue)))
remuneration_factors_exclna$revenue <- log(remuneration_factors_exclna$revenue)
plot(remuneration_factors_exclna$revenue, dnorm(remuneration_factors_exclna$revenue, mean(remuneration_factors_exclna$revenue), sd(remuneration_factors_exclna$revenue)))

# estimating the model
mlm <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors_exclna); summary(mlm)
mlm_res <- augment(mlm)

# estimating model incl covariates
remuneration_factors_exclna <- merge(remuneration_factors_exclna, tlt, by = "artist")
names(remuneration_factors_exclna)[6] <- "tlt"

# tlt as log? 
plot(remuneration_factors_exclna$tlt, dnorm(remuneration_factors_exclna$tlt, mean(remuneration_factors_exclna$tlt), sd(remuneration_factors_exclna$tlt)))
remuneration_factors_exclna$tlt <- log(remuneration_factors_exclna$tlt)
plot(remuneration_factors_exclna$tlt, dnorm(remuneration_factors_exclna$tlt, mean(remuneration_factors_exclna$tlt), sd(remuneration_factors_exclna$tlt)))

# estimating model + cov
mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_exclna); summary(mlm)

mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_exclna); summary(mlm)
plot(mlm, 1)

# estimating polynomial model
mlm_pol <- lm(revenue ~ model * label_type + model * ratiofem + tlt + (tlt^3) + (model^3) + (label_type^3) + (ratiofem^3), remuneration_factors_exclna); summary(mlm)
plot(mlm_pol, 1)
# adding polynomial variables does not help much with linearity 


# fixed effects
mlm_f <- feols(revenue ~ model + label_type + ratiofem | artist,
               data = remuneration_factors_exclna,
               cluster = ~ {artist} )

print(tidy(mlm_f, se='cluster', conf.int=TRUE))


############################################
#FINAL REGRESSION MODELS WHICH WILL BE USED#
############################################

mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_exclna); summary(mlm)
mlm_res <- augment(mlm)

mlm_exclna <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt, cluster = 'artist', data = remuneration_factors_exclna); summary(mlm_exclna)
#mlm <- lm_robust(revenue ~ model * label_type + model * ratiofem + tlt, clusters = artist, data = remuneration_factors_exclna); summary(mlm)
mlm_res_exclna <- augment(mlm_exclna$lm_res)
ggplot(mlm_res, aes(x = .fitted, y = .resid, color = artist)) + geom_point()

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
plot(mlm$lm_res, 1, family = "serif") + lines(col = "#bed6ff") 

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
rfcont <- remuneration_factors_exclna[, c(2,3,6)]
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

