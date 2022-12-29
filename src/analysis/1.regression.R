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

# revenue as log
plot(remuneration_factors_exclna$revenue, dnorm(remuneration_factors_exclna$revenue, mean(remuneration_factors_exclna$revenue), sd(remuneration_factors_exclna$revenue)), ylab = "Density", xlab = "Revenue", family = "serif", col = "#506B99")
remuneration_factors_exclna$revenue <- log(remuneration_factors_exclna$revenue)
plot(remuneration_factors_exclna$revenue, dnorm(remuneration_factors_exclna$revenue, mean(remuneration_factors_exclna$revenue), sd(remuneration_factors_exclna$revenue)), ylab = "Density", xlab = "Revenue", family = "serif", col = "#506B99")

# incl covariates
remuneration_factors_exclna <- merge(remuneration_factors_exclna, tlt, by = "artist")
names(remuneration_factors_exclna)[6] <- "tlt"

# estimate models
mlm_1 <- lm(revenue ~ model * label_type + model * ratiofem, data = remuneration_factors_exclna); summary(mlm_1)
mlm_2 <- lm(revenue ~ model * label_type + model * ratiofem + tlt, data = remuneration_factors_exclna); summary(mlm_2)
mlm_3 <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt, cluster = 'artist', data = remuneration_factors_exclna); summary(mlm_3)

# get F-statistic
summary(mlm_3$lm_res)

mlm_excla_res <- augment(mlm_3$lm_res)

#####################################
#CHECKING THE REGRESSION ASSUMPTIONS#
#####################################

# independence & linearity
plot(mlm_3$lm_res, 1, family = "serif", col = "#506B99")

 # equality of variance (homoscedasticity)
plot(mlm_3$lm_res, 3, family = "serif", col = "#506B99")

# normality
plot(mlm_3$lm_res, 2, family = "serif", col = "#506B99")
ggplot(mlm_excla_res, aes(.resid)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.5, col = "#bed6ff", fill = "#bed6ff") + 
  stat_function(fun = dnorm, args = list(mean = mean(mlm_excla_res$.resid), sd = sd(mlm_excla_res$.resid)), color="#506B99", size=2) + 
  theme_light() + 
  labs(x = "Residuals", y = "Density") + 
  theme(text = element_text(size = 12, family = "serif"))

############################
#CHECKING MULTICOLLINEARITY#
############################
remuneration <- fread("../../gen/temp/artist_remuneration_final_exclna.csv", select = c(2:7))

# incl covariates
remuneration <- merge(remuneration, tlt, by = "artist")
names(remuneration)[7] <- "tlt"

# correlation matrix --> do this with the continuous IVs 
rfcont <- remuneration[, c(2,3,7)]
res <- rcorr(as.matrix(rfcont))
res

vif(mlm_3$lm_res, type = "predictor")

# all VIFS are close to 1 which is good. Also, the correlation matrix looks good

#####################
#STATISTICAL TESTING#
#####################

#load data
remuneration <- fread("../../gen/temp/artist_remuneration_final_exclna.csv", select = c(2:7))

# transform revenue to log
remuneration$revenue_PR <- log(remuneration$revenue_PR)
remuneration$revenue_AGM <- log(remuneration$revenue_AGM)
remuneration$revenue_UC <- log(remuneration$revenue_UC)

# t test with revenue itself 
test_pr_uc <- t.test(remuneration$revenue_PR, remuneration$revenue_UC, alternative="two.sided", conf.level=0.95, paired = TRUE)
test_pr_uc

test_pr_agm <- t.test(remuneration$revenue_PR, remuneration$revenue_AGM,alternative="two.sided", conf.level=0.95, paired = TRUE)
test_pr_agm

test_agm_uc <- t.test(remuneration$revenue_AGM, remuneration$revenue_UC,alternative="two.sided", conf.level=0.95, paired = TRUE)
test_agm_uc


##########################
#INTERPRETATION OF COEFFS#
##########################
intercept <- exp(mlm_3$lm_res$coefficients[1])
intercept
modelAGM <- (exp(mlm_3$lm_res$coefficients[2])-1)*100
modelAGM
modelUC <- (exp(mlm_3$lm_res$coefficients[3])-1)*100
modelUC
label_type <- (exp(mlm_3$lm_res$coefficients[4])-1)*100
label_type
ratiofem <- (exp(mlm_3$lm_res$coefficients[5])-1)*100
ratiofem
tlt <- (exp(mlm_3$lm_res$coefficients[6])-1)*100
tlt
modelAGMlabel_type <- (exp(mlm_3$lm_res$coefficients[7])-1)*100
modelAGMlabel_type
modelUClabel_type <- (exp(mlm_3$lm_res$coefficients[8])-1)*100
modelUClabel_type
modelAGMratiofem <- (exp(mlm_3$lm_res$coefficients[9])-1)*100
modelAGMratiofem
modelUCratiofem <-(exp(mlm_3$lm_res$coefficients[10])-1)*100
modelUCratiofem
