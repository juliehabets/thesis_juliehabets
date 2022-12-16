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
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors_inclna.csv", select = c(2:6))
tlt <- fread("../../gen/temp/tlt.csv", select = c(2:3))

# factor vars
remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

# revenue as log?
plot(remuneration_factors$revenue, dnorm(remuneration_factors$revenue, mean(remuneration_factors$revenue), sd(remuneration_factors$revenue)), ylab = "Density", xlab = "Revenue")
remuneration_factors$revenue <- log(remuneration_factors$revenue)
plot(remuneration_factors$revenue, dnorm(remuneration_factors$revenue, mean(remuneration_factors$revenue), sd(remuneration_factors$revenue)), ylab = "Density", xlab = "log(Revenue)")
mlm <- lm(revenue ~ model * label_type + model * ratiofem, remuneration_factors); summary(mlm)

#  incl covariate
remuneration_factors <- merge(remuneration_factors, tlt, by = "artist")
names(remuneration_factors)[6] <- "tlt"

# estimating the model
mlm <- lm(revenue ~ model * label_type + model * ratiofem +tlt, remuneration_factors); summary(mlm)
# with clustered errors
mlm_clus <- mlm <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt, cluster = 'artist', data = remuneration_factors); summary(mlm_clus)

####################################
#COVARIATE VARIABLE TRANSFORMATIONS#
####################################
plot(remuneration_factors$tlt, dnorm(remuneration_factors$tlt, mean(remuneration_factors$tlt), sd(remuneration_factors$tlt)), ylab = "Density", xlab = "Times listened to")

# box-cox ?
b <- boxcox(lm(remuneration_factors$tlt ~ 1))
lambda <- b$x[which.max(b$y)]
remuneration_factors$tlt_bc <- (remuneration_factors$tlt ^ lambda - 1)/ lambda
plot(remuneration_factors$tlt_bc, dnorm(remuneration_factors$tlt_bc, mean(remuneration_factors$tlt_bc), sd(remuneration_factors$tlt_bc)), ylab = "Density", xlab = "Times listened to box-coxed")
mlm_bc <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt_bc, cluster = 'artist', data = remuneration_factors); summary(mlm_bc)
remuneration_factors <- remuneration_factors[, -7]

# sqrt? 
remuneration_factors$tlt_sqrt <- sqrt(remuneration_factors$tlt)
plot(remuneration_factors$tlt_sqrt, dnorm(remuneration_factors$tlt_sqrt, mean(remuneration_factors$tlt_sqrt), sd(remuneration_factors$tlt_sqrt)), ylab = "Density", xlab = "sqrt(Times listened to)")
mlm_sqrt <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt_sqrt, cluster = 'artist', data = remuneration_factors); summary(mlm_sqrt)
remuneration_factors <- remuneration_factors[, -7]

# tlt as log? 
remuneration_factors$tlt <- log(remuneration_factors$tlt)
plot(remuneration_factors$tlt, dnorm(remuneration_factors$tlt, mean(remuneration_factors$tlt), sd(remuneration_factors$tlt)), ylab = "Density", xlab = "log(Times listened to)")
mlm_log <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt, cluster = 'artist', data = remuneration_factors); summary(mlm_log)

# so use tlt as long from now on

#######################################################
#CHECK WHETHER REMOVING NAs IS SIGNIFICNATLY DIFFERENT#
#######################################################

# load data
remuneration_factors_exclna <- fread("../../gen/temp/artist_remuneration_factors_exclna.csv", select = c(2:6))
tlt <- fread("../../gen/temp/tlt.csv", select = c(2:3))

# factor vars
remuneration_factors_exclna$model <- as.factor(remuneration_factors_exclna$model)
remuneration_factors_exclna$model <- relevel(remuneration_factors_exclna$model, "PR")

# revenue as log
remuneration_factors_exclna$revenue <- log(remuneration_factors_exclna$revenue)

# incl covariates
remuneration_factors_exclna <- merge(remuneration_factors_exclna, tlt, by = "artist")
names(remuneration_factors_exclna)[6] <- "tlt"
remuneration_factors_exclna$tlt <- log(remuneration_factors_exclna$tlt)

# estimate model 
mlm_exclna <- lm.cluster(revenue ~ model * label_type + model * ratiofem + tlt, cluster = 'artist', data = remuneration_factors_exclna); summary(mlm_exclna)

# checking via a z-test whether the difference is significant

coeff_excl <- data.frame(mlm_exclna$lm_res$coefficients)
coeff_incl <- data.frame(mlm_log$lm_res$coefficients)

vcov_incl <- data.frame(mlm_log$vcov)
vcov_excl <- data.frame(mlm_exclna$vcov)

# z-test formula 
compare.coeff <- function(b1,se1,b2,se2){
  return((b1-b2)/sqrt(se1^2+se2^2))
}

# performing the test
b1 <- coeff_incl$mlm_log.lm_res.coefficients
se1 <- c(sqrt(4.017350e-06), sqrt(1.177348e-05), sqrt(6.701675e-05), sqrt(2.078365e-07), sqrt(1.226525e-07), sqrt(1.636017e-06), sqrt(3.083636e-05), sqrt(1.741986e-04), sqrt(4.672760e-05), sqrt(3.479817e-04))
b2 <- coeff_excl$mlm_exclna.lm_res.coefficients
se2 <- c(sqrt(3.849499e-06), sqrt(9.407593e-06), sqrt(6.877994e-05), sqrt(1.291018e-07), sqrt(3.726092e-08), sqrt(1.622036e-06), sqrt(2.434563e-05), sqrt(1.764310e-04), sqrt(3.443101e-05), sqrt(3.516288e-04))     

p_value <- 2*pnorm(-abs(compare.coeff(b1, se1, b2, se2)))
p_value


# so, statistically different from other model
# r squared of the excluding model is higher, hence we use the exlcuding NA regression from now on

#####################################
#CHECKING THE REGRESSION ASSUMPTIONS#
#####################################

# independence & linearity
plot(mlm_exclna$lm_res, 1, family = "serif", col = "#506B99")

 # equality of variance (homoscedasticity)
plot(mlm_exclna$lm_res, 3, family = "serif", col = "#506B99")

# normality
plot(mlm_exclna$lm_res, 2, family = "serif", col = "#506B99")
ggplot(mlm_excla_res, aes(.resid)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.5, col = "#bed6ff", fill = "#bed6ff") + 
  stat_function(fun = dnorm, args = list(mean = mean(mlm_excla_res$.resid), sd = sd(mlm_excla_res$.resid)), color="#506B99", size=2) + 
  theme_light() + 
  labs(x = "Residuals", y = "Density") + 
  theme(text = element_text(size = 12, family = "serif"))

############################
#CHECKING MULTICOLLINEARITY#
############################

# correlation matrix --> do this with the continuous IVs 
rfcont <- remuneration_factors_exclna[, c(2,3,6)]
res <- rcorr(as.matrix(rfcont))
res

vif(mlm_exclna$lm_res, type = "predictor")

# all VIFS are close to 1 which is good. Also, the correlation matrix looks good

#####################
#STATISTICAL TESTING#
#####################

#load dta
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
modelAGM <- (exp(mlm_exclna$lm_res$coefficients[2])-1)*100
modelAGM
modelUC <- (exp(mlm_exclna$lm_res$coefficients[3])-1)*100
modelUC
label_type <- (exp(mlm_exclna$lm_res$coefficients[4])-1)*100
label_type
ratiofem <- (exp(mlm_exclna$lm_res$coefficients[5])-1)*100
ratiofem
tlt <- (1.05^(mlm_exclna$lm_res$coefficients[6])-1)*100
tlt
modelAGMlabel_type <- (exp(mlm_exclna$lm_res$coefficients[7])-1)*100
modelAGMlabel_type
modelUClabel_type <- (exp(mlm_exclna$lm_res$coefficients[8])-1)*100
modelUClabel_type
modelAGMratiofem <- (exp(mlm_exclna$lm_res$coefficients[9])-1)*100
modelAGMratiofem
modelUCratiofem <-(exp(mlm_exclna$lm_res$coefficients[10])-1)*100
modelUCratiofem

