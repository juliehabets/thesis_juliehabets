library(data.table)
library(dplyr)
library(DescTools)
library(broom)
library(ggplot2)
library(car)
# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final.csv", select = c(2:7))
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))

# recoding ratiofem variable
remuneration_factors$ratiofem [remuneration_factors$ratiofem >= 0.5 ] <- "female"
remuneration_factors$ratiofem [remuneration_factors$ratiofem < 0.5 ] <- "male"

remuneration_factors$ratiofem <- as.factor(remuneration_factors$ratiofem)
remuneration_factors$ratiofem <- relevel(remuneration_factors$ratiofem, "male")

#recoding vars & adding covariates
remuneration_factors$revenue <- log(remuneration_factors$revenue)
remuneration_factors <- merge(remuneration_factors, tlt, by = "artist")
names(remuneration_factors)[6] <- "tlt"
remuneration_factors  <- merge(remuneration_factors, nou, by = "artist")
names(remuneration_factors)[7] <- "nou"

remuneration_factors$tlt <- log(remuneration_factors$tlt)
remuneration_factors$nou <- log(remuneration_factors$nou)

remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

remuneration_factors$label_type <- as.factor(remuneration_factors$label_type)
remuneration_factors$tlt_factor <- as.factor(remuneration_factors$tlt)

# ANOVA 
# 1 without interaction & without control
aov1 <- lm(revenue ~ model+ label_type +ratiofem, remuneration_factors)
anova(aov1)
summary(aov1)

# with interactions & without control
aov2 <- lm(revenue ~ model *label_type * ratiofem, remuneration_factors)
anova(aov2)
summary(aov2)

# with control & without interaction
aov3 <- lm(revenue ~ model + label_type + ratiofem + tlt +nou, remuneration_factors)
anova(aov3)
summary(aov3)

# with control & interaction 
aov4 <- lm(revenue ~ model *label_type + model * ratiofem + tlt_factor, remuneration_factors)
anova(aov4)
summary(aov4)

# checking which is best 
library(AICcmodavg)
model.set <- list(aov1, aov2, aov3, aov4)
model.names <- c("wo/ic", "w/iwo/c", "wo/iw/c", "w/ic")

aictab(model.set, modnames = model.names)

# so model 4 is best

# check homoscedasticity
leveneTest(revenue ~ model *label_type * ratiofem * tlt_factor, remuneration_factors, center=mean)

#violated

# check normality 
ks.test(aov4_res$revenue, "pnorm", mean=mean(aov4_res$revenue), sd=sd(aov4_res$revenue))

aov4_res <- augment(aov4)
ggplot(aov4_res, aes(.resid)) + geom_histogram(aes(y = ..density..), binwidth = 5) + stat_function(fun = dnorm, args = list(mean = mean(aov4_res$.resid), sd = sd(aov4_res$.resid)), color="red", size=2)
ggplot(remuneration_factors, aes(x = revenue)) + geom_histogram()
# violated
