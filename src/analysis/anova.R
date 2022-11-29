library(data.table)
library(dplyr)
library(DescTools)
library(broom)
library(ggplot2)

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

# ANOVA 
# filter 1 model away
aov1 <- lm(revenue ~ model*label_type + model*ratiofem + tlt +nou, remuneration_factors)
anova(aov1)
