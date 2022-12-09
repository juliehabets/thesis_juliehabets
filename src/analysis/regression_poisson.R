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

#libs for cluster
library(miceadds)
library(estimatr)

# load data
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))

# factor vars
remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

#  incl covariate
remuneration_factors <- merge(remuneration_factors, tlt, by = "artist")
names(remuneration_factors)[6] <- "tlt"

# label_type as integer
remuneration_factors$label_type <- as.integer(remuneration_factors$label_type)


# estimating poisson model
mlm_pois <- fepois(revenue ~ model * label_type + model * ratiofem + tlt, data = remuneration_factors, cluster = ~ {artist}, panel.id = ~artist)
print(tidy(mlm_pois, se='cluster', conf.int=TRUE))

# plot
plot(x = mlm_pois$fitted.values, y = mlm_pois$working_residuals)