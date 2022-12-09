library(car)
library(data.table)
library(dplyr)
library(lmtest)

# load data
remuneration_factors <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))

# factor vars
remuneration_factors$model <- as.factor(remuneration_factors$model)
remuneration_factors$model <- relevel(remuneration_factors$model, "PR")

# revenue as log
remuneration_factors$revenue <- log(remuneration_factors$revenue)

# add covariates
remuneration_factors <- merge(remuneration_factors, tlt, by = "artist")
names(remuneration_factors)[6] <- "tlt"

# tlt as log 
remuneration_factors$tlt <- log(remuneration_factors$tlt)

# estimating the model
mlm <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors); summary(mlm)

bptest(mlm)

# estimating other model with HCSE (open AI)
mlm.hcse <- lm(revenue ~ model * label_type + model * ratiofem + tlt, data = remuneration_factors, vcov = vcovHC)
model_HC <- sandwich::vcovHC(mlm, type = "HC3")
model_HC2 <- sandwich::vcovHC(mlm, type = "HC3", sandwich = FALSE)

vcov(mlm)

sigma2 <- sum(residuals(mlm)^2)/98
sigma2 * solve(crossprod(cbind(1, remuneration_factors$model * remuneration_factors$label_type + remuneration_factors$model * remuneration_factors$ratiofem + remuneration_factors$tlt)))

# generalized linear models
glm <- glm(revenue ~ model * label_type + model * ratiofem + tlt, family = "quasipoisson", data = remuneration_factors)
summary(glm)

plot(glm, 1)

# no logs 
glm <-  glm(revenue ~ model * label_type + model * ratiofem + tlt, family = "gaussian", data = remuneration_factors)
plot(glm, 1)

# no outliers

