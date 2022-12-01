library(data.table)
library(dplyr)
library(DescTools)
library(ggplot2)

# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final.csv", select = c(2:7))
remuneration_spread <- fread("../../gen/temp/artist_remuneration_factors.csv", select = c(2:6))
remuneration_spread$model <- as.factor(remuneration_spread$model)

# transforming revenue to log
remuneration$revenue_PR_log <- log(remuneration$revenue_PR)
remuneration$revenue_UC_log <- log(remuneration$revenue_UC)
remuneration$revenue_AGM_log <- log(remuneration$revenue_AGM)

remuneration_spread$revenue_log <- log(remuneration_spread$revenue)

###################
#GINI COEFFICIENTS#
###################

# calculate the Gini's
gini_PR <- Gini(remuneration$revenue_PR, conf.level = TRUE, na.rm = TRUE)
gini_UC <- Gini(remuneration$revenue_UC, conf.level = TRUE, na.rm = TRUE)
gini_AGM <- Gini(remuneration$revenue_AGM, conf.level = TRUE, na.rm = TRUE)

gini_total <- data.frame(gini_PR, gini_UC, gini_AGM)
gini_total <- data.frame(model = c("PR", "UC", "AGM"),
                         gini = c(gini_PR, gini_UC, gini_AGM))
gini_total$model <- as.factor(gini_total$model)
gini_total$model <- relevel(gini_total$model, "PR")

aov <- lm(gini ~ model, gini_total)
summary(aov)

###############
#LORENZ CURVES#
###############

# pro rata
plot(Lc(remuneration$revenue_PR), col = "blue", lwd = 2, main = "Lorenz Curve Pro Rata Model", 
     xlab = "cumulative % of artists", ylab = "cumulative % of income")
text(x = 0.16, y = 0.9, "Gini = 0.83", cex = 1.1)

# user centric
plot(Lc(remuneration$revenue_UC), col = "red", lwd = 2, main = "Lorenz Curve User-Centric Model", 
     xlab = "cumulative % of artists", ylab = "cumulative % of income")
text(x = 0.16, y = 0.9, "Gini = 0.87", cex = 1.1)

# agm
plot(Lc(remuneration$revenue_AGM), col = "green", lwd = 2, main = "Lorenz Curve AGM model", 
     xlab = "cumulative % of artists", ylab = "cumulative % of income")
text(x = 0.16, y = 0.9, "Gini = 0.77", cex = 1.1)

# overlaying the lorenz curves
plot(Lc(remuneration$revenue_PR), col = 'blue')
lines(Lc(remuneration$revenue_UC), col = 'red')
lines (Lc(remuneration$revenue_AGM), col = 'green')
legend("topleft", c("Pro Rata", "User-Centric", "AGM"), fill = c("blue", "red", "green"))

#####################
#STATISTICAL TESTING#
#####################
# t test with revenue itself 
test_pr_uc <- t.test(remuneration$revenue_PR_log, remuneration$revenue_UC_log,alternative="two.sided", conf.level=0.95)
test_pr_uc$p.value

test_pr_agm <- t.test(remuneration$revenue_PR_log, remuneration$revenue_AGM_log,alternative="two.sided", conf.level=0.95)
test_pr_agm$p.value

test_uc_agm <- t.test(remuneration$revenue_UC_log, remuneration$revenue_AGM_log,alternative="two.sided", conf.level=0.95)
test_uc_agm$p.value

# other trying of t test
remuneration_PR <- remuneration[, c(1:4)]
remuneration_UC <- remuneration[, c(1:3,5)]
remuneration_AGM <- remuneration[, c(1:3,6)]

remuneration_PR$gini <- gini_PR
remuneration_UC$gini <- gini_UC
remuneration_AGM$gini <- gini_AGM

ttest_pruc <- t.test(remuneration_PR$gini, remuneration_UC$gini,alternative="two.sided", conf.level=0.95)

# does not work: error is
#Error in t.test.default(remuneration_PR$gini, remuneration_UC$gini, alternative = "two.sided",  :  data are essentially constant

# trying else
ttt <- remuneration_spread
ttt$gini[ttt$model == 'PR'] <- gini_PR
ttt$gini[ttt$model == 'UC'] <- gini_UC
ttt$gini[ttt$model == 'AGM'] <- gini_AGM

lm <- aov(gini ~ model, ttt)
summary(lm)
emmeans(lm, pairwise ~ model, adjust = "bonferroni")

t.test(gini~model, data = ttt, paired = TRUE)
