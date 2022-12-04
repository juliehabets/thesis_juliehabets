library(data.table)
library(dplyr)
library(DescTools)
library(ggplot2)
library(boot)

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
gini_PR <- Gini(remuneration$revenue_PR)
gini_UC <- Gini(remuneration$revenue_UC)
gini_AGM <- Gini(remuneration$revenue_AGM)

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
plot(Lc(remuneration$revenue_PR), col = "#bed6ff", 
     lwd = 2, xlab = "cumulative % of artists", 
     ylab = "cumulative % of income", family = "serif")
text(x = 0.16, y = 0.9, "Gini = 0.83", cex = 1.1, family = "serif")
ggsave("../../gen/output/lorenzcurve_pr.png")

# user centric
plot(Lc(remuneration$revenue_UC), col = "red", lwd = 2, 
     xlab = "cumulative % of artists",
     ylab = "cumulative % of income", family = "serif")
text(x = 0.16, y = 0.9, "Gini = 0.87", cex = 1.1, family = "serif")
ggsave("../../gen/output/lorenzcurve_uc.png")

# agm
plot(Lc(remuneration$revenue_AGM), col = "#FFE8BE", lwd = 2, 
     xlab = "cumulative % of artists",
     ylab = "cumulative % of income", family = "serif")
text(x = 0.16, y = 0.9, "Gini = 0.77", cex = 1.1, family = "serif")
ggsave("../../gen/output/lorenzcurve_agm.png")

# overlaying the lorenz curves
plot(Lc(remuneration$revenue_PR), col = '#bed6ff', 
     xlab = "cumulative % of artists", 
     ylab = "cumulative % of income", family = "serif")
lines(Lc(remuneration$revenue_UC), col = 'red')
lines (Lc(remuneration$revenue_AGM), col = '#FFE8BE')
legend("topleft", c("Pro Rata", "User-Centric", "AGM"), 
       fill = c("#bed6ff", "red", "#FFE8BE"))
ggsave("../../gen/output/lorenzcurves_all.png")


####################################################
#BOOTSTRAPPING THE GINI TO GET CONFIDENCE INTERVALS#
####################################################

# pro rata
boot(remuneration$revenue_PR, Gini, 500)
boot_pr <- boot(remuneration$revenue_PR, Gini, 500)
quantile(boot_pr$t, probs = c(0.025, 0.975))
plot(density(boot_pr$t), family = "serif")
ggsave("../../gen/output/densitygini_pr.png")

# user centric
boot(remuneration$revenue_UC, Gini, 500)
boot_uc <- boot(remuneration$revenue_UC, Gini, 500)
quantile(boot_uc$t, probs = c(0.025, 0.975))
plot(density(boot_uc$t), family = "serif")
ggsave("../../gen/output/densitygini_uc.png")

# agm
boot(remuneration$revenue_AGM, Gini, 500)
boot_agm <- boot(remuneration$revenue_AGM, Gini, 500)
quantile(boot_agm$t, probs = c(0.025, 0.975))
plot(density(boot_agm$t), family = "serif")
ggsave("../../gen/output/densitygini_agm.png")

#####################
#STATISTICAL TESTING#
#####################

# pro rata vs. user-centric
# pro rata is less equal than user centric --> gini should be higher
testpruc <- t.test(boot_pr$t, boot_uc$t, alternative= "two.sided", conf.level=0.95, paired = TRUE)
testpruc


# pro rata vs. agm
testpragm <- t.test(boot_pr$t, boot_agm$t, alternative= "two.sided", conf.level=0.95, paired = TRUE)
testpragm

# agm vs. user-centric
testagmuc <- t.test(boot_agm$t, boot_uc$t, alternative= "two.sided", conf.level=0.95, paired = TRUE)
testagmuc
