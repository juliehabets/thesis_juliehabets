library(data.table)
library(dplyr)
library(DescTools)
library(ggplot2)
library(boot)

# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final_exclna.csv", select = c(2:7))
remuneration_spread <- fread("../../gen/temp/artist_remuneration_factors_exclna.csv", select = c(2:6))
remuneration_spread$model <- as.factor(remuneration_spread$model)

###################
#GINI COEFFICIENTS#
###################

# calculate the Gini's
gini_PR <-  DescTools::Gini(remuneration$revenue_PR)
gini_UC <-  DescTools::Gini(remuneration$revenue_UC)
gini_AGM <-  DescTools::Gini(remuneration$revenue_AGM)

###############
#LORENZ CURVES#
###############

# pro rata
plot(Lc(remuneration$revenue_PR), col = "#bed6ff", 
     lwd = 2, xlab = "cumulative % of artists", 
     ylab = "cumulative % of income", family = "serif")
text(x = 0.16, y = 0.9, "Gini = 0.812", cex = 1.1, family = "serif")
ggsave("../../gen/output/lorenzcurve_pr.png")

# user centric
plot(Lc(remuneration$revenue_UC), col = "#506B99", lwd = 2, 
     xlab = "cumulative % of artists",
     ylab = "cumulative % of income", family = "serif")
text(x = 0.16, y = 0.9, "Gini = 0.849", cex = 1.1, family = "serif")
ggsave("../../gen/output/lorenzcurve_uc.png")

# agm
plot(Lc(remuneration$revenue_AGM), col = "#FFE8BE", lwd = 2, 
     xlab = "cumulative % of artists",
     ylab = "cumulative % of income", family = "serif")
text(x = 0.16, y = 0.9, "Gini = 0.755", cex = 1.1, family = "serif")
ggsave("../../gen/output/lorenzcurve_agm.png")

# overlaying the lorenz curves
plot(Lc(remuneration$revenue_PR), col = '#bed6ff', 
     xlab = "cumulative % of artists", 
     ylab = "cumulative % of income", family = "serif")
lines (Lc(remuneration$revenue_AGM), col = '#FFE8BE')
lines(Lc(remuneration$revenue_UC), col = '#506B99')
legend("topleft", c("Pro rata", "Artist growth model", "User-centric"), 
       fill = c("#bed6ff", "#FFE8BE", "#506B99"))
ggsave("../../gen/output/lorenzcurves_all.png")


####################################################
#BOOTSTRAPPING THE GINI TO GET CONFIDENCE INTERVALS#
####################################################

# pro rata
boot(remuneration$revenue_PR,  DescTools::Gini, 1499)
boot_pr <- boot(remuneration$revenue_PR,  DescTools::Gini, 1499)
quantile(boot_pr$t, probs = c(0.025, 0.975))
plot(density(boot_pr$t), family = "serif") #iep
ggsave("../../gen/output/densitygini_pr.png")

# user centric
boot(remuneration$revenue_UC, DescTools::Gini, 1499)
boot_uc <- boot(remuneration$revenue_UC, DescTools::Gini, 1499)
quantile(boot_uc$t, probs = c(0.025, 0.975))
plot(density(boot_uc$t), family = "serif") #iep
ggsave("../../gen/output/densitygini_uc.png")

# agm
boot(remuneration$revenue_AGM,  DescTools::Gini, 1499)
boot_agm <- boot(remuneration$revenue_AGM,  DescTools::Gini, 1499)
quantile(boot_agm$t, probs = c(0.025, 0.975))
plot(density(boot_agm$t), family = "serif") #iep
ggsave("../../gen/output/densitygini_agm.png")

#####################
#STATISTICAL TESTING#
#####################

# pro rata vs. user-centric
testpruc <- t.test(boot_pr$t, boot_uc$t, alternative= "two.sided", conf.level=0.95, paired = TRUE)
testpruc


# pro rata vs. agm
testpragm <- t.test(boot_pr$t, boot_agm$t, alternative= "two.sided", conf.level=0.95, paired = TRUE)
testpragm

# agm vs. user-centric
testagmuc <- t.test(boot_agm$t, boot_uc$t, alternative= "two.sided", conf.level=0.95, paired = TRUE)
testagmuc
