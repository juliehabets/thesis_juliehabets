library(data.table)
library(dplyr)
library(DescTools)

# load data
remuneration <- fread("../../gen/temp/artist_remuneration_final.csv", select = c(2:7))

###################
#GINI COEFFICIENTS#
###################

# calculate the Gini's
gini_PR <- Gini(remuneration$revenue_PR)
gini_UC <- Gini(remuneration$revenue_UC)
gini_AGM <- Gini(remuneration$revenue_AGM)

gini_total <- c(gini_PR, gini_UC, gini_AGM)


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

t.test(remuneration$revenue_PR, remuneration$revenue_UC,alternative="two.sided",
       conf.level=0.95)
       