library(emmeans)
library(lavaan)

#####################
# FITTING THE MODEL##
#####################

trends <- rbind(
  emtrends(mlm_exclna, ~1, "model"),
  emtrends(mlm_incla, ~1, "model")
)



# lavaan method
m0 <- sem("revenue ~ model * label_type + model * ratiofem + tlt", data = remuneration_factors_exclna)
m <- sem("revenue ~ model * label_type + model * ratiofem + tlt", data = remuneration_factors_inclna)

# Z test
model_AGM <- (0.2262242891 - 0.256822909)/sqrt((0.0030671800)^2 + (0.0034312504)^2)


#coeftest
vcov_excldf <- data.frame(vcov_excl)
vcov_incldf <- data.frame(vcov_incl)

coeff_excl <- data.frame(mlm_exclna$lm_res$coefficients)
coeff_incl <- data.frame(mlm_inclna$lm_res$coefficients)

coeftest(coeff_excl, coeff_incl, vcov = vcov_excldf)

# other models
mlm2_exclna <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_exclna); summary(mlm)
mlm2_exclna <- lm(revenue ~ model * label_type + model * ratiofem + tlt, remuneration_factors_inclna); summary(mlm)

anova(mlm2_exclna, mlm2_exclna)

# other try
mlm_res_inclna <- mlm_res_inclna[, -1]

ttest <- t.test(mlm_res_exclna$.fitted, mlm_res_inclna$.fitted, alternative="two.sided", conf.level=0.95)
ttest


# t test
diff_agm <- 0.256822909 - 0.2262242891
se <- sqrt((sqrt(1.177348e-05))^2 + (sqrt(9.407593e-06))^2)
agm <- diff_agm/se

diff_label <- 0.003440901 - (-0.0004949594)
se <- sqrt((sqrt(2.078365e-07))^2 + (sqrt(1.291018e-07))^2)
label <- diff_label/se

# another anova
anova(mlm_inclna$lm_res, mlm_exclna$lm_res)



# another
compare.coeff <- function(b1,se1,b2,se2){
  return((b1-b2)/sqrt(se1^2+se2^2))
}

b1 <- coeff_excl$mlm_exclna.lm_res.coefficients
se1 <- c(sqrt(3.849499e-06), sqrt(9.407593e-06), sqrt(6.877994e-05), sqrt(1.291018e-07),  sqrt(3.726092e-08),  sqrt(1.622036e-06) , sqrt(2.434563e-05) , sqrt(1.764310e-04) , sqrt(3.443101e-05) , sqrt(3.516288e-04))
b2 <- coeff_incl$mlm_inclna.lm_res.coefficients
se2 <- c(sqrt(4.017350e-06), sqrt(1.177348e-05), sqrt(6.701675e-05), sqrt(2.078365e-07), sqrt(1.226525e-07), sqrt(1.636017e-06), sqrt(3.083636e-05), sqrt(1.741986e-04), sqrt(4.672760e-05), sqrt(3.479817e-04))      

p_value <- 2*pnorm(-abs(compare.coeff(b1, se1, b2, se2)))
p_value
