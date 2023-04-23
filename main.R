#Read in the data

dd <- read.csv("~/Downloads/proj_ecometrica/global_data.csv", sep = ";")



### Install packages

install.packages("sandwich")
install.packages("lmtest")

### Load packages

library(sandwich)
library(lmtest)


### Model

# m <- lm(wage ~ age + educ_higher, data = dd)
m <- lm(Happiness.Score ~ nakoplenia_ludei.xls + rashodi_na_med.xls + chislo_ubistv.xls + 
                          inflazia.xls + nalogi.xls + procent_tryd_naselenia.xls + 
                          trati_na_obraz.xls + rogdaemost.xls + patenti.csv + 
                          voennie_trati.xls + bezrabotiza.xls + vvp_pps_na_dyshy.xls + vvp.xls, data=dd)
summary(m)


# Heteroskedasticity tests

bptest(m) # White test
bptest(m, varformula = ~ age + I(age^2) + educ_higher + I(age * educ_higher), data = dd) # White test

gqtest(m) # Goldfeld Quandt, but senseless
gqtest(m, fraction = 0.2, order.by = dd$age, alternative = "two.sided") # Tune it to be ok

# Glejser "by hand"
e <- m$residuals # Here we get a series of residuals
mGlejser <- lm(abs(e) ~ dd$age) # Glejser test model
summary(mGlejser) # Summary to make decisions


# Robust Errors

vcovHC(m)

coeftest(m, vcov. = vcovHC(m, type = "HC5"))


m2 <- lm(wage ~ age + I(age^2) + educ_higher + I(age * educ_higher) + I(age^2 * educ_higher),
         data = dd)
summary(m2)

coeftest(m2, vcov. = vcovHC(m2))


# Plot dependency of wage on age for those without higher educ. (black) and with (red)

age <- c(18:70)
incNo <- 12.2 + 0.55 * age - 0.008 * (age^2)
incH <- (12.2 - 16.77) + (0.55 + 1.17) * age - (0.008 + 0.0126) * age^2
plot(y = incNo, x = age, ylim = c(0, 40), type = "l")
lines(y = incH, x = age, col = "red")



# F tests and Chow test

mShort <- lm(wage ~ age + I(age^2), data = dd)

anova(m2, mShort) # F test





################

library(AER)

data("CigarettesSW") 
dd <- CigarettesSW

mOLS <- lm(packs ~ price, data = dd)
summary(mOLS)

mIV1 <- ivreg(packs ~ price | taxs, data = dd)
summary(mIV1)

summary(mIV1, vcov = sandwich, diagnostics = T)


mIV2 <- ivreg(packs ~ price | taxs + I(income/population), data = dd)
summary(mIV2)

summary(mIV2, vcov = sandwich, diagnostics = T)


mIV2 <- ivreg(packs ~ I(price/cpi) | I(tax/cpi) + I(income/population/cpi), data = dd)
summary(mIV2)

summary(mIV2, vcov = sandwich, diagnostics = T)











### Multicollinearity

install.packages("car")
library(car)

vif(m2)      # VIFs are high, but it doesn't matter
summary(m2)


# Principal components

dd2 <- dd[, c("age", "wage", "gender", "educ_higher")]
dd2 <- dd[, c(1, 4, 5, 6)]

pcaVariable <- prcomp(dd2)
pcaVariable

head(pcaVariable$x)


pcaVariable <- prcomp(dd2, center = T, scale. = T)
pcaVariable

plot(x = pcaVariable$x[,1], y = pcaVariable$x[,2])

outliers <- which(pcaVariable$x[,1] > 10)
outliers <- which(pcaVariable$x[,1] > 5 & pcaVariable$x[,2] > 0 & pcaVariable$x[,2] < 1)
outliers <- which(pcaVariable$x[,1] > 4 & pcaVariable$x[,2] > 1)

dd[outliers, ]



### LASSO and Ridge

install.packages("glmnet")
library(glmnet)

x <- as.matrix(dd[, c("age", "gender", "educ_higher")])
y <- dd$wage

mLasso <- glmnet(x = x, y = y, alpha = 1)
mLasso$beta

mRidge <- glmnet(x = x, y = y, alpha = 0)
mRidge$beta



d3 <- mtcars
head(d3)

y <- d3$mpg
x <- d3[, c(2:11)]

mLasso2 <- glmnet(x = x, y = y, alpha = 1)
mLasso2$beta

