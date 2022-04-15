## 1
## (a)
library(mice)
help("nhanes")
x <- nhanes
n <- nrow(x)
com <- nrow(cc(x))
(n-com)/n

## (b)
# step 1.
imps <- mice(x, printFlag = FALSE, seed = 1)
# step 2.
fits <- with(imps, lm(bmi ~ age + hyp + chl))
# step 3.
ests <- pool(fits)
# ests
ests
summary(ests)

## (c)
#using the default M=5 but changing the seed
ests_seed2 <- pool(with(mice(x, printFlag = FALSE, seed = 2), lm(bmi ~ age + hyp + chl)))
ests_seed3 <- pool(with(mice(x, printFlag = FALSE, seed = 3), lm(bmi ~ age + hyp + chl)))
ests_seed4 <- pool(with(mice(x, printFlag = FALSE, seed = 4), lm(bmi ~ age + hyp + chl)))
ests_seed5 <- pool(with(mice(x, printFlag = FALSE, seed = 5), lm(bmi ~ age + hyp + chl)))
ests_seed6 <- pool(with(mice(x, printFlag = FALSE, seed = 6), lm(bmi ~ age + hyp + chl)))

ests_seed2
ests_seed3
ests_seed4
ests_seed5
ests_seed6

summary(ests_seed2)
summary(ests_seed3)
summary(ests_seed4)
summary(ests_seed5)
summary(ests_seed6)

## (d)
ests_seed1_100 <- pool(with(mice(x, printFlag = FALSE, seed = 1, m=100), lm(bmi ~ age + hyp + chl)))
ests_seed2_100 <- pool(with(mice(x, printFlag = FALSE, seed = 2, m=100), lm(bmi ~ age + hyp + chl)))
ests_seed3_100 <- pool(with(mice(x, printFlag = FALSE, seed = 3, m=100), lm(bmi ~ age + hyp + chl)))
ests_seed4_100 <- pool(with(mice(x, printFlag = FALSE, seed = 4, m=100), lm(bmi ~ age + hyp + chl)))
ests_seed5_100 <- pool(with(mice(x, printFlag = FALSE, seed = 5, m=100), lm(bmi ~ age + hyp + chl)))
ests_seed6_100 <- pool(with(mice(x, printFlag = FALSE, seed = 6, m=100), lm(bmi ~ age + hyp + chl)))

ests_seed1_100
ests_seed2_100
ests_seed3_100
ests_seed4_100
ests_seed5_100
ests_seed6_100

summary(ests_seed1_100)
summary(ests_seed2_100)
summary(ests_seed3_100)
summary(ests_seed4_100)
summary(ests_seed5_100)
summary(ests_seed6_100)

## 2
setwd("C:/Users/15204/Desktop/IDA/assignment 3")
load("C:/Users/15204/Desktop/IDA/assignment 3/dataex2.Rdata")
dataex2

beta1 <- 3

# stochastic regression imputation, 'norm.nob'
count <- 0 # count the cases fall into 95% confidence interval. 
for(i in 1:100){
  imps_sr <- mice(dataex2[,,i], method = 'norm.nob', printFlag = FALSE, seed = 1, m = 20)
  ests_sr <- pool(with(imps_sr, lm(Y ~ X)))
  interval <- summary(ests_sr,conf.int = TRUE)[, c(7, 8)]
  if ((beta1 >= interval[2,1]) & (beta1 <= interval[2,2])){
    count <- count + 1
    }
   # 97.5%, 2.5%, subtraction, 95% confidence interval.
}
count/100

# bootstrap based version, 'norm.boot'
count <- 0 # count the cases fall into 95% confidence interval. 
for(i in 1:100){
  imps_bs <- mice(dataex2[,,i], method = 'norm.boot', printFlag = FALSE, seed = 1, m = 20)
  ests_bs <- pool(with(imps_bs, lm(Y ~ X)))
  interval <- summary(ests_bs,conf.int = TRUE)[, c(7, 8)]
  if ((beta1 >= interval[2,1]) & (beta1 <= interval[2,2])){
    count <- count + 1
    }
  # 97.5%, 2.5%, subtraction, 95% confidence interval.
}
count/100

## 4
## (a)
setwd("C:/Users/15204/Desktop/IDA/assignment 3")
load("C:/Users/15204/Desktop/IDA/assignment 3/dataex4.Rdata")
imps <- mice(dataex4, printFlag = FALSE, seed = 1, m = 50)
fits <- with(imps, lm(y ~ x1 + x2 + x1*x2))
ests <- pool(fits)
summary(ests, conf.int = TRUE)[, c(7, 8)]
plot(imps, layout = c(2,2))

## (b)
x1x2 <- dataex4$x1*dataex4$x2
datanew <- cbind(dataex4, x1x2)

imp0 <- mice(datanew, maxit = 0)
imp0
meth <- imp0$method
meth["x1x2"] <- "~I(x1*x2)"
pred <- imp0$predictorMatrix
# x1x2 will not be used as predictor of x1 and x2
pred[c("x1", "x2"), "x1x2"] <- 0
pred[, c("x1", "x2")] <- 0
pred["x1", "x2"] <- 1
pred["x2", "x1"] <- 1
pred
visSeq <- imp0$visitSequence
visSeq

imp <- mice(datanew, method = meth, predictorMatrix = pred, 
            m = 50, seed = 1, printFlag = FALSE)
fit <- with(imp, lm(y ~ x1 + x2 + x1x2))
est <- pool(fit)
summary(est, conf.int = TRUE)

## (c)
imps <- mice(datanew, m = 50, seed = 1, printFlag = FALSE)
fits <- with(imps, lm(y ~ x1 + x2 + x1x2))
ests <- pool(fits)
summary(ests, conf.int = TRUE)

## 5
load("C:/Users/15204/Desktop/IDA/assignment 3/NHANES2.Rdata")
setwd("C:/Users/15204/Desktop/IDA/assignment 3")

# inspect the data
dim(NHANES2)
str(NHANES2)
summary(NHANES2)

require(mice)
mdpat_mice <- md.pattern(NHANES2)
mdpat_mice

require(JointAI)
md_pattern(NHANES2, pattern = FALSE, color = c('#34111b', '#e30f41'))

par(mar = c(3, 3, 2, 1), mgp = c(2, 0.6, 0))
plot_all(NHANES2, breaks = 30, ncol = 4)

# imputation procedure
imp0 <- mice(NHANES2, maxit = 0)
imp0

imp <- mice(NHANES2, method = meth, maxit = 20, m = 30, seed = 1, printFlag = FALSE)
imp

imp$loggedEvents

plot(imp, layout = c(4,4))

# continuous variables
densityplot(imp)
densityplot(imp, ~SBP|hypten + gender)
densityplot(imp, ~hgt|gender)

# binary/categorical variables
require(devtools)
require(reshape2)
require(RColorBrewer)
require(ggplot2)
source_url("https://gist.githubusercontent.com/NErler/0d00375da460dd33839b98faeee2fdab/raw/c6f537ecf80eddcefd94992ec7926aa57d454536/propplot.R")

propplot(imp)

xyplot(imp, hgt ~ wgt | gender, pch = c(1, 20))

# model
fit <- with(imp, lm(wgt ~ gender + age + hgt + WC))
summary(fit$analyses[[1]])

comp1 <- complete(imp, 1)
plot(fit$analyses[[1]]$fitted.values, residuals(fit$analyses[[1]]),
     xlab = "Fitted values", ylab = "Residuals")
plot(comp1$wgt ~ comp1$age, xlab = "Age", ylab = "wgt")
plot(comp1$wgt ~ comp1$WC, xlab = "WC", ylab = "wgt")
plot(comp1$wgt ~ comp1$hgt, xlab = "hgt", ylab = "wgt")
boxplot(comp1$wgt ~ comp1$gender, xlab = "Gender", ylab = "wgt")

# QQplot
qqnorm(rstandard(fit$analyses[[1]]), xlim = c(-4, 4), ylim = c(-6, 6))
qqline(rstandard(fit$analyses[[1]]), col = 2)

# pool
pooled_ests <- pool(fit)
summary(pooled_ests, conf.int = TRUE)
pool.r.squared(pooled_ests, adjusted = TRUE)