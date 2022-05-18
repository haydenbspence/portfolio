install.packages("lavaan", dependencies=TRUE)
library(lavaan)

#load the data file
data <- read.table("C:data.dat")

#This data file does not have column (variable) names
#so you have to supply them.
#You can change all the column names at once:
colnames(data) <- c("y1", "y2", "y3", "y4", "x1", "x2", "a1", "a2", "a3", "a4")

#fit the initial growth model
model1 <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
          s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
          '
fit1 <- growth(model1, data=data, missing = "ML")
summary(fit1, standardized=TRUE, fit.measures = TRUE)

#add predictors for a conditional growth model
model2 <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
          s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
          i ~ x1 + x2
          s ~ x1 + x2
          '
fit2 <- growth(model2, data=data, missing = "ML")
summary(fit2, standardized=TRUE, fit.measures = TRUE)

#add time-varying covariates
model3 <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
          s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
          y1 ~ a1
          y2 ~ a2
          y3 ~ a3
          y4 ~ a4
          '
fit3 <- growth(model3, data=data, missing = "ML")
summary(fit3, standardized=TRUE, fit.measures = TRUE)

#bivariate growth model
model4 <- '
          iy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
          sy =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
          ia =~ 1*a1 + 1*a2 + 1*a3 + 1*a4
          sa =~ 0*a1 + 1*a2 + 2*a3 + 3*a4
          iy ~~ sy
          ia ~~ sa
          iy ~~ ia
          sy ~~ sa
          sy ~ ia
          sa ~ iy
          '
fit4 <- growth(model4, data=data, missing = "ML")
summary(fit4, standardized=TRUE, fit.measures = TRUE, modindices = TRUE)

model4a <- '
          iy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
          sy =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
          ia =~ 1*a1 + 1*a2 + 1*a3 + 1*a4
          sa =~ 0*a1 + 1*a2 + 2*a3 + 3*a4
          iy ~~ sy
          ia ~~ sa
          iy ~~ ia
          sy ~~ sa
          sy ~ ia
          sa ~ iy
          y3 ~~ a3
          '
fit4a <- growth(model4a, data=data, missing = "ML")
summary(fit4a, standardized=TRUE, fit.measures = TRUE, modindices = TRUE)
anova(fit4, fit4a)


library(foreign)
families <- read.spss("C:/data2.sav", to.data.frame=TRUE)


#quadratic curve
#load the data file
ex6.11 <- read.table("C:/data3.dat")
colnames(ex6.11) <- c("y1", "y2", "y3", "y4", "y5")

model5 <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
          s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4 + 4*y5
          '
fit5 <- growth(model5, data=ex6.11, missing = "ML")
summary(fit5, standardized=TRUE, fit.measures = TRUE)

model5a <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
          s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4 + 4*y5
          q =~ 0*y1 + 1*y2 + 4*y3 + 9*y4 + 16*y5
'
fit5a <- growth(model5a, data=ex6.11, missing = "ML")
summary(fit5a, standardized=TRUE, fit.measures = TRUE)
anova(fit5,fit5a)

model5b <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
          s =~ -2*y1 + -1*y2 + 0*y3 + 1*y4 + 2*y5
          q =~ 4*y1 + 1*y2 + 0*y3 + 1*y4 + 4*y5
'
fit5b <- growth(model5b, data=ex6.11, missing = "ML")
summary(fit5b, standardized=TRUE, fit.measures = TRUE)


#piecewise growth model
model6 <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
          s1 =~ 0*y1 + 1*y2 + 2*y3 + 2*y4 + 2*y5
          s2 =~ 0*y1 + 0*y2 + 0*y3 + 1*y4 + 2*y5
          '
fit6 <- growth(model6, data=ex6.11, missing = "ML")
summary(fit6, standardized=TRUE, fit.measures = TRUE)

model6a <- '
          i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
          s1 =~ 0*y1 + 1*y2 + 2*y3 + 2*y4 + 2*y5
          s2 =~ 0*y1 + 0*y2 + 0*y3 + 1*y4 + 2*y5
          s1 ~ a*1
          s2 ~ a*1
          '
fit6a <- growth(model6a, data=ex6.11, missing = "ML")
summary(fit6a, standardized=TRUE, fit.measures = TRUE)
anova(fit6,fit6a)


#moderation examples
model7 <- '
          i =~ 1*devp1 + 1*devp2 + 1*devp3 + 1*devp4
          s =~ 0*devp1 + 1*devp2 + 2*devp3 + 3*devp4
          '
fit7a <- growth(model7, data=families, missing = "ML", group = "gender")
summary(fit7a, standardized=TRUE, fit.measures = TRUE)

fit7b <- growth(model7, data=families, missing = "ML", group = "gender", group.equal = "means")
summary(fit7b, standardized=TRUE, fit.measures = TRUE)

anova(fit7a, fit7b)

model8 <- '
          i =~ 1*mon1 + 1*mon2 + 1*mon3
          s =~ 0*mon1 + 1*mon2 + 2*mon3
          '
fit8a <- growth(model8, data=families, missing = "ML", group = "gender")
summary(fit8a, standardized=TRUE, fit.measures = TRUE)

fit8b <- growth(model8, data=families, missing = "ML", group = "gender", group.equal = "means")
summary(fit8b, standardized=TRUE, fit.measures = TRUE)

anova(fit8a, fit8b)
