

## install.packages("pwr")
library(pwr)


## Conduct a power analysis to solve for SAMPLE SIZE needed for an RCT 
### Note that we provide effect size and power, therefore n is estimated 
 
pwr.t.test(d=0.56, power=0.80, type="two.sample", alternative="two.sided")
 

## lot the power curve 
### Assign results to an object
 
pow.1<-pwr.t.test(d=0.43, power=0.80, type="two.sample", alternative="two.sided")
 
## plot the results from object
 
plot(pow.1)
plot(pow.1, xlab="Sample Size per group")



## Conduct a power analysis to solve for EFFECT SIZE for an RCT 
 
pow.2 <- pwr.t.test(power=0.60, n=75, type="two.sample", alternative="two.sided")

plot(pow.2)
plot(pow.2, xlab="Sample Size per group")


## Conduct a power analysis to solve for POWER for an RCT 

pow.3 <- pwr.t.test(d = .42, n=150, type="two.sample", alternative="two.sided")

plot(pow.3)
plot(pow.3, xlab="Sample Size per group")



pwr.anova.test(f=0.28,k=4,power =0.80, sig.level=0.05)



## solve for Power given that we provide sample size, groups, and effect size

pwr.anova.test(f=0.30,k=4,n=45, sig.level=0.05)



## solve for Power given that we provide sample size, groups, and effect size

pwr.anova.test(power=.80,k=4,n=45, sig.level=0.05)



## plot our three respective power analyses

pow.4<-pwr.anova.test(f=0.28,k=4,power =0.80, sig.level=0.05)
plot(pow.4)


## solve for Power given that we provide sample size, groups, and effect size

pow.5<-pwr.anova.test(f=0.30,k=4,n=45, sig.level=0.05)
plot(pow.5)


## solve for Power given that we provide sample size, groups, and effect size

pow.6<-pwr.anova.test(power=.80,k=4,n=45, sig.level=0.05)
plot(pow.6)
 
 