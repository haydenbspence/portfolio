 


library(haven)
FTC <- read_sav("data.sav")
View(FTC)
attach(FTC)


library(dplyr)
group_by(FTC, ThreeGroup) %>%
        summarise(
                count = n(),
                mean = mean(HARSH3, na.rm = TRUE),
                sd = sd(HARSH3, na.rm = TRUE)
                )

library(ggplot2)
# Box plot

FTC$ThreeGroup<-as.factor(FTC$ThreeGroup)
FTC$BOY<-as.factor(FTC$BOY)



plot <- ggplot(FTC, aes(ThreeGroup, HARSH3)) 

plot + geom_boxplot()

plot + geom_boxplot() +
     ggtitle("Harsh Parenting Post Test by Group") +
     xlab("Groups") + ylab("Harsh Parenting") 
      
plot + geom_boxplot(aes(group=ThreeGroup, fill = ThreeGroup)) +
        theme_minimal() + 
        theme(legend.position = "top")



ggplot((data=subset(FTC, !is.na(ThreeGroup))), aes(ThreeGroup, HARSH3)) + 
        geom_boxplot(aes(group=ThreeGroup, fill = ThreeGroup), notch=TRUE) +
        theme_minimal() + 
        theme(legend.position = "top")

plot2<-ggplot((data=subset(FTC, !is.na(ThreeGroup))), aes(ThreeGroup, HARSH3)) + 
        geom_boxplot(aes(group=ThreeGroup, fill = ThreeGroup), notch=TRUE) +
        theme_minimal() + 
        theme(legend.position = "top")


plot2 + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)

plot + geom_boxplot(aes(colour = BOY))


plot3<-ggplot((data=subset(FTC, !is.na(ThreeGroup))), aes(ThreeGroup, HARSH3)) + 
        theme_minimal() + 
        theme(legend.position = "top") +
        geom_boxplot(aes(fill=BOY))
plot3

 

# Conduct Three Group analysis of variance ANOVA
ANOVA.mod <- aov(HARSH3~ThreeGroup, data = FTC)

# Conduct Pairwise Comparisons using Tukey's Honest Significant Differences
TukeyHSD(ANOVA.mod)

# Install multcomp package
# Use glht() function to conduct multiple pairwise-comparisons for a one-way ANOVA:
library(multcomp)
summary(glht(ANOVA.mod, linfct = mcp(ThreeGroup = "Tukey")))




# 1. Plot the variances from the ANOVA model, i.e., visualize Homogeneity of variances
plot(ANOVA.mod, 1)

# 2. Conduct a formal Levene Test on homogeneity of variance 
library(car)
leveneTest(HARSH3~ThreeGroup, data = FTC)







