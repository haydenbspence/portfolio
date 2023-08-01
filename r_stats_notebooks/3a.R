#Script won't run until you load the hsb2.sav data set and characterize the columns
head(d)
d <- d %>%
  mutate(ses_low = ifelse(ses == "low", 1, 0),
         ses_middle = ifelse(ses == "middle", 1, 0))

#Dummy-manual-model
m1a <- lm(math ~ ses_low + ses_middle, d)
arm::display(m1a, detail = TRUE)

#Cat1a
d <- d %>%
  mutate(ses = factor(ses))

#Cat1b
m1b <- lm(math ~ ses, d)
arm::display(m1b, detail = TRUE)

#Cat2a
d <- d %>%
  mutate(ses = relevel(ses, ref = "low"))

#Cat2b
m1c <- lm(math ~ ses, d)
arm::display(m1c, detail = TRUE)

#See-contrasts
contrasts(d$ses)

#Contr.xxx
contr.sum(2)
contr.sum(5)

#Cat3
contrasts(d$ses) <- contr.sum(3)
m1c1 <- lm(math ~ ses, d)
arm::display(m1c1, detail = TRUE)

#See-ref
contrasts(d$ses)

#All-comp1
d <- d %>%
  mutate(ses_ = factor(ses, 
                       levels = c("low", "middle", "high")))
contrasts(d$ses_)

#All-comp2
contrasts(d$ses_) <- contr.sum(3)
m1c2 <- lm(math ~ ses_, d)

arm::display(m1c1, detail = TRUE)
arm::display(m1c2, detail = TRUE)

#Levs1
contrasts(d$ses)

#Levs2
contrasts(d$ses_)

#Contrast-colnames
d <- d %>%
  mutate(ses = factor(ses))

contrasts(d$ses) <- contr.sum(3)

colnames(contrasts(d$ses)) <- levels(d$ses)[-length(levels(d$ses))] #<<
contrasts(d$ses)

#m1c4
m1c4 <- lm(math ~ ses, d)
arm::display(m1c4, detail = TRUE)

#Pred-group-means1
d %>%
  group_by(ses) %>%
  summarize(mean_math = mean(math))
coef(m1c1)

#Pred-group-means2
coef(m1c1)[1] + coef(m1c1)[2]
coef(m1c1)[1] + coef(m1c1)[3]
coef(m1c1)[1] - coef(m1c1)[2] - coef(m1c1)[3]

#Fact-levels
colors <- factor(c("black", "green", "blue", "blue", "black"))
colors[6] <- "blue"
colors
colors[7] <- "purple"
colors

#m2-dataprep
d <- d %>%
  mutate(race = factor(race),
         prog = factor(prog))
head(d)

#m2-vis1
ggplot(d, aes(race)) +
  geom_bar(alpha = 0.7)

#m2-vis2
ggplot(d, aes(prog)) +
  geom_bar(alpha = 0.7)

#m2-vis-boxplots-race
ggplot(d, aes(race, read)) +
  geom_boxplot()

#m2-vis-boxplots-prog
ggplot(d, aes(prog, read)) +
  geom_boxplot()

#m2-vis-boxplots-facet
ggplot(d, aes(prog, read)) +
  geom_boxplot() +
  facet_wrap(~race) #<<

#m2-vis-boxplots-fill
ggplot(d, aes(prog, read, fill = race)) +
  geom_boxplot() 

#m2a-fit
m2a <- lm(read ~ race + prog, d)
arm::display(m2a, detail = TRUE)

#m2-refchange
d <- d %>%
  mutate(race = relevel(race, ref = "white"),
         prog = relevel(prog, ref = "academic"))

#m2b-fit
m2b <- lm(read ~ race + prog, d)
arm::display(m2b, detail = TRUE)

#m2-scheme-change
contrasts(d$race) <- contr.sum(4)
contrasts(d$prog) <- contr.sum(3)

#contrasts
contrasts(d$race)
contrasts(d$prog)

#m2c-fit
m2c <- lm(read ~ race + prog, d)
arm::display(m2c, detail = TRUE)

#imbalance
mean(d$read)

#sample-mean-recover-fail
group_means <- d %>%
  group_by(race, prog) %>%
  summarize(mean = mean(read))

tail(group_means)
predict(m2c, newdata = data.frame(race = "hispanic", prog = "vocation"))

#mean-group-means
coef(m2c)[1]
mean(group_means$mean)

#interaction-recover-means
m2d <- lm(read ~ race + prog + race:prog, d)
arm::display(m2d, detail = TRUE)

#show-mean-recover
predict(m2d, newdata = data.frame(race = "hispanic", prog = "vocation"))
tail(group_means )

#m3-dataprep
d <- d %>%
  mutate(prog = factor(prog))
contrasts(d$prog)

#m3a-fit
m3a <- lm(math ~ prog + write, d)
arm::display(m3a, detail = TRUE)

#viz-m3a-eval
d <- d %>%
  mutate(pred_m3a = predict(m3a))

ggplot(d, aes(write, math)) +
  geom_point() +
  geom_line(aes(y = pred_m3a, color = prog),
            size = 1.5)

#viz-m3-int-eval
ggplot(d, aes(write, math)) +
  geom_point() +
  geom_smooth(aes(color = prog),
              method = "lm",
              se = FALSE,
              size = 1.5)

#mod-3b-transform
d <- d %>%
  mutate(prog = relevel(prog, ref = "vocation"))

#mod-3b
m3b <- m3a <- lm(math ~ prog + write, d)
arm::display(m3b)

