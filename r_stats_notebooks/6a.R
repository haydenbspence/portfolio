## ----setup, include = FALSE----------------------------------------------
library(here)
library(tidyverse)
library(rio)

theme_set(theme_minimal(base_size = 25))

knitr::opts_chunk$set(fig.width = 13, 
                      message = FALSE, 
                      warning = FALSE)
options(pillar.sigfig = 7)

d <- import(here("data", "glo_sim.sav"),
            setclass = "tbl_df") %>%
      characterize()


## ----car-t-echo, eval = FALSE--------------------------------------------
## library(car) #new library #<<
## m1 <- lm(sex_risk ~ internalizing + externalizing + aces, d)
## qqPlot(m1)

## ----car-t-eval, echo = FALSE, fig.height = 6----------------------------
m1 <- lm(sex_risk ~ internalizing + externalizing + aces, d)
car::qqPlot(m1)

## ----outlier-test-echo, eval = FALSE-------------------------------------
## outlierTest(m1)

## ----outlier-test, echo = FALSE------------------------------------------
car::outlierTest(m1)

## ----dfbeta--------------------------------------------------------------
m1_dfb <- dfbeta(m1)
head(m1_dfb)

## ------------------------------------------------------------------------
m1_dfb <- as.data.frame(m1_dfb) %>%
  janitor::clean_names() %>%
  mutate(id = d$id) 

## ----dfbeta-arrange1-----------------------------------------------------
m1_dfb %>% 
  arrange(desc(intercept)) %>% # Arrange by intercept
  head()

## ----dfbeta-arrange2-----------------------------------------------------
m1_dfb %>%
  arrange(desc(internalizing)) %>% # Arrange by internalizing
  head()

## ----plot, echo = FALSE, message = FALSE, warning = FALSE----------------
ggplot(mpg, aes(displ, cty)) +
  geom_point(color = "gray70", 
             size = 2) +
  geom_smooth(aes(color = trans),
              method = "lm", 
              se = FALSE,
              lwd = 2) +
  scale_color_brewer(palette = "Set2") +
  guides(color = "none") +
  labs(x = "", 
       y = "") +
  ylim(10, 25)

## ----zero-order-corrs----------------------------------------------------
d %>%
  select(sex_risk, internalizing, externalizing, aces) %>%
  cor()

## ----pairs-plot-echo, eval = FALSE---------------------------------------
## d %>%
##   select(sex_risk, internalizing, externalizing, aces) %>%
##   pairs()

## ----pairs-plot-eval, echo = FALSE, fig.height = 9-----------------------
d %>%
  select(sex_risk, internalizing, externalizing, aces) %>%
  pairs()

## ----fit-m1--------------------------------------------------------------
m1 <- lm(sex_risk ~ internalizing + externalizing + aces, d)

## ----interpret-m1, highlight.output = 20:21------------------------------
summary(m1, detail = TRUE)

## ----lmSupport, highlight.output = 5-------------------------------------
# install.packages("lmSupport")
library(lmSupport)
modelEffectSizes(m1)

## ----pred----------------------------------------------------------------
x_axis <- seq(from = 0, to = 45, by = .1)

pred_frame <- data.frame(internalizing = 0, 
                         externalizing = rep(x_axis, 3),
                         aces = rep(c(0, 3, 6), 
                                    each = length(x_axis))) 
  
pred_frame <- pred_frame %>%
  mutate(pred = predict(m1, newdata = pred_frame),
         aces = as.factor(aces))  # Why change to factor?

## ----show-pred-data------------------------------------------------------
head(pred_frame)
tail(pred_frame)

## ----pred-plot1, fig.height = 5------------------------------------------
ggplot(d, aes(externalizing, sex_risk)) +
  geom_point() 

## ----pred-plot2, fig.height = 4.5----------------------------------------
ggplot(d, aes(externalizing, sex_risk)) +
  geom_point() +
  geom_line(aes(y = pred, color = aces), #<<
            data = pred_frame) #<<

## ----tidy----------------------------------------------------------------
library(broom)
tidy_pd <-  tidy(m1, conf.int = TRUE)
tidy_pd  

## ----coefplot, fig.height = 4.5------------------------------------------
ggplot(tidy_pd, aes(term, estimate)) +
  geom_hline(yintercept = 0, 
             color = "cornflowerblue",
             size = 2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high)) +
  coord_flip()

## ----y-noexternalizing---------------------------------------------------
y_noex <- lm(sex_risk ~ internalizing + aces, d)

## ----y-resids------------------------------------------------------------
y_res <- residuals(y_noex)

## ----externalizing-mod---------------------------------------------------
ex_mod <- lm(externalizing ~ internalizing + aces, d)

## ----externalizing-resids------------------------------------------------
ex_res <- residuals(ex_mod)

## ----partial-------------------------------------------------------------
cor(y_res, ex_res)

## ----semi-partial--------------------------------------------------------
cor(d$sex_risk, ex_res)

## ----squared-corrs-------------------------------------------------------
cor(y_res, ex_res)^2
cor(d$sex_risk, ex_res)^2

## ----es------------------------------------------------------------------
modelEffectSizes(m1)

## ----fit-two-mods--------------------------------------------------------
reduced_model <- lm(sex_risk ~ internalizing + aces, d)
full_model <- lm(sex_risk ~ internalizing + externalizing + aces, d)

## ----display-mods--------------------------------------------------------
arm::display(full_model)
arm::display(reduced_model)

