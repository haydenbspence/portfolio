## ----setup, 
library(here)
library(tidyverse)
library(rio)
theme_set(theme_minimal()) # Sets the plotting theme (no big deal)

d <- import(here(),
            setclass = "tbl_df") %>%
      characterize()

## ----
x <- 50:200
var <- 5 + x/1.5
sim2_df <- map2_df(x, var, 
                   ~data.frame(sim = rnorm(10, .x, .y), 
                               x   = .x)) 

ggplot(sim2_df, aes(x, sim)) +
    geom_point() +
    geom_smooth(method = "lm",
                color = "magenta")

## ----relevel
d <- d %>%
  mutate(prog = factor(prog),
         prog = relevel(prog, ref = "academic"))

## ----fit-mod
m1 <- lm(read ~ write + prog, d)

## ----m1-summary
arm::display(m1, detail = TRUE)

## ----sim-nonlin----------------------------------------------------------
sim_data <- import(here("data", "simdata.csv"),
                   setclass = "tbl_df")
sim_data

## ----lin
ggplot(sim_data, aes(x, y)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm",
              color = "magenta")

## ----m2
m2 <- lm(math ~ write + read, d)

## ----linearity-eval1
ggplot(d, aes(write, math)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm",
              color = "magenta")

## ----linearity-eval2
ggplot(d, aes(read, math)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method = "lm",
              color = "magenta")


## ----extract-resids
resids <- residuals(m1)

## ----fitted
fitted_vals <- fitted(m1)

## ----diag-plot-df
m1_pd <- data_frame(fitted = fitted_vals,
                    residuals = resids)

## ----diag-plot1
ggplot(m1_pd, aes(fitted, residuals)) +
  geom_point() +
  geom_smooth()

## ----pd
pd <- fortify(m1)
head(pd)

## ----diag-plot2
ggplot(pd, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth()

## ----resid-inspect2
sim_mod <- lm(y ~ x, sim_data)

ggplot(sim_mod, aes(.fitted, .resid)) +
 geom_point() +
 geom_smooth()


## ----correct-curvelin
sim_mod2 <- lm(y ~ x + I(x^2), sim_data)

ggplot(sim_mod2, aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth() 

## ----diag-auto
plot(sim_mod2, which = 1)

## ----hist1
ggplot(d, aes(read)) +
  geom_histogram(alpha = 0.7)

## ----hist2
ggplot(d, aes(write)) +
  geom_histogram(alpha = 0.7)

## ----hist3
ggplot(d, aes(math)) +
  geom_histogram(alpha = 0.7)


## ----ggscatmat
# Uncomment the below and run it if you don't have it installed
# install.packages(GGally)
library(GGally)
ggscatmat(d, 
          columns = c("math", 
                      "read", 
                      "write"))

## ----ggplot-qq
ggplot(m2, aes(sample = .stdresid)) +
  geom_qq(color = "gray60") +
  stat_qq_line()

## ----base-qq
plot(m2, which = 2)


## ----leverage-plot
d <- d %>%
  mutate(leverage = hatvalues(m2))

ggplot(d, aes(id, leverage)) +
  geom_label(aes(label = id))

## ----remove103
m2_no103 <- lm(math ~ write + read, d,
               subset = id != 103)
coef(m2_no103)
coef(m2)

## ----cook-d-plot
d <- d %>%
  mutate(cook_d = cooks.distance(m2))

ggplot(d, aes(id, cook_d)) +
  geom_label(aes(label = id))

## ----remove-cd
m2_rem_cd <- lm(math ~ write + read, d,
               subset = id != 126 & id != 167)
coef(m2_rem_cd)
coef(m2)

## ----cd-read
ggplot(d, aes(read, math)) +
  geom_point(size = 4) +
  geom_label(data = filter(d, id == 126 | id == 167),
             aes(label = id))

## ----cd-write
ggplot(d, aes(write, math)) +
  geom_point(size = 4) +
  geom_label(data = filter(d, id == 126 | id == 167),
             aes(label = id))

## ----visreg
library(visreg) 

par(mfrow = c(2, 2))
visreg(m2)
visreg(m2_rem_cd)

## ----leverage-distance
ggplot(m1, aes(.hat, .cooksd)) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "gray80") +
  geom_smooth(se = FALSE) +
  geom_point()

