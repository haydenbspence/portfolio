library(here)
library(tidyverse)
library(rio)
theme_set(theme_minimal()))

# Fill in the below to load the data
d <- import(here( , ),
            setclass = "tbl_df") %>%
      characterize()


# fit-m1
m1 <- lm(math ~ read, d)

# display-m1
arm::display(m1, detail = TRUE)

# calc-dev
mean(d$read)
ss <- d %>%
  select(read)

ss %>%
  mutate(dev = read - mean(read)) %>% 
  head()

# square-dev
  mutate(dev = read - mean(read),
         dev2 = dev^2) %>% 
  head()

# sum-squared-dev
  mutate(dev = read - mean(read),
         dev2 = dev^2,
         sum_squares = sum(dev2)) %>% 
  head()

# denom
ss %>%
  mutate(dev = read - mean(read),
         dev2 = dev^2,
         sum_squares = sum(dev2),
         denom = sqrt(sum_squares)) %>% 
  head()

## ----se1-----------------------------------------------------------------
7.04 / 144.6355

arm::display(m1, digits = 4, detail = TRUE)

## ----m2-fit--------------------------------------------------------------
m2 <- lm(math ~ read + write, d)
arm::display(m2, detail = TRUE, digits = 4)

## ----verify-se2----------------------------------------------------------
arm::display(m2, digits = 4, detail = TRUE)

# Compute correlation
x1x2_cor <- cor(d$read, d$write)

# Compute sums-of squares for reading (in a single line of code)
ss_read <- sum((d$read - mean(d$read))^2)

6.5553 / sqrt(ss_read*(1 - x1x2_cor^2))

# display-m2
arm::display(m2, digits = 4, detail = TRUE)

# approximate-cis
12.8651 - 2*2.8216
12.8651 + 2*2.8216

# exact-cis1
confint(m2)

# ci1
12.8651 + qt(0.05/2, 197)*2.8216
12.8651 - qt(0.05/2, 197)*2.8216

# exact-cis
confint(m2)

# ci2
confint(m2)

# ci3
coef(m2)[2] + qt(0.025, m2$df.residual) * summary(m2)$coefficients[2, 2]
coef(m2)[2] + qt(0.975, m2$df.residual) * summary(m2)$coefficients[2, 2]


# m3
d <- d %>%
  mutate(ses = factor(ses))

m3 <- lm(read ~ ses + math, d)
arm::display(m3, detail = TRUE)
confint(m3)

