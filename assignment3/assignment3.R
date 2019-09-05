
#@ title: Assignment 3
#@ author: Tzu
#@ date: 2019/4/4


# import packages
library(tidyverse)
library(dplyr); library(tidyr); library(rstan); library(ggplot2); library(ggthemes); library(gridExtra)
theme_set(theme_tufte(base_family = 'sans'))

# load the data
data('foxes', package = 'rethinking')
d  <- foxes; rm(foxes)
# take a look
str(d)


# Q1

# (1) model1 weight~area
m3_1 <- "
  data {
    int<lower=0> N;
    vector[N] weight_;
    vector[N] area_;
  }
  parameters {
    real a;
    real bA;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + area_ * bA;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bA ~ normal(0, 10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  area_ = d$area
)

fitm3_1 <- stan(model_code = m3_1, data = dat, cores = 2, chains = 2, iter = 1000)
print(fitm3_1, probs = c(0.10, 0.5, 0.9))


# (2) model2 weight~groupsize
m3_2 <- "
  data {
    int<lower=0> N;
    vector[N] weight_;
    vector[N] group_size;
  }
  parameters {
    real a;
    real bG;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + group_size * bG;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bG ~ normal(0, 10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  group_size = d$groupsize
)

fitm3_2 <- stan(model_code = m3_2, data = dat, cores = 2, chains = 2, iter = 1000)
print(fitm3_2, probs = c(0.10, 0.5, 0.9))


# plot model1
post <- as.data.frame(fitm3_1)
f_mu <- function(x) post$a + post$bA * x
area_new <- seq(1, 6)

mu <- 
  sapply(area_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) area_new) %>%
  mutate(Iter = row_number()) %>%
  gather(area_, weight_, -Iter) %>%
  group_by(area_) %>%
  mutate(pi_l = rethinking::PI(weight_, prob = 0.95)[1],
         pi_h = rethinking::PI(weight_, prob = 0.95)[2]) %>%
  mutate(mu = mean(weight_)) %>%
  ungroup() %>%
  mutate(area_ = as.numeric(area_))

p <- ggplot() 
p1 <- p + 
  geom_point(data = d,
             aes(area, weight), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = area_, ymin = pi_l, ymax = pi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = area_, y = mu))

# plot model2 
post <- as.data.frame(fitm3_2)

f_mu <- function(x) post$a + post$bG * x
groupsize_new <- seq(2, 8)
mu <- 
  sapply(groupsize_new, f_mu) %>%
  as_tibble() %>%
  rename_all(function(x) groupsize_new) %>%
  mutate(Iter = row_number()) %>%
  gather(group_size, weight_, -Iter) %>%
  group_by(group_size) %>%
  mutate(pi_l = rethinking::PI(weight_, prob = 0.95)[1],
         pi_h = rethinking::PI(weight_, prob = 0.95)[2]) %>%
  mutate(mu = mean(weight_)) %>%
  ungroup() %>%
  mutate(group_size = as.numeric(group_size))

p2 <- p + 
  geom_point(data = d,
             aes(groupsize, weight), 
             shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = mu,
              aes(x = group_size, ymin = pi_l, ymax = pi_h), alpha = .1) +
  geom_line(data = mu,
            aes(x = group_size, y = mu))

# plot together
grid.arrange(p1, p2, nrow = 1)


# Q2
# multiple regression model
m3_3 <- "
  data {
    int<lower=0> N;
    vector[N] weight_;
    vector[N] area_;
    vector[N] group_size;
  }
  parameters {
    real a;
    real bA;
    real bG;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + area_ * bA + group_size * bG;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bA ~ normal(0, 10);
    bG ~ normal(0, 10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  area_ = d$area,
  group_size = d$groupsize
)

fitm3_3 <- stan(model_code = m3_3, data = dat, cores = 2, chains = 2, iter = 1000)
print(fitm3_3, probs = c(0.10, 0.5, 0.9))


# create counterfatual plots of each variable
post <- as.matrix(fitm3_3)

# setup the dataframe
nd1 <- 
  expand.grid(area_ = seq(1, 6), 
              group_size = seq(2, 8)) %>% 
  as.matrix

mu <- post[,1] + post[,2:3] %*% t(nd1)

avg <- colMeans(mu)
hdi <- apply(mu, 2, HDInterval::hdi)

# simulate the data of weight 
iter <- 1e4
y_hat <- matrix(nrow = iter, ncol = NROW(nd1))
for(i in 1:NROW(nd1)) y_hat[,i] <- rnorm(iter, post[,1] + post[,2:3] %*% as.matrix(nd1[i,]), post[,4])

y_hat_avg <- colMeans(y_hat)
y_hat_hdi <- apply(y_hat, 2, HDInterval::hdi)

nd1 <- 
  as_tibble(nd1) %>%
  bind_cols(avg = avg, 
            mu_hdi_l = hdi[1,], 
            mu_hdi_h = hdi[2,],
            y_hdi_l = y_hat_hdi[1,],
            y_hdi_h = y_hat_hdi[2,])

# plot for area
p1 <- ggplot(nd1, aes(x = area_, y = avg, group = group_size)) + 
  geom_line(data = nd1 %>% filter(group_size==5), color = 'black') +
  geom_ribbon(data = nd1 %>% filter(group_size==5),
              aes(x = area_, ymin = mu_hdi_l, ymax = mu_hdi_h), alpha = .1) +
  geom_ribbon(data =  nd1 %>% filter(group_size==5),
              aes(x = area_, ymin = y_hdi_l, ymax = y_hdi_h), alpha = .1) +
  labs(x = 'area', y = 'weight') 


# plot for groupsize
p2 <- ggplot(nd1, aes(x = group_size, y = avg, group = area_)) + 
  geom_line(data = nd1 %>% filter(area_==3), color = 'black') +
  geom_ribbon(data = nd1 %>% filter(area_==3),
              aes(x = group_size, ymin = mu_hdi_l, ymax = mu_hdi_h), alpha = .1) +
  geom_ribbon(data =  nd1 %>% filter(area_==3),
              aes(x = group_size, ymin = y_hdi_l, ymax = y_hdi_h), alpha = .1) +
  labs(x = 'groupsize', y = 'weight') 

grid.arrange(p1, p2, nrow = 1)


# Q3
# create model weight~avgfood+groupsize
m3_4 <- "
  data {
  int<lower=0> N;
  vector[N] weight_;
  vector[N] avg_food;
  vector[N] group_size;
  }
  parameters {
    real a;
    real bAV;
    real bG;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + avg_food * bAV + group_size * bG;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bAV ~ normal(0, 10);
    bG ~ normal(0, 10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  avg_food = d$avgfood,
  group_size = d$groupsize
)

fitm3_4 <- stan(model_code = m3_4, data = dat, cores = 2, chains = 2, iter = 1000)
print(fitm3_4, probs = c(0.10, 0.5, 0.9))

# create model weight~avgfood+groupsize+area
m3_5 <- "
  data {
    int<lower=0> N;
    vector[N] weight_;
    vector[N] avg_food;
    vector[N] group_size;
    vector[N] area_;
  }
  parameters {
    real a;
    real bAV;
    real bG;
    real bAR;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + avg_food * bAV + group_size * bG + area_ * bAR;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bAV ~ normal(0, 10);
    bG ~ normal(0, 10);
    bAR ~ normal(0,10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  avg_food = d$avgfood,
  group_size = d$groupsize,
  area_ = d$area
)

fitm3_5 <- stan(model_code = m3_5, data = dat, cores = 2, chains = 2, iter = 1000)
print(fitm3_5, probs = c(0.10, 0.5, 0.9))


# standardize
d$avgfood.s <- (d$avgfood - mean(d$avgfood)) / sd(d$avgfood)
d$area.s <- (d$area - mean(d$area)) / sd(d$area)

# create bivariate model by area and avgfood to compare the standard deviation
# model3.6 weight~avgfood.s+groupsize
m3_6 <- "
  data {
    int<lower=0> N;
    vector[N] weight_;
    vector[N] avg_food_s;
    vector[N] group_size;
  }
  parameters {
    real a;
    real bAV;
    real bG;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + avg_food_s * bAV + group_size * bG;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bAV ~ normal(0, 10);
    bG ~ normal(0, 10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  avg_food_s = d$avgfood.s,
  group_size = d$groupsize
)

fitm3_6 <- stan(model_code = m3_6, data = dat, cores = 2, chains = 2, iter = 1000)

# model3.7 weight~ area.s+groupsize
m3_7 <- "
  data {
    int<lower=0> N;
    vector[N] weight_;
    vector[N] area_s;
    vector[N] group_size;
  }
  parameters {
    real a;
    real bAV;
    real bG;
    real<lower=0, upper=10> sigma;
  }
  model {
    vector[N] mu = a + area_s * bAV + group_size * bG;
    weight_ ~ normal(mu, sigma);
    a ~ normal(10, 10);
    bAV ~ normal(0, 10);
    bG ~ normal(0, 10);
  }"

dat <- list(
  N = NROW(d),
  weight_ = d$weight,
  area_s = d$area.s,
  group_size = d$groupsize
)

fitm3_7 <- stan(model_code = m3_7, data = dat, cores = 2, chains = 2, iter = 1000)

print(fitm3_6)
print(fitm3_7)
