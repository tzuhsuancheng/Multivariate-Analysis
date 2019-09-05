
#import library
library(rethinking)
library(tidyverse)
library(rstan)
library(ggplot2)


## ----eval=FALSE----------------------------------------------------------
## #import library
## library(rethinking)
## library(tidyverse)
## library(rstan)
## library(ggplot2)

## ----message=FALSE-------------------------------------------------------
#import the data
data(Howell1)
d <- Howell1

## ------------------------------------------------------------------------
d %>% 
  ggplot() +
  geom_histogram(aes(height))

## ------------------------------------------------------------------------
# create Rstan model
m01 <- "
  data {
    int<lower=1> N;
    int<lower=1> N2;
    vector[N] height;
    vector[N] weight;
    vector[N2] weight_test;
  }
  parameters {
    real alpha;
    real beta;
    real<lower=0,upper=50> sigma;
  }
  model {
    vector[N] mu = alpha + beta * weight;

    // prior
    alpha ~ normal(178, 20); 
    beta ~ lognormal(0,1);
    
    //likelihood
    height ~ normal(mu, sigma);
  }
  generated quantities{
    real pred_y[N2];
    vector[N2] mu = alpha + beta * weight_test;

    pred_y = normal_rng(mu, sigma);
  }

"

weight_test = c(46.95, 43.72, 64.78, 32.59, 54.63)

dat <- list(N = NROW(d),
            height = d$height,
            weight = d$weight,
            N2 = length(weight_test),
            weight_test = weight_test)

fit01 <- stan(model_code = m01, data = dat, cores = 2, chains = 2, iter = 1000)
fit01.post = as.data.frame(fit01)

## ------------------------------------------------------------------------
# table
pred_y = fit01.post %>% 
  select(contains("pred_y"))

answer.sheet = data.frame(
  individual = seq(1:5),
  weight = weight_test,
  `expected height` = pred_y %>% apply(., 2, mean), # by column
  L_HPDI = pred_y %>% apply(.,2,HPDI) %>% .[1,],
  H_HPDI = pred_y %>% apply(.,2,HPDI) %>% .[2,])

answer.sheet

## ------------------------------------------------------------------------
d2 <- Howell1[Howell1$age < 18, ]
nrow(d2)

## ------------------------------------------------------------------------
d2 %>% 
  ggplot() +
  geom_histogram(aes(height))

## ------------------------------------------------------------------------
m02 <- "
data {
int<lower=1> N;
vector[N] height;
vector[N] weight;
}
parameters {
real alpha;
real beta;
real<lower=0,upper=60> sigma;
}
model {
vector[N] mu = alpha + beta * weight;

height ~ normal(mu, sigma);
alpha ~ normal(178, 20); 
beta ~ lognormal(0,1);

}
"

# reorganize d data
dat <- list(N = NROW(d2),
            height = d2$height,
            weight = d2$weight)
#sampling
fit02 <- stan(model_code = m02, data = dat, cores = 2, chains = 2, iter = 1000)

# summarize
print(fit02)


## ------------------------------------------------------------------------
m02.1 <- "
  data {
    int<lower=1> N;
    vector[N] height;
    vector[N] weight;
  }
  parameters {
    real alpha;
    real beta;
    real<lower=0,upper=60> sigma;
  }
  model {
    vector[N] mu = alpha + beta * weight;
    
    height ~ normal(mu, sigma);
    alpha ~ normal(178, 20); 
    beta ~ lognormal(0, 1);
  }
  generated quantities {
    real y_pred[N];
    vector[N] mu;

    mu = alpha + beta * weight;
    y_pred = normal_rng(mu, sigma);
  }
"

# reorganize data
dat <- list(N = NROW(d2),
            height = d2$height,
            weight = d2$weight)
#sampling
fit02.1 <- stan(model_code = m02.1, data = dat, cores = 2, chains = 2, iter = 1000)
post.fit02.1 = as.data.frame(fit02.1)

## ------------------------------------------------------------------------
pred_mu = post.fit02.1 %>% 
  select(contains("mu"))

CI = data.frame(
  mean = pred_mu %>% apply(., 2, mean),
  L_HPDI = pred_mu %>% apply(.,2,HPDI) %>% .[1,],
  H_HPDI = pred_mu %>% apply(.,2,HPDI) %>% .[2,],
  weight = d2$weight)

CI %>% 
  ggplot() +
  geom_point(data = d2, aes(weight, height), color="blue", alpha=.3) +
  geom_line(aes(weight, mean)) +
  geom_ribbon(aes(x=weight,ymin=L_HPDI, ymax=H_HPDI), alpha=.3) +
  ggtitle("89% Confidence Interval")


## ------------------------------------------------------------------------
pred_y = post.fit02.1 %>% 
  select(contains("y_pred"))

PI = data.frame(
  mean = pred_y %>% apply(., 2, mean),
  L_HPDI = pred_y %>% apply(.,2,HPDI) %>% .[1,],
  H_HPDI = pred_y %>% apply(.,2,HPDI) %>% .[2,],
  weight = d2$weight)
  
PI %>% 
  ggplot() +
  geom_point(data = d2, aes(weight, height), color="blue", alpha=.3) +
  geom_line(aes(weight, mean)) +
  geom_ribbon(aes(x=weight,ymin=L_HPDI, ymax=H_HPDI), alpha=.3) +
  ggtitle("89% Prediction Interval")

## ------------------------------------------------------------------------
ggplot()+
  geom_point(data = d2, aes(weight, height), color="blue", alpha=.3) +
  geom_line(data=CI, aes(weight, mean)) +
  geom_ribbon(data=CI, aes(x=weight, ymin=L_HPDI, ymax=H_HPDI), alpha=.5) +
  geom_ribbon(data=PI, aes(x=weight,ymin=L_HPDI, ymax=H_HPDI), alpha=.2)

## ------------------------------------------------------------------------
d <- Howell1
d$weight_log = log(d$weight)

## ------------------------------------------------------------------------
m03 <- "
  data {
    int<lower=1> N;
    vector[N] height;
    vector[N] weight_log;
  }
  parameters {
    real alpha;
    real beta;
    real<lower=0,upper=50> sigma;
  }
  model {
    vector[N] mu = alpha + beta * weight_log;
    
    height ~ normal(mu, sigma);
    alpha ~ normal(178,100); 
    beta ~ normal(0,100);
  }
  generated quantities {
    real y_pred[N];
    vector[N] mu;
    
    mu = alpha + beta * weight_log;
    y_pred = normal_rng(mu, sigma);
  }
"

# reorganize d data
dat <- list(N = NROW(d),
            height = d$height,
            weight_log = d$weight_log)
#sampling
fit03 <- stan(model_code = m03, data = dat, cores = 2, chains = 2, iter = 1000)
post.fit03 = as.data.frame(fit03)

# summarize
summary(fit03, pars=c("alpha","beta", "sigma"))[[1]] %>% 
  as.data.frame() %>% 
  select(mean, sd)

## ------------------------------------------------------------------------
plot(height~weight, data=Howell1, col=col.alpha(rangi2, 0.4))

## ------------------------------------------------------------------------
pred_y = post.fit03 %>% 
  select(contains("y_pred"))

pred_y = data.frame(
  mean = pred_y %>% apply(., 2, mean),
  weight = d$weight)

pred_y %>% 
  ggplot() +
  geom_point(data = d, aes(weight, height), color="blue", alpha=.3) +
  geom_line(aes(weight, mean)) 


## ------------------------------------------------------------------------
pred_mu = post.fit03 %>% 
  select(contains("mu"))

CI = data.frame(
  mean = pred_mu %>% apply(., 2, mean),
  L_HPDI = pred_mu %>% apply(., 2, HPDI, prob = 0.97) %>% .[1,],
  H_HPDI = pred_mu %>% apply(., 2, HPDI, prob = 0.97) %>% .[2,],
  weight = d$weight)

CI %>% 
  ggplot() +
  geom_point(data = d, aes(weight, height), color="blue", alpha=.3) +
  geom_line(aes(weight, mean)) +
  geom_ribbon(aes(x=weight,ymin=L_HPDI, ymax=H_HPDI), alpha=.3) +
  ggtitle("97% Confidence Interval")

## ------------------------------------------------------------------------
pred_y = post.fit03 %>% 
  select(contains("y_pred"))

PI = data.frame(
  mean = pred_y %>% apply(., 2, mean),
  L_HPDI = pred_y %>% apply(.,2,HPDI, prob = 0.97) %>% .[1,],
  H_HPDI = pred_y %>% apply(.,2,HPDI, prob = 0.97) %>% .[2,],
  weight = d$weight)
  
PI %>% 
  ggplot() +
  geom_point(data = d, aes(weight, height), color="blue", alpha=.3) +
  geom_line(aes(weight, mean)) +
  geom_ribbon(aes(x=weight,ymin=L_HPDI, ymax=H_HPDI), alpha=.3) +
  ggtitle("97% Prediction Interval")

## ------------------------------------------------------------------------
ggplot()+
  geom_point(data = d, aes(weight, height), color="blue", alpha=.3) +
  geom_line(data=CI, aes(weight, mean)) +
  geom_ribbon(data=CI, aes(x=weight, ymin=L_HPDI, ymax=H_HPDI), alpha=.5) +
  geom_ribbon(data=PI, aes(x=weight,ymin=L_HPDI, ymax=H_HPDI), alpha=.2)

