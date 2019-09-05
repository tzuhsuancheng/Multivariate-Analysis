'''
@author: Tzu
@date: 2019/5/14
@title: Assignment 5 (CH10)
'''
# required packages
library(tidyverse)
library(rstan)
library(broom)
library(loo)

# load data
library(MASS)
data(eagles)
dd <- eagles
str(dd)

# transform data
dd$pirate <- ifelse(dd$P == "L", 1, 0)
dd$adult <- ifelse(dd$A == "A", 1, 0)
dd$victim <- ifelse(dd$V == "L", 1, 0)

# question1(a)----

# Rstan setup
m1='
  data {
    int N;
    int<lower=0, upper=1> pirate[N];
    int<lower=0, upper=1> adult[N];
    int<lower=0, upper=1> victim[N];
    int N_y[N];                       // number of successful applicants
    int N_n[N];                       // total number of attempts
  }

  parameters {
    real a;
    real bp;
    real bv;
    real ba;
  }

  model {
    vector[N] p;
    a  ~ normal(0,10);
    bp ~ normal(0,5);
    bv ~ normal(0,5);
    ba ~ normal(0,5);
  
    for (i in 1:N){
      p[i] = inv_logit(a + bp * pirate[i] + ba * adult[i] + bv * victim[i]);
    }

    N_y ~ binomial(N_n, p);
  }

  generated quantities {
    vector[N] log_lik;
    {
      vector[N] p_y;
      for(n in 1:N) {
        p_y[n] = a + bp * pirate[n] + ba * adult[n] + bv * victim[n];
        log_lik[n] = binomial_logit_lpmf(N_y[n] | N_n[n], p_y[n]);
      }
    }
  }
'

dat <- list(
  N = NROW(dd),
  N_n = dd$n,
  N_y = dd$y,
  pirate = dd$pirate,
  adult = dd$adult,
  victim = dd$victim
)

fit1<- stan(model_code = m1, 
                data = dat, 
                iter = 1000, 
                chains = 2, 
                cores = 2)

# quadratic approximation using map()
library(rethinking)
m1_qr <- map(
  alist(
    y ~ dbinom( n, p),
    logit(p) <- a + bp*pirate + ba*adult + bv*victim,
    a ~ dnorm(0, 10),
    c(bp, ba, bv) ~ dnorm(0, 5)
  ), data=dd
)


# Comparing estimates
coef <-
  select(tidy(fit1)[1:4,], term, estimate)  %>%
  rename(., MCMC = estimate) %>%
  mutate(MCMC = round(MCMC, digits = 2)) %>%
  mutate(quadratic = c(0.59, 4.24, -4.59, 1.08))

print(coef)

# (b) ----

lab <- c()
for(i in c(1:nrow(dd))){
  tmp <- c(paste(dd$pirate[i], dd$adult[i], dd$victim[i], sep="/"))
  lab <- c(lab, tmp)
}

dd <-
  dd %>%
  mutate(case = factor(1:nrow(dd))) %>%
  mutate(label = lab)

post1 <- as.data.frame(fit1)
f_mu <- function(p, v, a) plogis(post1$a + post1$bp * p + post1$bv * v + post1$ba * a)
p_y_hat <- mapply(f_mu, p = (dd$pirate == 1), v = (dd$victim == 1), a = (dd$adult == 1) )

# get expectation and 89% intervals of the expectation
dd <- 
  dd %>%
  mutate(p_y = y / n,
         p_hat_mean = colMeans(p_y_hat),
         p_hat_hpdi_l = apply(p_y_hat, 2, rethinking::HPDI)[1,], # 2: by row
         p_hat_hpdi_h = apply(p_y_hat, 2, rethinking::HPDI)[2,])

# plot the Predicted Probability of Success
ggplot(dd) + 
  geom_point(aes(x = label, y = p_y), color = 'dodgerblue') + 
  geom_segment(aes(x = label, xend = label, y = p_hat_hpdi_l, yend = p_hat_hpdi_h)) + 
  geom_point(aes(x = label, y = p_hat_mean), shape = 21, fill = 'white') + 
  scale_y_continuous(limits = c(0, 1)) +
  theme(panel.border = element_rect(colour = "gray90", fill=NA, size=1),
        panel.spacing.x = unit(-0.5, "mm"),
        panel.spacing.y = unit(2, "lines")) + 
  labs(x = 'Pirate/ Adult/ Victim', y = 'Predicted Probability of Success')

# plot the Predicted Success Count
f_mu <- function(n, p, v, a) n * plogis(post1$a + post1$bp * p + post1$bv * v + post1$ba * a)
y_hat <- mapply(f_mu, n = dd$n, p = (dd$pirate == 1), v = (dd$victim == 1), a = (dd$adult == 1) )

# size 該怎麼取
# f_mu <- function(p, v, a) rbinom(n = 1e3, size = 20, prob = plogis(post1$a + post1$bp * p + post1$bv * v + post1$ba * a ))
# y_hat <- mapply(f_mu, p = (dd$pirate == 1), v = (dd$victim == 1), a = (dd$adult == 1))

dd <- 
  dd %>%
  mutate(y_hat_mean = colMeans(y_hat),
         y_hat_hpdi_l = apply(y_hat, 2, rethinking::HPDI)[1,], # 2: by row
         y_hat_hpdi_h = apply(y_hat, 2, rethinking::HPDI)[2,])

ggplot(dd) + 
  geom_point(aes(x = label, y = y), color = 'dodgerblue') + 
  geom_segment(aes(x = label, xend = label, y = y_hat_hpdi_l, yend = y_hat_hpdi_h)) + 
  geom_point(aes(x = label, y = y_hat_mean), shape = 21, fill = 'white') + 
  theme(panel.border = element_rect(colour = "gray90", fill=NA, size=1),
        panel.spacing.x = unit(-0.5, "mm"),
        panel.spacing.y = unit(2, "lines")) + 
  labs(x = 'Pirate/ Adult/ Victim', y = 'Predicted Count of Success')

# (c) ----
# Rstan setup: add interaction term
m2='
data {
int N;
int<lower=0, upper=1> pirate[N];
int<lower=0, upper=1> adult[N];
int<lower=0, upper=1> victim[N];
int N_y[N];                       // number of successful applicants
int N_n[N];                       // total number of attempts
}

parameters {
real a;
real bp;
real bv;
real ba;
real bap;
}

model {
vector[N] p;
a  ~ normal(0,10);
bp ~ normal(0,5);
bv ~ normal(0,5);
ba ~ normal(0,5);
bap ~ normal(0,5);

for (i in 1:N){
p[i] = inv_logit(a + bp * pirate[i] + ba * adult[i] + bv * victim[i] + bap * adult[i] * pirate[i]);
}

N_y ~ binomial(N_n, p);
}

generated quantities {
vector[N] log_lik;
{
  vector[N] p_y;
  for(n in 1:N) {
  p_y[n] = a + bp * pirate[n] + ba * adult[n] + bv * victim[n] + bap * adult[n] * pirate[n];
  log_lik[n] = binomial_logit_lpmf(N_y[n] | N_n[n], p_y[n]);
  }
}
}
'

dat <- list(
  N = NROW(dd),
  N_n = dd$n,
  N_y = dd$y,
  pirate = dd$pirate,
  adult = dd$adult,
  victim = dd$victim
)

fit2<- stan(model_code = m2, 
            data = dat, 
            iter = 1000, 
            chains = 2, 
            cores = 2)

m3='
data {
int N;
int N_y[N];                       // number of successful applicants
int N_n[N];                       // total number of attempts
}

parameters {
real a;

}

model {
vector[N] p;
a  ~ normal(0,10);


for (i in 1:N){
p[i] = inv_logit(a);
}

N_y ~ binomial(N_n, p);
}

generated quantities {
vector[N] log_lik;
{
  vector[N] p_y;
  for(n in 1:N) {
  p_y[n] = a ;
  log_lik[n] = binomial_logit_lpmf(N_y[n] | N_n[n], p_y[n]);
  }
}
}
'

dat <- list(
  N = NROW(dd),
  N_n = dd$n,
  N_y = dd$y
)

fit3<- stan(model_code = m3, 
            data = dat, 
            iter = 1000, 
            chains = 2, 
            cores = 2)

library(GGally) 
post2 <- as.data.frame(fit2)
post2 %>% 
  select(a, bp, bv, ba, bap) %>% 
  ggpairs() + theme_tufte(base_family = 'sans')


# WAIC comparison
# initial model
log_lik_m1 <- extract_log_lik(fit1, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_m1))
loo_m1 <- loo(log_lik_m1, r_eff = r_eff, cores = 2)
waic1 <- loo(log_lik_m1)

# include interaction term
log_lik_m2 <- extract_log_lik(fit2, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_m2))
loo_m2 <- loo(log_lik_m2, r_eff = r_eff, cores = 2)
waic2 <- loo(log_lik_m2)

# only include intercept
log_lik_m3 <- extract_log_lik(fit3, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_m3))
loo_m2 <- loo(log_lik_m3, r_eff = r_eff, cores = 2)
waic3 <- loo(log_lik_m3)

loo::compare(waic1, waic2, waic3)
