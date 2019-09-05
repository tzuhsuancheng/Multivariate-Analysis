
#@ title: Assignment 4
#@ author: r07741023 鄭子萱
#@ date: 2019/5/1


# import packages
library(tidyverse)
library(dplyr); library(tidyr); library(rstan); library(ggplot2); library(ggthemes); library(gridExtra)
theme_set(theme_tufte(base_family = 'sans'))

# load the data
data('rugged', package = 'rethinking')
d  <- rugged; rm(rugged)
# take a look
str(d)

# Question 1
# (a)比較有沒有變數Seychelles 的差別

# extract countries with gdp data
d <- d[complete.cases(d$rgdppc_2000), ] # comlete.cases() ==> drop na row

# make log version of outcome
d$loggdp <- log(d$rgdppc_2000)

d2 <- d[d$country != "Seychelles", ]

# Rstan setup
ma = "
data {
int N;
vector[N] loggdp;
vector[N] rugged;
vector<lower=0,upper=1>[N] cont_africa;
}
parameters {
real a;
real bR;
real bA;
real bAR;
real<lower=0, upper=10> sigma;
}
model {
//transfer variable
vector[N] mu;
for (i in 1:N) {
mu[i] = a + bR * rugged[i] + bAR * rugged[i] * cont_africa[i] + bA * cont_africa[i];
}
//prior
a   ~ normal(8,100);
bR  ~ normal(0,1);
bA  ~ normal(0,1);
bAR ~ normal(0,1);

//likelihood
loggdp ~ normal(mu,sigma); 
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu; vector[N] gamma;
  for(n in 1:N) {
  gamma[n] = bR + bAR * cont_africa[n];
  mu[n] = a + gamma[n] * rugged[n] + bA * cont_africa[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}
"

# fitm1: include Seychelles
dat <- list(
  N = NROW(d),
  loggdp = d$loggdp,
  rugged = d$rugged,
  cont_africa = d$cont_africa
)

fitm1 = stan(model_code = m1, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

# fitm2: exclude Seychelles
dat <- list(
  N = NROW(d2),
  loggdp = d2$loggdp,
  rugged = d2$rugged,
  cont_africa = d2$cont_africa
)

fitm2 = stan(model_code = m1, 
             data = dat, 
             cores = 4, 
             chains = 4, 
             iter = 3000)

library(broom)
my_coef_tab <-
  rbind(tidy(fitm1), tidy(fitm2)) %>%
  mutate(model = c(rep("fitm1", times = nrow(tidy(fitm1))),
                   rep("fitm2", times = nrow(tidy(fitm2))))) %>%
  filter(term != "lp__") %>%
  select(model, everything())

coef <-
  my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# compare the coefficients of 2 models
head(coef, n=4)

# (b) plotting the interaction
# with Seyhelles
post1 <- as.data.frame(fitm1)
f_mu_1 <- function(rugged, cont_africa) with(post1, 
                                                a + bR * rugged + bAR * rugged * cont_africa + bA * cont_africa )
mu_1 <- mapply(f_mu_1, rugged = d$rugged, cont_africa = d$cont_africa)
mu_1_mean <- apply(mu_1, 2, mean) # 2 compute by row
mu_1_pi <- apply(mu_1, 2, rethinking::PI, prob = .97)
d_1 <- d %>%
  mutate(mu_mean = mu_1_mean,
         mu_pi_l = mu_1_pi[1,],
         mu_pi_h = mu_1_pi[2,],
         inAfrica = ifelse(cont_africa, 'African nations', 'Non-African nations'))



d_1 %>%
  ggplot(aes(x = rugged)) +
  geom_point(aes(rugged, loggdp, color = cont_africa), shape = 16) +
  theme(legend.position = '') +
  geom_line(aes(rugged, mu_mean)) +
  geom_ribbon(aes(x=rugged, 
                  ymin=mu_pi_l, 
                  ymax=mu_pi_h, 
                  fill = cont_africa), 
              alpha = .1) +
  facet_wrap(~inAfrica) +
  theme_pander()  +
  labs(x = 'Terrain Ruggedness Index(with Seyhelles)', y = 'log GDP year 2000')

# without Seyhelles
post2 <- as.data.frame(fitm2)
f_mu_2 <- function(rugged, cont_africa) with(post2, 
                                             a + bR * rugged + bAR * rugged * cont_africa + bA * cont_africa )
mu_2 <- mapply(f_mu_2, rugged = d$rugged, cont_africa = d$cont_africa)
mu_2_mean <- apply(mu_2, 2, mean)
mu_2_pi <- apply(mu_2, 2, rethinking::PI, prob = .97)
d_2 <- d %>%
  mutate(mu_mean = mu_2_mean,
         mu_pi_l = mu_2_pi[1,],
         mu_pi_h = mu_2_pi[2,],
         inAfrica = ifelse(cont_africa, 'African nations', 'Non-African nations'))



d_2 %>%
  ggplot(aes(x = rugged)) +
  geom_point(aes(rugged, loggdp, color = cont_africa), shape = 16) +
  theme(legend.position = '') +
  geom_line(aes(rugged, mu_mean)) +
  geom_ribbon(aes(x=rugged, 
                  ymin=mu_pi_l, 
                  ymax=mu_pi_h, 
                  fill = cont_africa), 
              alpha = .1) +
  facet_wrap(~inAfrica) +
  theme_pander()  +
  labs(x = 'Terrain Ruggedness Index(without Seyhelles)', y = 'log GDP year 2000')


# (c) model comparison
# Rstan setup

mc_1="
data {
int N;
vector[N] loggdp;
vector[N] rugged;
}
parameters {
real a;
real bR;
real<lower=0, upper=10> sigma;
}
model {
vector[N] mu = a + bR * rugged;
//prior
a ~ normal(8,100);
bR ~ normal(0,1);
//likelihood
loggdp ~ normal(mu,sigma);  
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bR * rugged[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d2),
  loggdp = d2$loggdp,
  rugged = d2$rugged
)
fitmc_1 = stan(model_code = mc_1, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

mc_2 ="
data {
int N;
vector[N] loggdp;
vector[N] rugged;
vector<lower=0,upper=1>[N] cont_africa;
}

parameters {
real a;
real bR;
real bA;
real<lower=0, upper=10> sigma;
}

model {
vector[N] mu = a + bR * rugged + bA * cont_africa;
//prior
a  ~ normal(8,100);
bR ~ normal(8,100);
bA ~ normal(8,100);

//likelihood
loggdp ~ normal(mu,sigma);  
}

generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bR * rugged[n] + bA * cont_africa[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d2),
  loggdp = d2$loggdp,
  rugged = d2$rugged,
  cont_africa = d2$cont_africa
)
fitmc_2 = stan(model_code = mc_2, 
               data = dat,
               cores = 4, 
               chains = 4, 
               iter = 3000)


mc_3 = "
data {
int N;
vector[N] loggdp;
vector[N] rugged;
vector<lower=0,upper=1>[N] cont_africa;
}
parameters {
real a;
real bR;
real bA;
real bAR;
real<lower=0, upper=10> sigma;
}
model {
//transfer variable
vector[N] mu;
for (i in 1:N) {
mu[i] = a + bR * rugged[i] + bAR * rugged[i] * cont_africa[i] + bA * cont_africa[i];
}
//prior
a   ~ normal(8,100);
bR  ~ normal(0,1);
bA  ~ normal(0,1);
bAR ~ normal(0,1);

//likelihood
loggdp ~ normal(mu,sigma); 
}
generated quantities {
vector[N] log_lik;
{
  vector[N] mu; vector[N] gamma;
  for(n in 1:N) {
  gamma[n] = bR + bAR * cont_africa[n];
  mu[n] = a + gamma[n] * rugged[n] + bA * cont_africa[n];
  log_lik[n] = normal_lpdf(loggdp[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d2),
  loggdp = d2$loggdp,
  rugged = d2$rugged,
  cont_africa = d2$cont_africa
)

fitmc_3 = stan(model_code = mc_3, 
             data = dat, 
             cores = 4, 
             chains = 4, 
             iter = 3000)

# WAIC comparison
log_lik_mc1 <- extract_log_lik(fitmc_1, merge_chains = FALSE)
log_lik_mc2 <- extract_log_lik(fitmc_2, merge_chains = FALSE)
log_lik_mc3 <- extract_log_lik(fitmc_3, merge_chains = FALSE)

waic1 <- waic(log_lik_mc1)
waic2 <- waic(log_lik_mc2)
waic3 <- waic(log_lik_mc3)

mod_comp <- loo::compare(waic1, waic2, waic3)
mod_comp

# plot model-averaged-predictions
w <- 
  mod_comp %>% 
  as_tibble() %>%
  mutate(ratio = w$waic / sum(w$waic) )

postmc1 <- as.data.frame(fitmc_1)
postmc2 <- as.data.frame(fitmc_2)
postmc3 <- as.data.frame(fitmc_3)

mu_c_1 <- function(rugged) with(postmc1, 
                                        a + bR * rugged)
mu_c_2 <- function(rugged, cont_africa) with(postmc2, 
                                             a + bR * rugged + bA * cont_africa )
mu_c_3 <- function(rugged, cont_africa) with(postmc3, 
                                             a + bR * rugged + bAR * rugged * cont_africa + bA * cont_africa )

mu_c_avg <- 
  w$ratio[1] * mapply(mu_c_1, rugged = d2$rugged) + 
  w$ratio[2] * mapply(mu_c_2, rugged = d2$rugged, cont_africa = d2$cont_africa) +
  w$ratio[3] * mapply(mu_c_3, rugged = d2$rugged, cont_africa = d2$cont_africa)

mu_c_mean <- apply(mu_c_avg, 2, mean) # 2 compute by row
mu_c_pi <- apply(mu_c_avg, 2, rethinking::PI, prob = .97)
d_c <- d2 %>%
  mutate(mu_mean = mu_c_mean,
         mu_pi_l = mu_c_pi[1,],
         mu_pi_h = mu_c_pi[2,],
         inAfrica = ifelse(cont_africa, 'African nations', 'Non-African nations'))

d_c %>%
  ggplot(aes(x = rugged)) +
  geom_point(aes(rugged, loggdp, color = cont_africa), shape = 16) +
  theme(legend.position = '') +
  geom_line(aes(rugged, mu_mean)) +
  geom_ribbon(aes(x=rugged, 
                  ymin=mu_pi_l, 
                  ymax=mu_pi_h, 
                  fill = cont_africa), 
              alpha = .1) +
  facet_wrap(~inAfrica) +
  theme_pander()  +
  labs(x = 'Terrain Ruggedness Index(avg pred w/o S)', y = 'log GDP year 2000')


# Question2
# (a)
data('nettle', package = 'rethinking')
d3  <- nettle; rm(rugged)

# data-preprocessing
d3$lang.per.cap <- d3$num.lang / d3$k.pop
d3$log_lpc <- log(d3$lang.per.cap)
d3$log_area <- log(d3$area)
d3$log_area.c <- d3$log_area - mean(d3$log_area)
d3$mgs.c <- d3$mean.growing.season - mean(d3$mean.growing.season)
d3$sgs.c <- d3$sd.growing.season - mean(d3$sd.growing.season)


# Rstan setup

m2a_1="
data {
int N;
vector[N] log_lpc;
vector[N] mgsc;
vector[N] log_areac;
}

parameters {
real a;
real bM;
real bA;
real<lower=0, upper=10> sigma;
}

model {
vector[N] mu = a + bM * mgsc + bA * log_areac;
//prior
a ~ normal(-5,5);
bM ~ normal(0,5);
bA ~ normal(0,5);
//likelihood
log_lpc ~ normal(mu,sigma);  
}

generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bM * mgsc[n] + bA * log_areac[n];
  log_lik[n] = normal_lpdf(log_lpc[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d3),
  log_lpc = d3$log_lpc,
  mgsc = d3$mgs.c,
  log_areac = d3$log_area.c
)

fitm2a_1 = stan(model_code = m2a_1, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

d3 %>%
  ggplot(aes(x = mgs.c)) +
  geom_point(aes(mgs.c, log_lpc), 
             shape = 16, 
             color = 'dodgerblue',size = 2) + 
  geom_abline(slope = mean(post$bM),
              intercept = mean(post$a),
              size = 1 ) +
  theme(text = element_text(family = "Times"),
        legend.position = "none") +
  theme_pander() + 
  xlab("mgs.c") +
  ylab("log_lpc") 



m2a_0="
data {
int N;
vector[N] log_lpc;
vector[N] mgsc;
}

parameters {
real a;
real bM;
real<lower=0, upper=10> sigma;
}

model {
vector[N] mu = a + bM * mgsc;
//prior
a ~ normal(-5,5);
bM ~ normal(0,5);
//likelihood
log_lpc ~ normal(mu,sigma);  
}

generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bM * mgsc[n] ;
  log_lik[n] = normal_lpdf(log_lpc[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d3),
  log_lpc = d3$log_lpc,
  mgsc = d3$mgs.c
)

fitm2a_0 = stan(model_code = m2a_0, 
                data = dat, 
                cores = 4, 
                chains = 4, 
                iter = 3000)

#coeff
my_coef_tab <-
  rbind(tidy(fitm2a_0), tidy(fitm2a_1)) %>%
  mutate(model = c(rep("fitm2a_0", times = nrow(tidy(fitm2a_0))),
                   rep("fitm2a_1", times = nrow(tidy(fitm2a_1))))) %>%
  filter(term != "lp__") %>%
  select(model, everything())

coef <-
  my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# compare the coefficients of 2 models
head(coef, n=3)


# (b)
m2b_0="
data {
int N;
vector[N] log_lpc;
vector[N] sgsc;
}

parameters {
real a;
real bS;
real<lower=0, upper=10> sigma;
}

model {
vector[N] mu = a + bS * sgsc;
//prior
a ~ normal(-5,5);
bS ~ normal(0,5);
//likelihood
log_lpc ~ normal(mu,sigma);  
}

generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bS * sgsc[n] ;
  log_lik[n] = normal_lpdf(log_lpc[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d3),
  log_lpc = d3$log_lpc,
  sgsc = d3$sgs.c
)

fitm2b_0 = stan(model_code = m2b_0, 
                data = dat, 
                cores = 4, 
                chains = 4, 
                iter = 3000)


m2b_1="
data {
int N;
vector[N] log_lpc;
vector[N] sgsc;
vector[N] log_areac;
}

parameters {
real a;
real bS;
real bA;
real<lower=0, upper=10> sigma;
}

model {
vector[N] mu = a + bS * sgsc + bA * log_areac;
//prior
a ~ normal(-5,5);
bS ~ normal(0,5);
bA ~ normal(0,5);
//likelihood
log_lpc ~ normal(mu,sigma);  
}

generated quantities {
vector[N] log_lik;
{
  vector[N] mu;
  for(n in 1:N) {
  mu[n] = a + bS * sgsc[n] + bA * log_areac[n];
  log_lik[n] = normal_lpdf(log_lpc[n] | mu[n], sigma);
  }
}
}
"

dat <- list(
  N = NROW(d3),
  log_lpc = d3$log_lpc,
  sgsc = d3$sgs.c,
  log_areac = d3$log_area.c
)

fitm2b_1 = stan(model_code = m2b_1, 
               data = dat, 
               cores = 4, 
               chains = 4, 
               iter = 3000)

#coeff
my_coef_tab <-
  rbind(tidy(fitm2b_0), tidy(fitm2b_1)) %>%
  mutate(model = c(rep("fitm2b_0", times = nrow(tidy(fitm2b_0))),
                   rep("fitm2b_1", times = nrow(tidy(fitm2b_1))))) %>%
  filter(term != "lp__") %>%
  select(model, everything())

coef <-
  my_coef_tab %>%
  complete(term = distinct(., term), model) %>%
  select(model, term, estimate) %>%
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate)

# compare the coefficients of 2 models
head(coef, n=3)


# (c)
m2c="
data {
int N;
vector[N] log_lpc;
vector[N] sgsc;
vector[N] mgsc;
}

parameters {
real a;
real bS;
real bM;
real bMS;
real<lower=0, upper=10> sigma;
}

model {
vector[N] mu;
for (i in 1:N) {
mu[i] = a + bS * sgsc[i] + bM * mgsc[i] + bMS*mgsc[i]*sgsc[i];}

//prior
a ~ normal(-5,5);
bS ~ normal(0,5);
bM ~ normal(0,5);
bMS ~ normal(0,5);

//likelihood
log_lpc ~ normal(mu,sigma);  
}
"

dat <- list(
  N = NROW(d3),
  log_lpc = d3$log_lpc,
  sgsc = d3$sgs.c,
  mgsc = d3$mgs.c
)

fitm2c = stan(model_code = m2c, 
                data = dat, 
                cores = 4, 
                chains = 4, 
                iter = 3000)

print(fitm2c)

a <- 
  mod_comp %>% 
  as_tibble()

w <- w %>% mutate(ratio = w$waic / sum(w$waic) )

              