
#import library
library(rethinking)
library(tidyverse)
library(tidybayes)
library(gridExtra)

## ----eval=FALSE----------------------------------------------------------
## #import library
## library(rethinking)
## library(tidyverse)
## library(tidybayes)
## library(gridExtra)

## ------------------------------------------------------------------------
#import data
data(homeworkch3)
birth1
birth2

## ------------------------------------------------------------------------
#information
n_boys <- sum(birth1, birth2) #total boys=111
n <- length(birth1)+length(birth2) #total observations=200

# define the grid approximation dataframe function
grid_df <- function(x,n){
  tibble(p_grid = seq(from = 0, to = 1, length.out = 1000), prior=1) %>% 
    mutate(likelihood      = dbinom(x, size=n, prob = p_grid)) %>% 
    mutate(unstd_posterior = likelihood * prior) %>%                 
    mutate(posterior       = unstd_posterior / sum(unstd_posterior))
}

## ------------------------------------------------------------------------
# create a dataframe for grid approximation
d <- grid_df(x=n_boys, n=n) 

#plot
d %>%  
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_line() + # add line to connect the dots
  labs(x = "probability of boy birth",
       y = "posterior probability") +
  theme(panel.grid = element_blank())

## ------------------------------------------------------------------------
# the maximum of posterior
maximum <-d$p_grid[which.max(d$posterior)]
print(maximum)

## ------------------------------------------------------------------------
#generate samples into tibble
set.seed(331)
samples <- sample(d$p_grid, # randomly select from p_grid
                  prob = d$posterior, # probility of each p_grid
                  size = 1e4, # draw 10,000 samples
                  replace = T) # sample with replacement 

## ------------------------------------------------------------------------
find_hpdi <- tibble() #create a empty dataframe
for (i in c(0.5,0.89,0.97)){
  df<-
    mode_hdi(samples, .width = i) %>%
    summarise(min=ymin, max=ymax, hpdi=paste(i, "Percentile Interval"))
  
  find_hpdi<-rbind(find_hpdi, df) #append the rows
  }

print(find_hpdi)

## ------------------------------------------------------------------------
for (i in 1:3){
p = d %>% 
  ggplot(aes(x = p_grid)) +
  geom_ribbon(data = d %>% filter(p_grid > find_hpdi$min[i] & 
                                    p_grid < find_hpdi$max[i]),
              aes(ymin = 0, ymax = posterior),
              fill = "grey75") +
  geom_line(aes(y = posterior)) +
  labs(subtitle = find_hpdi$hpdi[i],
       x = "proportion of birth of boys (p)",
       y = "density")
print(p)
}

## ------------------------------------------------------------------------
simulation <- rbinom(1e4, size = n,  prob = samples)
dens(simulation)
abline(v=n_boys)

## ------------------------------------------------------------------------
#update new info
n_boys<- sum(birth1) #n_boys=51
n <- length(birth1) #n=100

# create a new dataframe for grid approximation
d_birth1 <- grid_df(x=n_boys, n=n)

# new samples
samples_birth1 <- sample(d_birth1$p_grid, prob = d_birth1$posterior, size = 1e4, replace = T)

## ------------------------------------------------------------------------
simulation2 <- rbinom(1e4, size=n, prob=samples_birth1)
dens(simulation2)
abline(v=n_boys) 

## ------------------------------------------------------------------------
simulation3 <- rbinom(1e4, size=n, prob=samples)
dens(simulation3)
abline(v=n_boys) 

## ------------------------------------------------------------------------
#dataframe that girls born first
d_gb <-
  tibble(birth1 = birth1,
         birth2 = birth2) %>%
  filter(birth1 == 0)

## ------------------------------------------------------------------------
simulation4 <- rbinom(1e4, size=nrow(d_gb), prob=samples)
dens(simulation4, adj=0.9)
abline(v=sum(d_gb$birth2))

