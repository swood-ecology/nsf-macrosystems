# Load packages
library(tidyverse)
library(rstan)

# Set working directory
setwd("~/Box Sync/Work/The Nature Conservancy/NSF Macrosystems/")

# Set global Stand conditions
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#### Generate fake data ####
rand <- MASS::mvrnorm(n=10, mu=c(10, 20), Sigma=matrix(c(1, 0.8, 0.8, 1), nrow=2), empirical=TRUE)

# Generate y data which are n=10
y.data <- tibble::tibble(
  y=rand[,1],
  sample.id = c('sample1','sample2','sample3','sample4','sample5','sample6','sample7','sample8','sample9','sample10')
)

# Generate x data which are n=30 (10 samples x 3 repeats)
x.data <- tibble(
  b=rnorm(30,2,0.2),
  a_temp=rnorm(30,0,1),
  row=seq(1:30),
  sample.id=rep(c('sample1','sample2','sample3','sample4','sample5','sample6','sample7',
                  'sample8','sample9','sample10'),3)
  ) %>%
  mutate(
    a = ifelse(row %in% 1:10, rep(rand[,2]*b[1:10],3),
               ifelse(row %in% 11:20, rep(rand[,2]*b[11:20],3),
                      ifelse(row %in% 21:30, rep(rand[,2]*b[21:30],3),'NA')))
  ) %>%
  dplyr::select(a,b,sample.id)

x.data$a <- as.numeric(x.data$a)
x.data$b <- as.numeric(x.data$b)

rm(rand)


#### RUN NO ERROR MODEL ####
# Format data
data.typical <- aggregate(. ~ sample.id, data=x.data,FUN=mean) %>%
  mutate(x = a/b) %>% left_join(y.data)

ne.dat <- list(
  N = 10,
  y = data.typical$y,
  x = data.typical$x
)

# Run model
p1 <- stan(file = "code/statistics/stan/nosigma.stan", data = ne.dat,
           iter = 4000,
           control=list(adapt_delta=0.95),
           chains = 3)


#### RUN ERROR MODEL ####
aggregate(. ~ sample.id, data=x.data,FUN=mean) %>%
  mutate(x = a/b) %>% left_join(y.data)

e.dat <- list(
  N = 10,
  y = data.typical$y,
  x_meas = data.typical$x,
  tau = 0.1
)

# Run model
p2 <- stan(file = "code/statistics/stan/sigma.stan", data = e.dat,
           iter = 4000,
           control=list(adapt_delta=0.95),
           chains = 3)



#### Typical model ####
# Aggregate data
data.typical <- aggregate(. ~ sample.id, data=x.data,FUN=mean) %>%
  mutate(x = a/b) %>% left_join(y.data)

# Run model
lm(y ~ x, data=data.typical) %>%
  summary()
print(p,pars=c('alpha','beta'))

# Compare estimated x data with averaged
print(p,pars=c('x'))
data.typical %>%
  dplyr::select(x,sample.id)


