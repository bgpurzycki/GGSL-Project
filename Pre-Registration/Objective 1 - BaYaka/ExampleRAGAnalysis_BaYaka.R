### Example code for Martin's BaYaka paper - Binomial RAG models
### Created 24/03/2026 by Dan Major-Smith
### R version 4.4.1


## Install and load packages
rm(list=ls())
Sys.setenv(LANG = "en")

# For BRMS, make sure that Stan is installed
#install.packages("brms")
library(brms)

#install.packages("rstan")
library(rstan)

#install.packages("loo")
library(loo)


### Simulate data based on DAG in pre-reg document
set.seed(2112)
n <- 100

## Confounders (for simplicity, will assume all confounders are independent of one another)

# Age (standardised)
AGE <- rnorm(n = n, mean = 0, sd = 1)
summary(AGE); sd(AGE)

# Sex (1 = male)
SEX <- rbinom(n = n, size = 1, prob = 0.5)
table(SEX)

# Number of years of education
FORMALED <- rpois(n = n, lambda = 8)
table(FORMALED)
summary(FORMALED); sd(FORMALED)
hist(FORMALED)

# Material insecurity
MAT <- runif(n = n, min = 0, max = 1)
hist(MAT)

# Number of children
CHILDREN <- rpois(n = n, lambda = 1)
table(CHILDREN)
summary(CHILDREN); sd(CHILDREN)

# Ever lived in village/city
CITY <- rbinom(n = n, size = 1, prob = 0.25)
table(CITY)


## Features of the game

# Order of games
ORDER <- rbinom(n = n, size = 1, prob = 0.5)
table(ORDER)

# Number of comprehension checks needed
COMP <- rpois(n = n, lambda = 1)
table(COMP)
summary(COMP); sd(COMP)


### Exposure variables - As described in the pre-reg doc, we're interested in the joint effect of two supernatural belief factors (punishment [2 vars] and omniscience [2 vars]) for 'forest spirit' local gods - All individual variables are binary, and will simulate them as generating from an unmeasured latent religiosity factor
LG_U <- rnorm(n = n, mean = 0, sd = 1)
summary(LG_U); sd(LG_U)

### Next simulate the individual variables, including causation from confounders. For ease, will simulate on natural probability scale

## Punishment variables

# Ever punishes for behaviour
LGPUNISH <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGPUNISH)

# Influences in afterlife
LGDIE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGDIE)

# Average these variables together
LGPUN_AVE <- (LGPUNISH + LGDIE) / 2
table(LGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
LGFEEL <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGFEEL)

# Knows what people are doing far away
LGSEE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGSEE)

# Average these variables together
LGOMNI_AVE <- (LGFEEL + LGSEE) / 2
table(LGOMNI_AVE)


# Combine data together
dat <- as.data.frame(cbind(AGE, SEX, FORMALED, MAT, CHILDREN, CITY, ORDER, COMP, 
                     LGPUNISH, LGDIE, LGPUN_AVE, LGFEEL, LGSEE, LGOMNI_AVE))
head(dat)


### Outcome model for RAG decisions (number of coins donated to distant, out of 30) - For simplicity here, will assume additive relationship for religious belief variables. For the Self vs Distant RAG, there was an excess of '15' responses, so will model this inflation here as well

# Probability of inflation (for simplicity, and because low numbers of inflated cases, will assume not caused by anything)
dat$inflate_self <- rbinom(n = n, size = 1, prob = 0.2)
table(dat$inflate_self)

# Binomial distribution if not inflated
dat_noInf <- subset(dat[dat$inflate_self == 0, ])
dat_noInf$self_RAG <- rbinom(n = nrow(dat_noInf), size = 30, 
                             prob = 0.5 + (0.1 * dat_noInf$LGPUN_AVE) + (0.1 * dat_noInf$LGOMNI_AVE) + 
                               (0.02 * dat_noInf$AGE) + (-0.1 * dat_noInf$SEX) + (0.01 * dat_noInf$FORMALED) + 
                               (-0.1 * dat_noInf$MAT) + (-0.03 * dat_noInf$CHILDREN) + (-0.1 * dat_noInf$CITY) + 
                               (0.05 * dat_noInf$ORDER) + (-0.05 * dat_noInf$COMP))

summary(dat_noInf$self_RAG)

# 15 if inflated
dat_inf <- subset(dat[dat$inflate_self == 1, ])
dat_inf$self_RAG <- 15

# Merge datasets back together
dat <- rbind(dat_noInf, dat_inf)
head(dat)

summary(dat$self_RAG)
hist(dat$self_RAG)
table(dat$self_RAG)



##################################################################################
#### Run model in stan

## Stan code (adapted from BRMS code for zero-inflated binomial model)
model_code_15inf <- '
functions {
  /* Inflated binomial log-PDF of a single response
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   theta: probability parameter of the binomial part
   *   zi: inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_lpmf(int y, int trials,
                                   real theta, real zi) {
    if (y == 15) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         binomial_lpmf(15 | trials, theta));
    } else {
      return bernoulli_lpmf(0 | zi) +
             binomial_lpmf(y | trials, theta);
    }
  }
  /* Inflated binomial log-PDF of a single response
   * logit parameterization of the inflation part
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   theta: probability parameter of the binomial part
   *   zi: linear predictor for inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_logit_lpmf(int y, int trials,
                                         real theta, real zi) {
    if (y == 15) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         binomial_lpmf(15 | trials, theta));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             binomial_lpmf(y | trials, theta);
    }
  }
  /* Inflated binomial log-PDF of a single response
   * logit parameterization of the binomial part
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   eta: linear predictor for binomial part
   *   zi: inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_blogit_lpmf(int y, int trials,
                                          real eta, real zi) {
    if (y == 15) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         binomial_logit_lpmf(15 | trials, eta));
    } else {
      return bernoulli_lpmf(0 | zi) +
             binomial_logit_lpmf(y | trials, eta);
    }
  }
  /* Inflated binomial log-PDF of a single response
   * logit parameterization of the binomial part
   * logit parameterization of the inflation part
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   eta: linear predictor for binomial part
   *   zi: linear predictor for inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_blogit_logit_lpmf(int y, int trials,
                                                real eta, real zi) {
    if (y == 15) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         binomial_logit_lpmf(15 | trials, eta));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             binomial_logit_lpmf(y | trials, eta);
    }
  }
  // inflated binomial log-CCDF and log-CDF functions
  real inflated_binomial_lccdf(int y, int trials, real theta, real zi) {
    return bernoulli_lpmf(0 | zi) + binomial_lccdf(y | trials, theta);
  }
  real inflated_binomial_lcdf(int y, int trials, real theta, real zi) {
    return log1m_exp(inflated_binomial_lccdf(y | trials, theta, zi));
  }
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  array[N] int trials;  // number of trials
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0,upper=1> zi;  // inflation probability
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += beta_lpdf(zi | 1, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
      target += inflated_binomial_blogit_lpmf(Y[n] | trials[n], mu[n], zi);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  
  // Log-likelihood (mu in brackets so are just temporary parameters)
  vector[N] log_lik;
  {
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
      log_lik[n] = inflated_binomial_blogit_lpmf(Y[n] | trials[n], mu[n], zi);
    }
  }
}
'

## Data for Stan code
dat_temp <- cbind(Intercept = as.integer(rep(1, n)), LGPUN_AVE = dat$LGPUN_AVE, 
                  LGOMNI_AVE = dat$LGOMNI_AVE, AGE = dat$AGE, SEX = as.integer(dat$SEX), 
                  FORMALED = as.integer(dat$FORMALED), MAT = dat$MAT, 
                  CHILDREN = as.integer(dat$CHILDREN), CITY = as.integer(dat$CITY),
                  ORDER = as.integer(dat$ORDER), COMP = as.integer(dat$COMP))

stan_dat <- list(
  Y = as.integer(dat$self_RAG),
  X = dat_temp,
  N = as.integer(n),
  trials = as.integer(rep(30, n)),
  K = as.integer(ncol(dat_temp)),
  Kc = as.integer(ncol(dat_temp) - 1),
  prior_only = as.integer(0)
)


str(stan_dat)


### Stan model
mod <- stan(model_code = model_code_15inf, data = stan_dat,
           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 24876)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod, probs = c(0.025, 0.975))


## Model fit values
log_lik_matrix <- extract_log_lik(mod, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_add <- loo(log_lik_matrix, r_eff = r_eff)
loo_add


### Model with interaction between exposures

## Data for Stan code
dat_temp_int <- cbind(Intercept = as.integer(rep(1, n)), LGPUN_AVE = dat$LGPUN_AVE, 
                  LGOMNI_AVE = dat$LGOMNI_AVE, LGPUN_OMNI_INT = dat$LGPUN_AVE * dat$LGOMNI_AVE,
                  AGE = dat$AGE, SEX = as.integer(dat$SEX), 
                  FORMALED = as.integer(dat$FORMALED), MAT = dat$MAT, 
                  CHILDREN = as.integer(dat$CHILDREN), CITY = as.integer(dat$CITY),
                  ORDER = as.integer(dat$ORDER), COMP = as.integer(dat$COMP))

stan_dat_int <- list(
  Y = as.integer(dat$self_RAG),
  X = dat_temp_int,
  N = as.integer(n),
  trials = as.integer(rep(30, n)),
  K = as.integer(ncol(dat_temp_int)),
  Kc = as.integer(ncol(dat_temp_int) - 1),
  prior_only = as.integer(0)
)


### Stan model with interaction
mod_int <- stan(model_code = model_code_15inf, data = stan_dat_int,
            iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 52454)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod_int, probs = c(0.025, 0.975))


## Model fit values
log_lik_matrix <- extract_log_lik(mod_int, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_int <- loo(log_lik_matrix, r_eff = r_eff)
loo_int


## Compare additive and interaction models - No improvement with inclusion of interaction terms (as expected, given data were simulated as additive)
loo_compare(loo_add, loo_int)



### G-computation (based on additive model)

# Extract model coefficients
(post_coefs <- as_draws_df(mod))

# Where supernatural beliefs are 0
dat_temp <- data.frame(cbind(Intercept = as.integer(rep(1, n)), LGPUN_AVE = dat$LGPUN_AVE, 
                             LGOMNI_AVE = dat$LGOMNI_AVE, AGE = dat$AGE, SEX = as.integer(dat$SEX), 
                             FORMALED = as.integer(dat$FORMALED), MAT = dat$MAT, 
                             CHILDREN = as.integer(dat$CHILDREN), CITY = as.integer(dat$CITY),
                             ORDER = as.integer(dat$ORDER), COMP = as.integer(dat$COMP)))

df_sup0 <- dat_temp
df_sup0$LGPUN_AVE <- 0
df_sup0$LGOMNI_AVE <- 0

sup0 <- list()
for (i in 1:nrow(post_coefs)) {
  
  # Make temp dataset for this
  dat_temp <- df_sup0
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = n, size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$self_RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                    prob = plogis(post_coefs$b_Intercept[i] +
                                                    (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                    (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                    (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                    (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                    (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                    (post_coefs$'b[6]'[i] * dat_temp_noInf$MAT) +
                                                    (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                    (post_coefs$'b[8]'[i] * dat_temp_noInf$CITY) +
                                                    (post_coefs$'b[9]'[i] * dat_temp_noInf$ORDER) +
                                                    (post_coefs$'b[10]'[i] * dat_temp_noInf$COMP)))
  
  # Code as '15' if inflated, then merge together
  dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
  dat_temp_inf$self_RAG_temp <- 15
  
  dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  
  sup0[i] <- as.data.frame(dat_temp$self_RAG_temp)
}


# Where supernatural beliefs are 1
df_sup1 <- dat_temp
df_sup1$LGPUN_AVE <- 1
df_sup1$LGOMNI_AVE <- 1

sup1 <- list()
for (i in 1:nrow(post_coefs)) {
  
  # Make temp dataset for this
  dat_temp <- df_sup1
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = n, size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$self_RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                         prob = plogis(post_coefs$b_Intercept[i] +
                                                         (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                         (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                         (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                         (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                         (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                         (post_coefs$'b[6]'[i] * dat_temp_noInf$MAT) +
                                                         (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                         (post_coefs$'b[8]'[i] * dat_temp_noInf$CITY) +
                                                         (post_coefs$'b[9]'[i] * dat_temp_noInf$ORDER) +
                                                         (post_coefs$'b[10]'[i] * dat_temp_noInf$COMP)))
  
  # Code as '15' if inflated, then merge together
  dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
  dat_temp_inf$self_RAG_temp <- 15
  
  dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  
  sup1[i] <- as.data.frame(dat_temp$self_RAG_temp)
}


## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, length(sup0))
for (i in 1:length(sup0)) {
  sup_diff[i] <- mean(sup1[[i]]) - mean(sup0[[i]])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))



###############################################################################################
###############################################################################################
#### As above, but now for the Local vs Distant RAG condition, where is not just excess 15s, but also excess 16s and 17s as well, so will adapt the code above to model this

### Simulate data based on DAG in pre-reg document
rm(list=ls())
set.seed(2112)
n <- 100

## Confounders (for simplicity, will assume all confounders are independent of one another)

# Age (standardised)
AGE <- rnorm(n = n, mean = 0, sd = 1)
summary(AGE); sd(AGE)

# Sex (1 = male)
SEX <- rbinom(n = n, size = 1, prob = 0.5)
table(SEX)

# Number of years of education
FORMALED <- rpois(n = n, lambda = 8)
table(FORMALED)
summary(FORMALED); sd(FORMALED)
hist(FORMALED)

# Material insecurity
MAT <- runif(n = n, min = 0, max = 1)
hist(MAT)

# Number of children
CHILDREN <- rpois(n = n, lambda = 1)
table(CHILDREN)
summary(CHILDREN); sd(CHILDREN)

# Ever lived in village/city
CITY <- rbinom(n = n, size = 1, prob = 0.25)
table(CITY)


## Features of the game

# Order of games
ORDER <- rbinom(n = n, size = 1, prob = 0.5)
table(ORDER)

# Number of comprehension checks needed
COMP <- rpois(n = n, lambda = 1)
table(COMP)
summary(COMP); sd(COMP)


### Exposure variables - As described in the pre-reg doc, we're interested in the joint effect of two supernatural belief factors (punishment [2 vars] and omniscience [2 vars]) for 'forest spirit' local gods - All individual variables are binary, and will simulate them as generating from an unmeasured latent religiosity factor
LG_U <- rnorm(n = n, mean = 0, sd = 1)
summary(LG_U); sd(LG_U)

### Next simulate the individual variables, including causation from confounders. For ease, will simulate on natural probability scale

## Punishment variables

# Ever punishes for behaviour
LGPUNISH <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGPUNISH)

# Influences in afterlife
LGDIE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGDIE)

# Average these variables together
LGPUN_AVE <- (LGPUNISH + LGDIE) / 2
table(LGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
LGFEEL <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                   (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGFEEL)

# Knows what people are doing far away
LGSEE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (-0.1 * CITY))
table(LGSEE)

# Average these variables together
LGOMNI_AVE <- (LGFEEL + LGSEE) / 2
table(LGOMNI_AVE)


# Combine data together
dat <- as.data.frame(cbind(AGE, SEX, FORMALED, MAT, CHILDREN, CITY, ORDER, COMP, 
                           LGPUNISH, LGDIE, LGPUN_AVE, LGFEEL, LGSEE, LGOMNI_AVE))
head(dat)


### Outcome model for RAG decisions (number of coins donated to distant, out of 30) - For simplicity here, will assume additive relationship for religious belief variables. For the Local vs Distant RAG, there was an excess of '15', '16' and '17' responses, so will model this inflation here as well

# Probability of inflation (for simplicity, and because low numbers of inflated cases, will assume not caused by anything)
dat$inflate_local <- rbinom(n = n, size = 1, prob = 0.3)
table(dat$inflate_local)

# Binomial distribution if not inflated
dat_noInf <- subset(dat[dat$inflate_local == 0, ])
dat_noInf$local_RAG <- rbinom(n = nrow(dat_noInf), size = 30, 
                             prob = 0.5 + (0.1 * dat_noInf$LGPUN_AVE) + (0.1 * dat_noInf$LGOMNI_AVE) + 
                               (0.02 * dat_noInf$AGE) + (-0.1 * dat_noInf$SEX) + (0.01 * dat_noInf$FORMALED) + 
                               (-0.1 * dat_noInf$MAT) + (-0.03 * dat_noInf$CHILDREN) + (-0.1 * dat_noInf$CITY) + 
                               (0.05 * dat_noInf$ORDER) + (-0.05 * dat_noInf$COMP))

summary(dat_noInf$local_RAG)

# 15 if inflated
dat_inf <- subset(dat[dat$inflate_local == 1, ])
dat_inf$temp <- runif(n = nrow(dat_inf), min = 0, max = 1)
dat_inf$local_RAG <- ifelse(dat_inf$temp < 0.33, 15, 
                            ifelse(dat_inf$temp > 0.67, 16, 17))
dat_inf$temp <- NULL

# Merge datasets back together
dat <- rbind(dat_noInf, dat_inf)
head(dat)

summary(dat$local_RAG)
hist(dat$local_RAG)
table(dat$local_RAG)



##################################################################################
#### Run model in stan

## Stan code (adapted from BRMS code for zero-inflated binomial model)
model_code_151617inf <- '
functions {
  /* Inflated binomial log-PDF of a single response
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   theta: probability parameter of the binomial part
   *   zi: inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_lpmf(int y, int trials,
                                   real theta, real zi) {
    if (y == 15 || y == 16 || y == 17) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         binomial_lpmf(y | trials, theta));
    } else {
      return bernoulli_lpmf(0 | zi) +
             binomial_lpmf(y | trials, theta);
    }
  }
  /* Inflated binomial log-PDF of a single response
   * logit parameterization of the inflation part
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   theta: probability parameter of the binomial part
   *   zi: linear predictor for inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_logit_lpmf(int y, int trials,
                                         real theta, real zi) {
    if (y == 15 || y == 16 || y == 17) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         binomial_lpmf(y | trials, theta));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             binomial_lpmf(y | trials, theta);
    }
  }
  /* Inflated binomial log-PDF of a single response
   * logit parameterization of the binomial part
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   eta: linear predictor for binomial part
   *   zi: inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_blogit_lpmf(int y, int trials,
                                          real eta, real zi) {
    if (y == 15 || y == 16 || y == 17) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         binomial_logit_lpmf(y | trials, eta));
    } else {
      return bernoulli_lpmf(0 | zi) +
             binomial_logit_lpmf(y | trials, eta);
    }
  }
  /* Inflated binomial log-PDF of a single response
   * logit parameterization of the binomial part
   * logit parameterization of the inflation part
   * Args:
   *   y: the response value
   *   trials: number of trials of the binomial part
   *   eta: linear predictor for binomial part
   *   zi: linear predictor for inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real inflated_binomial_blogit_logit_lpmf(int y, int trials,
                                                real eta, real zi) {
    if (y == 15 || y == 16 || y == 17) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         binomial_logit_lpmf(y | trials, eta));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             binomial_logit_lpmf(y | trials, eta);
    }
  }
  // inflated binomial log-CCDF and log-CDF functions
  real inflated_binomial_lccdf(int y, int trials, real theta, real zi) {
    return bernoulli_lpmf(0 | zi) + binomial_lccdf(y | trials, theta);
  }
  real inflated_binomial_lcdf(int y, int trials, real theta, real zi) {
    return log1m_exp(inflated_binomial_lccdf(y | trials, theta, zi));
  }
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  array[N] int trials;  // number of trials
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0,upper=1> zi;  // inflation probability
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += beta_lpdf(zi | 1, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
      target += inflated_binomial_blogit_lpmf(Y[n] | trials[n], mu[n], zi);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  
  // Log-likelihood (mu in brackets so are just temporary parameters)
  vector[N] log_lik;
  {
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
      log_lik[n] = inflated_binomial_blogit_lpmf(Y[n] | trials[n], mu[n], zi);
    }
  }
}
'

## Data for Stan code
dat_temp <- cbind(Intercept = as.integer(rep(1, n)), LGPUN_AVE = dat$LGPUN_AVE, 
                  LGOMNI_AVE = dat$LGOMNI_AVE, AGE = dat$AGE, SEX = as.integer(dat$SEX), 
                  FORMALED = as.integer(dat$FORMALED), MAT = dat$MAT, 
                  CHILDREN = as.integer(dat$CHILDREN), CITY = as.integer(dat$CITY),
                  ORDER = as.integer(dat$ORDER), COMP = as.integer(dat$COMP))

stan_dat <- list(
  Y = as.integer(dat$local_RAG),
  X = dat_temp,
  N = as.integer(n),
  trials = as.integer(rep(30, n)),
  K = as.integer(ncol(dat_temp)),
  Kc = as.integer(ncol(dat_temp) - 1),
  prior_only = as.integer(0)
)


str(stan_dat)


### Stan model
mod <- stan(model_code = model_code_151617inf, data = stan_dat,
            iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12865)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod, probs = c(0.025, 0.975))


## Model fit values
log_lik_matrix <- extract_log_lik(mod, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_add <- loo(log_lik_matrix, r_eff = r_eff)
loo_add


### Model with interaction between exposures

## Data for Stan code
dat_temp_int <- cbind(Intercept = as.integer(rep(1, n)), LGPUN_AVE = dat$LGPUN_AVE, 
                      LGOMNI_AVE = dat$LGOMNI_AVE, LGPUN_OMNI_INT = dat$LGPUN_AVE * dat$LGOMNI_AVE,
                      AGE = dat$AGE, SEX = as.integer(dat$SEX), 
                      FORMALED = as.integer(dat$FORMALED), MAT = dat$MAT, 
                      CHILDREN = as.integer(dat$CHILDREN), CITY = as.integer(dat$CITY),
                      ORDER = as.integer(dat$ORDER), COMP = as.integer(dat$COMP))

stan_dat_int <- list(
  Y = as.integer(dat$local_RAG),
  X = dat_temp_int,
  N = as.integer(n),
  trials = as.integer(rep(30, n)),
  K = as.integer(ncol(dat_temp_int)),
  Kc = as.integer(ncol(dat_temp_int) - 1),
  prior_only = as.integer(0)
)


### Stan model with interaction
mod_int <- stan(model_code = model_code_151617inf, data = stan_dat_int,
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 7471)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod_int, probs = c(0.025, 0.975))


## Model fit values
log_lik_matrix <- extract_log_lik(mod_int, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_int <- loo(log_lik_matrix, r_eff = r_eff)
loo_int


## Compare additive and interaction models - No improvement with inclusion of interaction terms (as expected, given data were simulated as additive)
loo_compare(loo_add, loo_int)



### G-computation (based on additive model)

# Extract model coefficients
(post_coefs <- as_draws_df(mod))

# Where supernatural beliefs are 0
dat_temp <- data.frame(cbind(Intercept = as.integer(rep(1, n)), LGPUN_AVE = dat$LGPUN_AVE, 
                             LGOMNI_AVE = dat$LGOMNI_AVE, AGE = dat$AGE, SEX = as.integer(dat$SEX), 
                             FORMALED = as.integer(dat$FORMALED), MAT = dat$MAT, 
                             CHILDREN = as.integer(dat$CHILDREN), CITY = as.integer(dat$CITY),
                             ORDER = as.integer(dat$ORDER), COMP = as.integer(dat$COMP)))

df_sup0 <- dat_temp
df_sup0$LGPUN_AVE <- 0
df_sup0$LGOMNI_AVE <- 0

sup0 <- list()
for (i in 1:nrow(post_coefs)) {
  
  # Make temp dataset for this
  dat_temp <- df_sup0
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = n, size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$local_RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                         prob = plogis(post_coefs$b_Intercept[i] +
                                                         (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                         (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                         (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                         (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                         (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                         (post_coefs$'b[6]'[i] * dat_temp_noInf$MAT) +
                                                         (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                         (post_coefs$'b[8]'[i] * dat_temp_noInf$CITY) +
                                                         (post_coefs$'b[9]'[i] * dat_temp_noInf$ORDER) +
                                                         (post_coefs$'b[10]'[i] * dat_temp_noInf$COMP)))
  
  # Code as '15', '16' or '17' if inflated, then merge together
  dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
  dat_temp_inf$temp <- runif(n = nrow(dat_temp_inf), min = 0, max = 1)
  dat_temp_inf$local_RAG_temp <- ifelse(dat_temp_inf$temp < 0.33, 15, 
                              ifelse(dat_temp_inf$temp > 0.67, 16, 17))
  dat_temp_inf$temp <- NULL
  
  dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  
  sup0[i] <- as.data.frame(dat_temp$local_RAG_temp)
}


# Where supernatural beliefs are 1
df_sup1 <- dat_temp
df_sup1$LGPUN_AVE <- 1
df_sup1$LGOMNI_AVE <- 1

sup1 <- list()
for (i in 1:nrow(post_coefs)) {
  
  # Make temp dataset for this
  dat_temp <- df_sup1
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = n, size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$local_RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                         prob = plogis(post_coefs$b_Intercept[i] +
                                                         (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                         (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                         (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                         (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                         (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                         (post_coefs$'b[6]'[i] * dat_temp_noInf$MAT) +
                                                         (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                         (post_coefs$'b[8]'[i] * dat_temp_noInf$CITY) +
                                                         (post_coefs$'b[9]'[i] * dat_temp_noInf$ORDER) +
                                                         (post_coefs$'b[10]'[i] * dat_temp_noInf$COMP)))
  
  # Code as '15', '16' or '17' if inflated, then merge together
  dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
  dat_temp_inf$temp <- runif(n = nrow(dat_temp_inf), min = 0, max = 1)
  dat_temp_inf$local_RAG_temp <- ifelse(dat_temp_inf$temp < 0.33, 15, 
                                        ifelse(dat_temp_inf$temp > 0.67, 16, 17))
  dat_temp_inf$temp <- NULL
  
  dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  
  sup1[i] <- as.data.frame(dat_temp$local_RAG_temp)
}


## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, length(sup0))
for (i in 1:length(sup0)) {
  sup_diff[i] <- mean(sup1[[i]]) - mean(sup0[[i]])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))


## Given the relatively low number of inflated cases, and the large number of covariates, we have assumed that inflation is independent of all covariates here (could quite easily adapt the code above to include covariates in the inflated term, though).