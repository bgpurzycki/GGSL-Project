### Example code for Kichwa paper - Binomial RAG models
### Created 04/02/2026 by Dan Major-Smith
### R version 4.4.1


## Install and load packages
rm(list=ls())
Sys.setenv(LANG = "en")

# For BRMS, make sure that Stan is installed
#install.packages("brms")
library(brms)


### Simulate data based on DAG in pre-reg document
set.seed(934)
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

# Material/food insecurity
MAT <- runif(n = n, min = 0, max = 1)
hist(MAT)

# Number of children
CHILDREN <- rpois(n = n, lambda = 1)
table(CHILDREN)
summary(CHILDREN); sd(CHILDREN)

# Market Integration
MARKET <- rnorm(n = n, mean = 0.5, sd = 0.16)
summary(MARKET); sd(MARKET)


## Features of the game

# Order of games
ORDER <- rbinom(n = n, size = 1, prob = 0.5)
table(ORDER)

# Number of comprehension checks needed
COMP <- rpois(n = n, lambda = 1)
table(COMP)
summary(COMP); sd(COMP)


### Exposure variables - As described in the pre-reg doc, we're interested in the joint effect of two supernatural belief factors (punishment [2 vars] and omniscience [2 vars]) for Pachamama local god - All individual variables are binary, and will simulate them as generating from an unmeasured latent religiosity factor
LG_U <- rnorm(n = n, mean = 0, sd = 1)
summary(LG_U); sd(LG_U)

### Next simulate the individual variables, including causation from confounders. For ease, will simulate on natural probability scale

## Punishment variables

# Ever punishes for behaviour
LGPUNISH <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGPUNISH)

# Influences in afterlife
LGDIE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGDIE)

# Average these variables together
LGPUN_AVE <- (LGPUNISH + LGDIE) / 2
table(LGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
LGFEEL <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGFEEL)

# Knows what people are doing far away
LGSEE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGSEE)

# Average these variables together
LGOMNI_AVE <- (LGFEEL + LGSEE) / 2
table(LGOMNI_AVE)



### Outcome model for RAG decisions (number of coins donated to distant, out of 30) - For simplicity here, will assume additive relationship for religious belief variables
RAG <- rbinom(n = n, size = 30, prob = 0.3 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE) +
                (0.02 * AGE) + (-0.1 * SEX) + (0.01 * FORMALED) + (-0.1 * MAT) + (-0.03 * CHILDREN) +
                (0.05 * ORDER) + (-0.05 * COMP) + (0.1 * MARKET))
summary(RAG)
hist(RAG, breaks = 30)
table(RAG)



##################################################################################
#### Run model in BRMS

## Put data into list (Note: this treats categorical variables as indicator variables, rather than index variables)
dat <- list(
  RAG = RAG,
  LGPUN_AVE = LGPUN_AVE,
  LGOMNI_AVE = LGOMNI_AVE,
  AGE = AGE,
  SEX = SEX,
  FORMALED = FORMALED,
  MAT = MAT,
  CHILDREN = CHILDREN,
  MARKET = MARKET,
  ORDER = ORDER,
  COMP = COMP,
  N = as.integer(n)
)

str(dat)
summary(as.data.frame(dat))


### BRMS model
mod <- brm(RAG | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
             AGE + SEX + FORMALED + MAT + CHILDREN + MARKET +
             ORDER + COMP,
           family = "binomial",
           data = dat,
           prior = c(prior(normal(0, 1), class = b),
                     prior(normal(0, 2), class = Intercept)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 54321)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
summary(mod)
#plot(mod)


## Model fit values
mod <- add_criterion(mod, "loo")
loo_add <- loo(mod)
loo_add


## Model with interaction between exposures
mod_int <- brm(RAG | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE + 
             AGE + SEX + FORMALED + MAT + CHILDREN + MARKET +
             ORDER + COMP,
           family = "binomial",
           data = dat,
           prior = c(prior(normal(0, 1), class = b),
                     prior(normal(0, 2), class = Intercept)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12345)


## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
summary(mod_int)
#plot(mod_int)

## Model fit values
mod_int <- add_criterion(mod_int, "loo")
loo_int <- loo(mod_int)
loo_int


## Compare additive and interaction models - No real improvement with inclusion of interaction terms (as expected, given data were simulated as additive)
loo_compare(loo_add, loo_int)



### G-computation (based on additive model)

## Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                         AGE = AGE, SEX = SEX, FORMALED = FORMALED, 
                         MAT = MAT, CHILDREN = CHILDREN, MARKET = MARKET,
                         ORDER = ORDER, COMP = COMP))
head(df_sup0)

sup0 <- predict(mod, newdata = df_sup0, summary = FALSE)
head(sup0)


## Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, 
                               MAT = MAT, CHILDREN = CHILDREN, MARKET = MARKET,
                               ORDER = ORDER, COMP = COMP))
head(df_sup1)

sup1 <- predict(mod, newdata = df_sup1, summary = FALSE)
head(sup1)

## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  sup_diff[i] <- mean(sup1[i, ]) - mean(sup0[i, ])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))



### And on probability scale, to make sure that results match the simulation

## If belief = 0

# Extract results on log_odds scale
log0 <- fitted(mod, newdata = df_sup0, scale = "linear", summary = FALSE)
log0

# Convert to probabilities
prob0 <- plogis(log0)
prob0


## If belief = 1

# Extract results on log_odds scale
log1 <- fitted(mod, newdata = df_sup1, scale = "linear", summary = FALSE)
log1

# Convert to probabilities
prob1 <- plogis(log1)
prob1


## Difference between counter-factual state from each of the posterior samples
sup_diff_prob <- rep(NA, nrow(prob0))
for (i in 1:nrow(prob0)) {
  sup_diff_prob[i] <- mean(prob1[i, ]) - mean(prob0[i, ])
}

hist(sup_diff_prob)
summary(sup_diff_prob)
quantile(sup_diff_prob, c(0.025, 0.5, 0.975))

# This is about right (42%-point increase in donating to DISTANT), as this is close to the sum of the three terms from the linear probability model above when simulating this outcome (40%-points - "(0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE)")



###################################################################################################
##### Sensitivity analyses

### Sensitivity analysis: Inclusion of variables which could be either confounders and/or mediators - i.e., supernatural beliefs and rituals regarding moralistic god (Catholic god), and Pachamama rituals. For simplicity here, will just assume that these are all mediators, rather than confounders

### Simulate data based on DAG in pre-reg document
set.seed(935)
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

# Market Integration
MARKET <- rnorm(n = n, mean = 0.5, sd = 0.16)
summary(MARKET); sd(MARKET)


## Features of the game

# Order of games
ORDER <- rbinom(n = n, size = 1, prob = 0.5)
table(ORDER)

# Number of comprehension checks needed
COMP <- rpois(n = n, lambda = 1)
table(COMP)
summary(COMP); sd(COMP)


### Exposure variables - As described in the pre-reg doc, we're interested in the joint effect of two supernatural belief factors (punishment [2 vars] and omniscience [2 vars]) for Pachamama local gods - All individual variables are binary, and will simulate them as generating from an unmeasured latent religiosity factor
LG_U <- rnorm(n = n, mean = 0, sd = 1)
summary(LG_U); sd(LG_U)

### Next simulate the individual variables, including causation from confounders. For ease, will simulate on natural probability scale

## Punishment variables

# Ever punishes for behaviour
LGPUNISH <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGPUNISH)

# Influences in afterlife
LGDIE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGDIE)

# Average these variables together
LGPUN_AVE <- (LGPUNISH + LGDIE) / 2
table(LGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
LGFEEL <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                   (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGFEEL)

# Knows what people are doing far away
LGSEE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN) + (0.1 * MARKET))
table(LGSEE)

# Average these variables together
LGOMNI_AVE <- (LGFEEL + LGSEE) / 2
table(LGOMNI_AVE)


### Mediator variables (assume just caused by exposures and independent of confounders)

## Catholic God (MG) beliefs

## Punishment variables

# Ever punishes for behaviour
MGPUNISH <- rbinom(n = n, size = 1, prob = 0.5 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE))
table(MGPUNISH)

# Influences in afterlife
MGDIE <- rbinom(n = n, size = 1, prob = 0.5 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE))
table(MGDIE)

# Average these variables together
MGPUN_AVE <- (MGPUNISH + MGDIE) / 2
table(MGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
MGFEEL <- rbinom(n = n, size = 1, prob = 0.4 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE))
table(MGFEEL)

# Knows what people are doing far away
MGSEE <- rbinom(n = n, size = 1, prob = 0.4 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE))
table(MGSEE)

# Average these variables together
MGOMNI_AVE <- (MGFEEL + MGSEE) / 2
table(MGOMNI_AVE)


## Catholic rituals (Likert scale from 0 to 4; will model using an aggregated binomial model, just for ease here)
MGRIT <- rbinom(n = n, size = 4, prob = 0.3 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE))
table(MGRIT)


## Pachamama rituals (Likert scale from 0 to 4; will model using an aggregated binomial model, just for ease here)
LGRIT <- rbinom(n = n, size = 4, prob = 0.3 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE))
table(LGRIT)


### Outcome model for RAG decisions (number of coins donated to distant, out of 30) - For simplicity here, will assume additive relationship for religious belief variables
RAG <- rbinom(n = n, size = 30, prob = 0.3 + (0.2 * LGPUN_AVE) + (0.2 * LGOMNI_AVE) +
                (0.02 * AGE) + (-0.1 * SEX) + (0.01 * FORMALED) + 
                (-0.1 * MAT) + (-0.03 * CHILDREN) + (0.1 * MARKET) +
                (0.05 * ORDER) + (-0.05 * COMP) + 
                (-0.1 * MGPUN_AVE) + (-0.1 * MGOMNI_AVE) + (0.05 * MGRIT) + (0.05 * LGRIT))
summary(RAG)
hist(RAG, breaks = 30)
table(RAG)


#### Run model in BRMS

## Put data into list
dat <- list(
  RAG = RAG,
  LGPUN_AVE = LGPUN_AVE,
  LGOMNI_AVE = LGOMNI_AVE,
  AGE = AGE,
  SEX = SEX,
  FORMALED = FORMALED,
  MAT = MAT,
  CHILDREN = CHILDREN,
  MARKET = MARKET,
  ORDER = ORDER,
  COMP = COMP,
  MGPUN_AVE = MGPUN_AVE,
  MGOMNI_AVE = MGOMNI_AVE,
  MGRIT = MGRIT,
  LGRIT = LGRIT,
  N = as.integer(n)
)

str(dat)
summary(as.data.frame(dat))


### BRMS model without confounders/mediators
mod <- brm(RAG | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
             AGE + SEX + FORMALED + MAT + CHILDREN + MARKET +
             ORDER + COMP,
           family = "binomial",
           data = dat,
           prior = c(prior(normal(0, 1), class = b),
                     prior(normal(0, 2), class = Intercept)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 54321)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
summary(mod)
#plot(mod)

### G-computation

## Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, 
                               MAT = MAT, CHILDREN = CHILDREN, MARKET = MARKET,
                               ORDER = ORDER, COMP = COMP))
head(df_sup0)

sup0 <- predict(mod, newdata = df_sup0, summary = FALSE)
head(sup0)


## Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, 
                               MAT = MAT, CHILDREN = CHILDREN, MARKET = MARKET,
                               ORDER = ORDER, COMP = COMP))
head(df_sup1)

sup1 <- predict(mod, newdata = df_sup1, summary = FALSE)
head(sup1)

## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  sup_diff[i] <- mean(sup1[i, ]) - mean(sup0[i, ])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))


### BRMS model with confounders/mediators
mod2 <- brm(RAG | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
             AGE + SEX + FORMALED + MAT + CHILDREN + MARKET +
             ORDER + COMP +
             MGPUN_AVE + MGOMNI_AVE + MGRIT + LGRIT,
           family = "binomial",
           data = dat,
           prior = c(prior(normal(0, 1), class = b),
                     prior(normal(0, 2), class = Intercept)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12321)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
summary(mod2)
#plot(mod2)

### G-computation

## Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, 
                               MAT = MAT, CHILDREN = CHILDREN, MARKET = MARKET,
                               ORDER = ORDER, COMP = COMP,
                               MGPUN_AVE = MGPUN_AVE, MGOMNI_AVE = MGOMNI_AVE, MGRIT = MGRIT, 
                               LGRIT = LGRIT))
head(df_sup0)

sup0 <- predict(mod2, newdata = df_sup0, summary = FALSE)
head(sup0)


## Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, 
                               MAT = MAT, CHILDREN = CHILDREN, MARKET = MARKET,
                               ORDER = ORDER, COMP = COMP,
                               MGPUN_AVE = MGPUN_AVE, MGOMNI_AVE = MGOMNI_AVE, MGRIT = MGRIT, 
                               LGRIT = LGRIT))
head(df_sup1)

sup1 <- predict(mod2, newdata = df_sup1, summary = FALSE)
head(sup1)

## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  sup_diff[i] <- mean(sup1[i, ]) - mean(sup0[i, ])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))


## Can easily extend this to include all combinations of mediators/confounders, but haven't shown that here for simplicity.



