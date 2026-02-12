### Example code for Byamba's Buryat paper - Ordinal DG models
### Created 29/01/2025 by Dan Major-Smith
### R version 4.4.1


## Install and load packages
rm(list=ls())
Sys.setenv(LANG = "en")

# For BRMS, make sure that Stan is installed
#install.packages("brms")

library(brms)


### Simulate data based on DAG in pre-reg document
set.seed(567)
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


## Features of the game

# Order of games
ORDER <- rbinom(n = n, size = 1, prob = 0.5)
table(ORDER)

# Number of comprehension checks needed
COMP <- rpois(n = n, lambda = 1)
table(COMP)
summary(COMP); sd(COMP)


### Exposure variables - As described in the pre-reg doc, we're interested in the joint effect of two supernatural belief factors (punishment [2 vars] and omniscience [2 vars]) for 'spirit-master' local gods - All individual variables are binary, and will simulate them as generating from an unmeasured latent religiosity factor
LG_U <- rnorm(n = n, mean = 0, sd = 1)
summary(LG_U); sd(LG_U)

### Next simulate the individual variables, including causation from confounders. For ease, will simulate on natural probability scale

## Punishment variables

# Ever punishes for behaviour
LGPUNISH <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN))
table(LGPUNISH)

# Influences in afterlife
LGDIE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN))
table(LGDIE)

# Average these variables together
LGPUN_AVE <- (LGPUNISH + LGDIE) / 2
table(LGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
LGFEEL <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                     (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN))
table(LGFEEL)

# Knows what people are doing far away
LGSEE <- rbinom(n = n, size = 1, prob = 0.6 + (0.15 * LG_U) + (0.05 * AGE) + (-0.15 * SEX) + 
                  (-0.01 * FORMALED) + (0.05 * MAT) + (0.02 * CHILDREN))
table(LGSEE)

# Average these variables together
LGOMNI_AVE <- (LGFEEL + LGSEE) / 2
table(LGOMNI_AVE)



### Outcome model for DG decisions (number of coins donated to distant, out of 10) - For simplicity here, will assume additive relationship for religious belief variables

# Set the intercept parameters (want 11 responses, so 10 parameters)
b01 <- 1
b12 <- 0.5
b23 <- 0
b34 <- -0.5
b45 <- -1
b56 <- -1.5
b67 <- -2
b78 <- -2.5
b89 <- -3
b910 <- -3.5

# Cumulative probabilities
prob01 <- plogis(b01 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob12 <- plogis(b12 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob23 <- plogis(b23 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob34 <- plogis(b34 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob45 <- plogis(b45 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob56 <- plogis(b56 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob67 <- plogis(b67 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob78 <- plogis(b78 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob89 <- plogis(b89 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                   (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                   (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))
prob910 <- plogis(b910 + (log(2) * LGPUN_AVE) + (log(1.5) * LGOMNI_AVE) + 
                    (log(1.1) * AGE) + (log(0.5) * SEX) + (log(1.1) * FORMALED) + (log(0.8) * MAT) + 
                    (log(0.95) * CHILDREN) + (log(1.2) * ORDER) + (log(0.9) * COMP))

# Use subtraction to get probability of each category
prob0 <- 1 - prob01
prob1 <- prob01 - prob12
prob2 <- prob12 - prob23
prob3 <- prob23 - prob34
prob4 <- prob34 - prob45
prob5 <- prob45 - prob56
prob6 <- prob56 - prob67
prob7 <- prob67 - prob78
prob8 <- prob78 - prob89
prob9 <- prob89 - prob910
prob10 <- prob910

# Randomly sample from these predicted probabilities to get actual number of gifts shared
DG <- c()
for (i in 1:n) {
  DG[i] <- sample(
    x = c(0:10), 
    size = 1, 
    prob = c(prob0[i], prob1[i], prob2[i], prob3[i], prob4[i], prob5[i],
             prob6[i], prob7[i], prob8[i], prob9[i], prob10[i])
  )
}
table(DG)

# Make this an ordered factor
DG <- factor(DG, ordered = TRUE)



##################################################################################
#### Run model in BRMS

## Put data into list (Note: this treats categorical variables as indicator variables, rather than index variables)
dat <- list(
  DG = DG,
  LGPUN_AVE = LGPUN_AVE,
  LGOMNI_AVE = LGOMNI_AVE,
  AGE = AGE,
  SEX = SEX,
  FORMALED = FORMALED,
  MAT = MAT,
  CHILDREN = CHILDREN,
  ORDER = ORDER,
  COMP = COMP,
  N = as.integer(n)
)

str(dat)
summary(as.data.frame(dat))


### BRMS model
mod <- brm(DG ~ LGPUN_AVE + LGOMNI_AVE +
             AGE + SEX + FORMALED + MAT + CHILDREN +
             ORDER + COMP,
           family = cumulative("logit"),
           data = dat,
           prior = c(prior(normal(0, 1), class = b),
                     prior(normal(0, 2), class = Intercept)),
           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 87654)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots). The coefficients are quite far from the true values, but this is just sampling error due to the low n (if increase from 100 to 10,000, then coefficients match the 'true' values)
summary(mod)
#plot(mod)

## Model fit values
mod <- add_criterion(mod, "loo")
loo_add <- loo(mod)
loo_add


## Model with interaction between exposures
mod_int <- brm(DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE + 
                 AGE + SEX + FORMALED + MAT + CHILDREN +
                 ORDER + COMP,
               family = cumulative("logit"),
               data = dat,
               prior = c(prior(normal(0, 1), class = b),
                         prior(normal(0, 2), class = Intercept)),
               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 456)

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
                         AGE = AGE, SEX = SEX, FORMALED = FORMALED, MAT = MAT, CHILDREN = CHILDREN,
                         ORDER = ORDER, COMP = COMP))
head(df_sup0)

sup0 <- predict(mod, newdata = df_sup0, summary = FALSE)
head(sup0)


## Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, MAT = MAT, CHILDREN = CHILDREN,
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



### And on log-odds and probability scales, to make sure that results match the simulation

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

# For log-odds
sup_diff_logodds <- rep(NA, nrow(log0))
for (i in 1:nrow(log0)) {
  sup_diff_logodds[i] <- mean(log1[i, ]) - mean(log0[i, ])
}

hist(sup_diff_logodds)
summary(sup_diff_logodds)
quantile(sup_diff_logodds, c(0.025, 0.5, 0.975))


# For probabilities
sup_diff_prob <- rep(NA, nrow(prob0))
for (i in 1:nrow(prob0)) {
  sup_diff_prob[i] <- mean(prob1[i, ]) - mean(prob0[i, ])
}

hist(sup_diff_prob)
summary(sup_diff_prob)
quantile(sup_diff_prob, c(0.025, 0.5, 0.975))


###################################################################################################
##### Sensitivity analyses

### Sensitivity analysis 1: Inclusion of supernatural beliefs regarding moralistic god (Buddha). For simplicity here, will just assume these are correlated with local spirit-master beliefs, but do not cause the outcome (cooperation towards distant oboo co-religionists)
set.seed(18241)

## Punishment variables

# Ever punishes for behaviour
MGPUNISH <- rbinom(n = n, size = 1, prob = 0.2 + (0.6 * LGPUNISH))
table(MGPUNISH)
table(LGPUNISH, MGPUNISH)

# Influences in afterlife
MGDIE <- rbinom(n = n, size = 1, prob = 0.2 + (0.6 * LGDIE))
table(MGDIE)
table(LGDIE, MGDIE)

# Average these variables together
MGPUN_AVE <- (MGPUNISH + MGDIE) / 2
table(MGPUN_AVE)
table(LGPUN_AVE, MGPUN_AVE)


## Omniscience variables

# Can know thoughts/feelings
MGFEEL <- rbinom(n = n, size = 1, prob = 0.2 + (0.6 * LGFEEL))
table(MGFEEL)
table(LGFEEL, MGFEEL)

# Knows what people are doing far away
MGSEE <- rbinom(n = n, size = 1, prob = 0.2 + (0.6 * LGSEE))
table(MGSEE)
table(LGSEE, MGSEE)

# Average these variables together
MGOMNI_AVE <- (MGFEEL + MGSEE) / 2
table(MGOMNI_AVE)
table(LGOMNI_AVE, MGOMNI_AVE)


### Run model in BRMS, now also including the moralistic supernatural belief variables

## Put data into list
dat_sens1 <- list(
  DG = DG,
  LGPUN_AVE = LGPUN_AVE,
  LGOMNI_AVE = LGOMNI_AVE,
  MGPUN_AVE = MGPUN_AVE,
  MGOMNI_AVE = MGOMNI_AVE,
  AGE = AGE,
  SEX = SEX,
  FORMALED = FORMALED,
  MAT = MAT,
  CHILDREN = CHILDREN,
  ORDER = ORDER,
  COMP = COMP,
  N = as.integer(n)
)

str(dat_sens1)
summary(as.data.frame(dat_sens1))


## Additive BRMS model
mod_sens1 <- brm(DG ~ LGPUN_AVE + LGOMNI_AVE +
                   MGPUN_AVE + MGOMNI_AVE +
                   AGE + SEX + FORMALED + MAT + CHILDREN +
                   ORDER + COMP,
                 family = cumulative("logit"),
                 data = dat_sens1,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 76567)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
summary(mod_sens1)
#plot(mod_sens1)


### G-computation

## Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0, 
                               MGPUN_AVE = MGPUN_AVE, MGOMNI_AVE = MGOMNI_AVE, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, MAT = MAT, CHILDREN = CHILDREN,
                               ORDER = ORDER, COMP = COMP))
head(df_sup0)

sup0 <- predict(mod_sens1, newdata = df_sup0, summary = FALSE)
head(sup0)


## Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1, 
                               MGPUN_AVE = MGPUN_AVE, MGOMNI_AVE = MGOMNI_AVE, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, MAT = MAT, CHILDREN = CHILDREN,
                               ORDER = ORDER, COMP = COMP))
head(df_sup1)

sup1 <- predict(mod_sens1, newdata = df_sup1, summary = FALSE)
head(sup1)

## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  sup_diff[i] <- mean(sup1[i, ]) - mean(sup0[i, ])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))



### And on log-odds and probability scales, to make sure that results match the simulation

## If belief = 0

# Extract results on log_odds scale
log0 <- fitted(mod_sens1, newdata = df_sup0, scale = "linear", summary = FALSE)
log0

# Convert to probabilities
prob0 <- plogis(log0)
prob0


## If belief = 1

# Extract results on log_odds scale
log1 <- fitted(mod_sens1, newdata = df_sup1, scale = "linear", summary = FALSE)
log1

# Convert to probabilities
prob1 <- plogis(log1)
prob1


## Difference between counter-factual state from each of the posterior samples

# For log-odds
sup_diff_logodds <- rep(NA, nrow(log0))
for (i in 1:nrow(log0)) {
  sup_diff_logodds[i] <- mean(log1[i, ]) - mean(log0[i, ])
}

hist(sup_diff_logodds)
summary(sup_diff_logodds)
quantile(sup_diff_logodds, c(0.025, 0.5, 0.975))


# For probabilities
sup_diff_prob <- rep(NA, nrow(prob0))
for (i in 1:nrow(prob0)) {
  sup_diff_prob[i] <- mean(prob1[i, ]) - mean(prob0[i, ])
}

hist(sup_diff_prob)
summary(sup_diff_prob)
quantile(sup_diff_prob, c(0.025, 0.5, 0.975))



##### Sensitivity analysis 2: Using single, rather than composite, variables for supernatural punishment and omniscience.

## Put data into list (Note: this treats categorical variables as indicator variables, rather than index variables)
dat_sens2 <- list(
  DG = DG,
  LGPUNISH = LGPUNISH,
  LGFEEL = LGFEEL,
  AGE = AGE,
  SEX = SEX,
  FORMALED = FORMALED,
  MAT = MAT,
  CHILDREN = CHILDREN,
  ORDER = ORDER,
  COMP = COMP,
  N = as.integer(n)
)

str(dat_sens2)
summary(as.data.frame(dat_sens2))


## BRMS model
mod_sens2 <- brm(DG ~ LGPUNISH + LGFEEL +
                   AGE + SEX + FORMALED + MAT + CHILDREN +
                   ORDER + COMP,
                 family = cumulative("logit"),
                 data = dat_sens2,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 34567)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
summary(mod_sens2)
#plot(mod_sens2)


### G-computation

## Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, MAT = MAT, CHILDREN = CHILDREN,
                               ORDER = ORDER, COMP = COMP))
head(df_sup0)

sup0 <- predict(mod_sens2, newdata = df_sup0, summary = FALSE)
head(sup0)


## Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1, 
                               AGE = AGE, SEX = SEX, FORMALED = FORMALED, MAT = MAT, CHILDREN = CHILDREN,
                               ORDER = ORDER, COMP = COMP))
head(df_sup1)

sup1 <- predict(mod_sens2, newdata = df_sup1, summary = FALSE)
head(sup1)

## Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  sup_diff[i] <- mean(sup1[i, ]) - mean(sup0[i, ])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))



### And on log-odds and probability scales, to make sure that results match the simulation

## If belief = 0

# Extract results on log_odds scale
log0 <- fitted(mod_sens2, newdata = df_sup0, scale = "linear", summary = FALSE)
log0

# Convert to probabilities
prob0 <- plogis(log0)
prob0


## If belief = 1

# Extract results on log_odds scale
log1 <- fitted(mod_sens2, newdata = df_sup1, scale = "linear", summary = FALSE)
log1

# Convert to probabilities
prob1 <- plogis(log1)
prob1


## Difference between counter-factual state from each of the posterior samples

# For log-odds
sup_diff_logodds <- rep(NA, nrow(log0))
for (i in 1:nrow(log0)) {
  sup_diff_logodds[i] <- mean(log1[i, ]) - mean(log0[i, ])
}

hist(sup_diff_logodds)
summary(sup_diff_logodds)
quantile(sup_diff_logodds, c(0.025, 0.5, 0.975))


# For probabilities
sup_diff_prob <- rep(NA, nrow(prob0))
for (i in 1:nrow(prob0)) {
  sup_diff_prob[i] <- mean(prob1[i, ]) - mean(prob0[i, ])
}

hist(sup_diff_prob)
summary(sup_diff_prob)
quantile(sup_diff_prob, c(0.025, 0.5, 0.975))

