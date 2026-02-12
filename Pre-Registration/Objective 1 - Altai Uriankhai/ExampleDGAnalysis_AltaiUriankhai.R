### Example code for Attila's Altai Uriankhai paper - Ordinal DG models
### Created 2/12/2024 by Dan Major-Smith
### R version 4.4.1


## Install and load packages
rm(list=ls())
Sys.setenv(LANG = "en")

# For BRMS, make sure that Stan is installed
#install.packages("brms")
#install.packages("mice")

library(brms)
library(mice)


### Simulate data based on DAG in pre-reg document
set.seed(42)
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


## Compare additive and interaction models - No improvement with inclusion of interaction terms (as expected, given data were simulated as additive)
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

# These are broadly as simulated



##############################################################################################
#### Next to extend this to include missing data and multiple imputation

### Is actually relatively easy - Just run the model in each imputed dataset, store the posterior probabilities for each level of the marginal contrast in each, then combine all posterior probabilities together and sample the difference/contrasts from these.


### Add some missing data to some of the religious belief variables (here, just variables of the punishment and omniscience scales)
set.seed(23432)

LGPUNISH_M <- ifelse(rbinom(n = n, size = 1, prob = 0.05) == 1,
                     NA, LGPUNISH)
table(LGPUNISH_M, useNA = "ifany")

LGDIE_M <- ifelse(rbinom(n = n, size = 1, prob = 0.05) == 1,
                  NA, LGDIE)
table(LGDIE_M, useNA = "ifany")

LGPUN_AVE_M <- rowMeans(matrix(c(LGPUNISH_M, LGDIE_M), ncol = 2), na.rm = FALSE)
table(LGPUN_AVE_M, useNA = "ifany")

LGFEEL_M <- ifelse(rbinom(n = n, size = 1, prob = 0.05) == 1,
                   NA, LGFEEL)
table(LGFEEL_M, useNA = "ifany")

LGSEE_M <- ifelse(rbinom(n = n, size = 1, prob = 0.05) == 1,
                  NA, LGSEE)
table(LGSEE_M, useNA = "ifany")

LGOMNI_AVE_M <- rowMeans(matrix(c(LGFEEL_M, LGSEE_M), ncol = 2), na.rm = FALSE)
table(LGOMNI_AVE_M, useNA = "ifany")


## Combine data into a dataframe
dat <- as.data.frame(cbind(DG = DG,
                           LGPUNISH_M = LGPUNISH_M,
                           LGDIE_M = LGDIE_M,
                           LGPUN_AVE_M = LGPUN_AVE_M,
                           LGFEEL_M = LGFEEL_M,
                           LGSEE_M = LGSEE_M,
                           LGOMNI_AVE_M = LGOMNI_AVE_M,
                           AGE = AGE,
                           SEX = SEX,
                           FORMALED = FORMALED,
                           MAT = MAT,
                           CHILDREN = CHILDREN,
                           ORDER = ORDER,
                           COMP = COMP))

# And code binary variables as factors (for MICE to work with logistic regression) - Plus DG outcome
dat$DG <- factor(dat$DG, ordered = TRUE)
dat$LGPUNISH_M <- as.factor(dat$LGPUNISH_M)
dat$LGDIE_M <- as.factor(dat$LGDIE_M)
dat$LGFEEL_M <- as.factor(dat$LGFEEL_M)
dat$LGSEE_M <- as.factor(dat$LGSEE_M)
dat$SEX <- as.factor(dat$SEX)
dat$ORDER <- as.factor(dat$ORDER)

head(dat)
summary(dat)
str(dat)


## Set up imputations - Change 'LGPUN_AVE_M' and 'LGOMNI_AVE_M' from PMM to passively impute (as is just based on means of two prior variables)
meth <- make.method(dat)
meth["LGPUN_AVE_M"] <- "~ I(((as.numeric(LGPUNISH_M) - 1) + (as.numeric(LGDIE_M) - 1)) / 2)"
meth["LGOMNI_AVE_M"] <- "~ I(((as.numeric(LGFEEL_M) - 1) + (as.numeric(LGSEE_M) - 1)) / 2)"
meth

# Predictor matrix - Exclude punishment and omniscience scores as predictors
pred <- make.predictorMatrix(dat)
pred[, "LGPUN_AVE_M"] <- 0
pred[, "LGOMNI_AVE_M"] <- 0
pred

# Visit sequence - Make sure passively-imputed scales are after the individual variables (they are)
visit <- make.visitSequence(dat)
visit

# Test imputation to check formulas and that no obvious errors
test <- mice(dat, m = 5, maxit = 0, method = meth, predictorMatrix = pred, visitSequence = visit)
test$formulas


## Run imputations - 20 imputations as approx. 20% cases with missing data
imp <- mice(dat, m = 20, maxit = 5, 
            method = meth, predictorMatrix = pred, visitSequence = visit,
            seed = 5432, print = TRUE)


## Check imputations worked correctly
imp1 <- complete(imp, 1)

head(dat, n = 30L)
head(imp1, n = 30L)


### Now run data through BRMS for each imputed dataset (see https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html)
mod_mi <- brm_multiple(DG ~ LGPUN_AVE_M + LGOMNI_AVE_M +
                         AGE + SEX + FORMALED + MAT + CHILDREN +
                         ORDER + COMP,
                       family = cumulative("logit"),
                       data = imp,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 3456)

summary(mod_mi)


## Model fit values (note that is just based on first imputed dataset, which will assume is representative of all imputed datasets)
mod_mi <- add_criterion(mod_mi, "loo")
loo_add <- loo(mod_mi)
loo_add


## To note: r-hat and effective sample size values may indicate poor fitting and convergence issues, but this is because different imputed datasets have different values, so may not give exactly the same results - This is completely normal (and to be expected with MI!), so is likely a false positive. Still, worth checking that each individual chain converged, just to make sure (although in this example they are mostly fine)

# Overall plot of chains
#plot(mod_mi, regex = TRUE)

# Extract stats per chain
library(posterior)
draws <- as_draws_array(mod_mi)
num_chains <- nchains(mod_mi) / imp$m
draws_per_dat <- lapply(1:imp$m,
                        \(i) subset_draws(draws, 
                                          chain = ((i - 1) * num_chains + 1):(i * num_chains))
)
per_dat <- lapply(draws_per_dat, summarize_draws, default_convergence_measures())

# These look absolutely fine
for (i in 1:length(per_dat)) {
  print(paste0("On results for imputed dataset: ", i))
  print(per_dat[[i]], n = nrow(per_dat[[i]]))
}


## Model with interaction between exposures
mod_mi_int <- brm_multiple(DG ~ LGPUN_AVE_M + LGOMNI_AVE_M + LGPUN_AVE_M:LGOMNI_AVE_M + 
                             AGE + SEX + FORMALED + MAT + CHILDREN +
                             ORDER + COMP,
                           family = cumulative("logit"),
                           data = imp,
                           prior = c(prior(normal(0, 1), class = b),
                                     prior(normal(0, 2), class = Intercept)),
                           iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 6543)

summary(mod_mi_int)


## Model fit values (again, note that is just based on first imputed dataset, which will assume is representative of all imputed datasets)
mod_mi_int <- add_criterion(mod_mi_int, "loo")
loo_int <- loo(mod_mi_int)
loo_int


## Compare additive and interaction models - No improvement with inclusion of interaction terms (as expected, given data were simulated as additive)
loo_compare(loo_add, loo_int)


#### G-computation with MI (using additive model)

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Extract imputed datasets
imp_all <- complete(imp, action = "all")
str(imp_all)

# Set up lists to store results in
sup0 <- list()
sup1 <- list()
log0 <- list()
log1 <- list()
prob0 <- list()
prob1 <- list()

# Set seed and loop over each imputed dataset
set.seed(987)

for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith imputed dataset
  df_temp <- imp_all[[i]]
  
  # Run model on ith imputed dataset
  mod_temp <- brm(DG ~ LGPUN_AVE_M + LGOMNI_AVE_M +
                    AGE + SEX + FORMALED + MAT + CHILDREN +
                    ORDER + COMP,
                  family = cumulative("logit"),
                  data = df_temp,
                  prior = c(prior(normal(0, 1), class = b),
                            prior(normal(0, 2), class = Intercept)),
                  iter = 2000, warmup = 1000, chains = 4, cores = 4)
  
  # Predict outcome where all supernatural belief variables are '0' for ith dataset (Note: have to convert factor variables back to factors with correct levels)
  df_sup0_temp <- as.data.frame(cbind(LGPUN_AVE_M = 0, LGOMNI_AVE_M = 0,
                                      AGE = df_temp$AGE, SEX = df_temp$SEX, FORMALED = df_temp$FORMALED, 
                                      MAT = df_temp$MAT, CHILDREN = df_temp$CHILDREN,
                                      ORDER = df_temp$ORDER, COMP = df_temp$COMP))
  
  df_sup0_temp$SEX <- df_sup0_temp$SEX - 1
  df_sup0_temp$SEX <- factor(df_sup0_temp$SEX, levels = c("0", "1"))
  df_sup0_temp$ORDER <- df_sup0_temp$ORDER - 1
  df_sup0_temp$ORDER <- factor(df_sup0_temp$ORDER, levels = c("0", "1"))
  
  sup0_temp <- predict(mod_temp, newdata = df_sup0_temp, summary = FALSE)
  sup0[[i]] <- sup0_temp
  
  # And on log-odds and probability scales
  log0_temp <- fitted(mod_temp, newdata = df_sup0_temp, scale = "linear", summary = FALSE)
  prob0_temp <- plogis(log0_temp)
  log0[[i]] <- log0_temp
  prob0[[i]] <- prob0_temp
  
  # Predict outcome where all supernatural belief variables are '1' for ith dataset (Note: have to convert factor variables back to factors with correct levels)
  df_sup1_temp <- as.data.frame(cbind(LGPUN_AVE_M = 1, LGOMNI_AVE_M = 1,
                                      AGE = df_temp$AGE, SEX = df_temp$SEX, FORMALED = df_temp$FORMALED, 
                                      MAT = df_temp$MAT, CHILDREN = df_temp$CHILDREN,
                                      ORDER = df_temp$ORDER, COMP = df_temp$COMP))
  
  df_sup1_temp$SEX <- df_sup1_temp$SEX - 1
  df_sup1_temp$SEX <- factor(df_sup1_temp$SEX, levels = c("0", "1"))
  df_sup1_temp$ORDER <- df_sup1_temp$ORDER - 1
  df_sup1_temp$ORDER <- factor(df_sup1_temp$ORDER, levels = c("0", "1"))
  
  sup1_temp <- predict(mod_temp, newdata = df_sup1_temp, summary = FALSE)
  sup1[[i]] <- sup1_temp
  
  # And on log-odds and probability scales
  log1_temp <- fitted(mod_temp, newdata = df_sup1_temp, scale = "linear", summary = FALSE)
  prob1_temp <- plogis(log1_temp)
  log1[[i]] <- log1_temp
  prob1[[i]] <- prob1_temp
  
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
sup_diff <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  sup_diff[i] <- mean(sup1_all[i, ]) - mean(sup0_all[i, ])
}

hist(sup_diff)
summary(sup_diff)
quantile(sup_diff, c(0.025, 0.5, 0.975))



## And on probability scale
prob0_all <- do.call(rbind, prob0)
prob1_all <- do.call(rbind, prob1)

# Difference between counter-factual states from each of the posterior samples
prob_diff <- rep(NA, nrow(prob0_all))
for (i in 1:nrow(prob0_all)) {
  prob_diff[i] <- mean(prob1_all[i, ]) - mean(prob0_all[i, ])
}

hist(prob_diff)
summary(prob_diff)
quantile(prob_diff, c(0.025, 0.5, 0.975))


## And on log-odds scale
log0_all <- do.call(rbind, log0)
log1_all <- do.call(rbind, log1)

# Difference between counter-factual states from each of the posterior samples
log_diff <- rep(NA, nrow(log0_all))
for (i in 1:nrow(log0_all)) {
  log_diff[i] <- mean(log1_all[i, ]) - mean(log0_all[i, ])
}

hist(log_diff)
summary(log_diff)
quantile(log_diff, c(0.025, 0.5, 0.975))



### Process would be the same for other model specification (i.e., addition of potential moralistic god confounders, using just one punishment and omniscience variable rather than a mini-scale)



