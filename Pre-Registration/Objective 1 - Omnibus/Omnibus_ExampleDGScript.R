### Example analysis code for GGSL omnibus paper - Ordinal DG models
### Created 25/9/2025 by Dan Major-Smith
### R version 4.4.1

## Will focus on just one version of the DG (e.g., SELF vs DISTANT) here, as analyses will be identical for other (i.e., LOCAL vs DISTANT)

## Clear workspace, and install and load packages
rm(list=ls())
Sys.setenv(LANG = "en")

# For BRMS, make sure that Stan is installed
#install.packages("brms")
#install.packages("mice")
#install.packages("tidyverse")
#install.packages("cobalt")
#install.packages("WeightIt")
#install.packages("posterior")

library(brms)
library(mice)
library(tidyverse)
library(cobalt)
library(WeightIt)
library(posterior)


### Simulate data based on DAG in doc
set.seed(41)
n <- 100 # Number of participants by site
n_sites <- 18 # Number of sites/societies


## Specify the site-specific coefficients to use

# Specify which sites are local vs moralising gods (1 = local)
local <- c(rep(0, 9), rep(1, 9))

# For exposures (log-odds scale) - Say that local gods punish less and are less omniscient, but otherwise relationship between confounders and beliefs is the same (albeit with variation by site)
a_x <- rnorm(n = n_sites, mean = 0 - local, sd = 0.5) # Intercept
b_agex <- rnorm(n = n_sites, mean = 0.5, sd = 0.5) # Slope for age (older = more religious)
b_malex <- rnorm(n = n_sites, mean = -0.5, sd = 0.25) # Slope for sex (males = less religious)
b_edux <- rnorm(n = n_sites, mean = -0.5, sd = 0.5) # Slope for education (more education = less religious)
b_secx <- rnorm(n = n_sites, mean = -0.25, sd = 0.5) # Slope for material security (more security = less religious)

# For outcomes (log-odds scale) - Say that the relationship between supernatural punishment and omniscience and cooperation is null in local god traditions, but positive in moralistic god traditions, but otherwise relationship between confounders and cooperation is the same (albeit with variation by site)
b_agey <- rnorm(n = n_sites, mean = 0.25, sd = 0.5) # Slope for age (older = more cooperative)
b_maley <- rnorm(n = n_sites, mean = 0, sd = 0.25) # Slope for sex (no overall relationship)
b_eduy <- rnorm(n = n_sites, mean = 0.5, sd = 0.5) # Slope for education (more education = more cooperative)
b_secy <- rnorm(n = n_sites, mean = 0.25, sd = 0.5) # Slope for material security (more security = more cooperative)
b_gamey <- rnorm(n = n_sites, mean = 0, sd = 0.5) # Slope for game order (no overall relationship)
b_puny <- rnorm(n = n_sites, mean = 1 - local , sd = 0.25) # slope for punishment (more punishment beliefs = more cooperative, but only in moralistic god traditions)
b_omniy <- rnorm(n = n_sites, mean = 1 - local, sd = 0.25) # slope for omniscience (more omniscience beliefs = more cooperative, but only in moralistic god traditions)

# Intercept parameters are a bit more faffy for ordinal models - As want 11 responses (from 0 to 10), need 10 parameters, and for this to vary between sites
a_y01 <- rnorm(n = n_sites, mean = 1, sd = 0.5)
a_y12 <- a_y01 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y23 <- a_y12 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y34 <- a_y23 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y45 <- a_y34 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y56 <- a_y45 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y67 <- a_y56 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y78 <- a_y67 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y89 <- a_y78 - runif(n = n_sites, min = 0.25, max = 0.75)
a_y910 <- a_y89 - runif(n = n_sites, min = 0.25, max = 0.75)


## Simulate data for each society independently and store in list
dat <- list()

for (i in 1:n_sites) {
  
  print(paste0("On society: ", i))
  
  # Society ID
  soc_id <- i
  
  # Participant ID (with different IDs per society)
  id <- seq(from = (i * 100), to = (i * 100) + (n - 1), length.out = n)
  
  # Moralistic or local god tradition (local = 1)
  LG <- rep(local[i], n)
  
  # Continuous confounder of age (standardised)
  Age <- rnorm(n = n, mean = 0, sd = 1)
  
  # Binary confounder of sex (Male = 1)
  Male <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Number of years education (standardised)
  Edu <- rnorm(n = n, mean = 0, sd = 1)
  
  # Material security (Re-scaled to between 0 and 1)
  FoodSec <- rnorm(n = n, mean = 0, sd = 1)
  FoodSec <- FoodSec + (-min(FoodSec))
  FoodSec <- FoodSec / max(FoodSec)
  
  # Game order
  Order <- rbinom(n = n, size = 1, prob = 0.5)
  
  # Supernatural punishment exposures (ever punish and affects afterlife)
  Pun <- rbinom(n = n, size = 1, prob = plogis(a_x[i] + (b_agex[i] * Age) + (b_malex[i] * Male) + 
                                                 (b_edux[i] * Edu) + (b_secx[i] * FoodSec)))
  Die <- rbinom(n = n, size = 1, prob = plogis(a_x[i] + (b_agex[i] * Age) + (b_malex[i] * Male) + 
                                                 (b_edux[i] * Edu) + (b_secx[i] * FoodSec)))
  
  # Combine together for punishment mini-scale
  PunAve <- (Pun + Die) / 2
  
  # Add average for Mundlak model
  PunAve_bar <- mean(PunAve)
  
  # Supernatural omniscience exposure (knows thoughts/feelings and can see far away)
  Know <- rbinom(n = n, size = 1, prob = plogis(a_x[i] + (b_agex[i] * Age) + (b_malex[i] * Male) + 
                                                  (b_edux[i] * Edu) + (b_secx[i] * FoodSec)))
  See <- rbinom(n = n, size = 1, prob = plogis(a_x[i] + (b_agex[i] * Age) + (b_malex[i] * Male) + 
                                                 (b_edux[i] * Edu) + (b_secx[i] * FoodSec)))
  
  # Combine together for omniscience mini-scale
  OmniAve <- (Know + See) / 2
  
  # Add average for Mundlak model
  OmniAve_bar <- mean(OmniAve)
  
  ## DG cooperation outcome (ordinal value between 0 and 10)
  
  # First, calculate cumulative probabilities based on intercept values from above
  prob01 <- plogis(a_y01[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob12 <- plogis(a_y12[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob23 <- plogis(a_y23[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob34 <- plogis(a_y34[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob45 <- plogis(a_y45[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob56 <- plogis(a_y56[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob67 <- plogis(a_y67[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob78 <- plogis(a_y78[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob89 <- plogis(a_y89[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  prob910 <- plogis(a_y910[i] + (b_agey[i] * Age) + (b_maley[i] * Male) + (b_eduy[i] * Edu) + 
                     (b_secy[i] * FoodSec) + (b_gamey[i] * Order) + 
                     (b_puny[i] * PunAve) + (b_omniy[i] * OmniAve))
  
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
  for (j in 1:n) {
    DG[j] <- sample(x = c(0:10), size = 1, prob = c(prob0[j], prob1[j], prob2[j], prob3[j], prob4[j], prob5[j], 
                                                    prob6[j], prob7[j], prob8[j], prob9[j], prob10[j]))
  }
  
  # Bring data together and store in list
  temp <- data.frame(soc_id, id, LG, Age, Male, Edu, FoodSec, Order, Pun, Die, PunAve, PunAve_bar, Know, See, OmniAve, OmniAve_bar, DG)
  dat[[i]] <- temp
  
}


# Check list of data and combine into a single data frame
str(dat)
dat_df <- do.call(rbind.data.frame, dat)
summary(dat_df)


## Sanity check of data - Moralistic groups are more cooperative overall, and have greater supernatural punishment and omniscience. Great!
by(dat_df$PunAve, dat_df$LG, summary)
by(dat_df$OmniAve, dat_df$LG, summary)
by(dat_df$DG, dat_df$LG, summary)

# Make DG an ordered outcome (need either this or positive integers for model to run)
dat_df$DG <- factor(dat_df$DG, ordered = TRUE)


### Check positivity in each society (will just show one for demo purposes here, as steps are identical for all other societies)
soc1 <- subset(dat_df, soc_id == 1)
head(soc1)

### We want the distributions of covariate values to overlap/there is variation in the covariate for all exposure values

## Punishment 
pun_weight <- WeightIt::weightit(as.factor(PunAve) ~ Age + Male + Edu + FoodSec + Order,
                                 data = soc1, method = "glm", estimand = "ATE")

# Plot of covariate balance across all covariates (both unadjusted and adjusted models)
bal.plot(pun_weight, var.name = "Age", which = "unadjusted")
bal.plot(pun_weight, var.name = "Male", which = "unadjusted")
bal.plot(pun_weight, var.name = "Edu", which = "unadjusted")
bal.plot(pun_weight, var.name = "FoodSec", which = "unadjusted")
bal.plot(pun_weight, var.name = "Order", which = "unadjusted")

## Omniscience
omni_weight <- WeightIt::weightit(as.factor(OmniAve) ~ Age + Male + Edu + FoodSec + Order,
                                  data = soc1, method = "glm", estimand = "ATE")

# Plot of covariate balance across all covariates (both unadjusted and adjusted models)
bal.plot(omni_weight, var.name = "Age", which = "unadjusted")
bal.plot(omni_weight, var.name = "Male", which = "unadjusted")
bal.plot(omni_weight, var.name = "Edu", which = "unadjusted")
bal.plot(omni_weight, var.name = "FoodSec", which = "unadjusted")
bal.plot(omni_weight, var.name = "Order", which = "unadjusted")



################################################################################
#### Statistical models

### With fully-varying parameters and intercept-specific fixed effects by site, with site-specific averages of exposures as fixed-effects a la Mundlak models (takes about 10 minutes to run)
mod1 <- brm(DG | thres(10, gr = as.factor(soc_id)) ~ PunAve + OmniAve + Age + Male + Edu + FoodSec + Order + PunAve_bar + OmniAve_bar +
               (PunAve + OmniAve + Age + Male + Edu + FoodSec + Order | soc_id),
             family = cumulative("logit"), data = dat_df,
             prior = c(prior(normal(0, 1), class = b), # Priors for coefficient slopes
                       prior(normal(0, 2), class = Intercept), # Prior for intercept term
                       prior(exponential(1), class = sd, group = soc_id), # Priors for random intercepts
                       prior(lkj(2), class = cor, group = soc_id)), # Priors for correlation between random effects
             iter = 2000, warmup = 1000,
             chains = 4, cores = 4, seed = 15123)

options(max.print = 10000)
summary(mod1)
#plot(mod1)

# Add model fit, so can compare models
mod1 <- add_criterion(mod1, "loo")
loo1 <- loo(mod1)
loo1



## G-computation - First averaged over all sites (ignoring effect modification), then separately by moralistic vs local tradition

# Predict outcome where punishment and omniscience are '0'
df0 <- as.data.frame(cbind(PunAve = 0, OmniAve = 0, LG = dat_df$LG, Age = dat_df$Age, Male = dat_df$Male, Edu = dat_df$Edu, 
                           FoodSec = dat_df$FoodSec, PunAve_bar = dat_df$PunAve_bar, OmniAve_bar = dat_df$OmniAve_bar, 
                           Order = dat_df$Order, soc_id = dat_df$soc_id))
out0 <- predict(mod1, newdata = df0, summary = FALSE)

# Convert these back from factors to numbers of tokens
out0_temp <- out0
for(i in 1:length(attr(out0, "levels"))) {
  out0_temp[out0 == i] <- as.numeric(attr(out0, "levels")[i])
}
out0 <- out0_temp

# Predict outcome where punishment and omniscience are '1'
df1 <- as.data.frame(cbind(PunAve = 1, OmniAve = 1, LG = dat_df$LG, Age = dat_df$Age, Male = dat_df$Male, Edu = dat_df$Edu, 
                           FoodSec = dat_df$FoodSec, PunAve_bar = dat_df$PunAve_bar, OmniAve_bar = dat_df$OmniAve_bar, 
                           Order = dat_df$Order, soc_id = dat_df$soc_id))
out1 <- predict(mod1, newdata = df1, summary = FALSE)

# Convert these back from factors to numbers of tokens
out1_temp <- out1
for(i in 1:length(attr(out1, "levels"))) {
  out1_temp[out1 == i] <- as.numeric(attr(out1, "levels")[i])
}
out1 <- out1_temp

# Difference between counter-factual states based on posterior samples over whole sample
DG0 <- rep(NA, nrow(out0))
DG1 <- rep(NA, nrow(out0))
diff_DG <- rep(NA, nrow(out0))

for (i in 1:nrow(out0)) {
  DG0[i] <- mean(out0[i, ])
  DG1[i] <- mean(out1[i, ])
  diff_DG[i] <- DG1[i] - DG0[i]
}

quantile(DG0, c(0.025, 0.5, 0.975))
quantile(DG1, c(0.025, 0.5, 0.975))
quantile(diff_DG, c(0.025, 0.5, 0.975))

# Difference between counter-factual states based on posterior samples split by moralistic vs local tradition
DG0_LG <- rep(NA, nrow(out0))
DG1_LG <- rep(NA, nrow(out0))
diff_DG_LG <- rep(NA, nrow(out0))
DG0_MG <- rep(NA, nrow(out0))
DG1_MG <- rep(NA, nrow(out0))
diff_DG_MG <- rep(NA, nrow(out0))

for (i in 1:nrow(out0)) {
  DG0_LG[i] <- mean(out0[i, ][df0$LG == 1])
  DG1_LG[i] <- mean(out1[i, ][df0$LG == 1])
  diff_DG_LG[i] <- DG1_LG[i] - DG0_LG[i]
  
  DG0_MG[i] <- mean(out0[i, ][df0$LG == 0])
  DG1_MG[i] <- mean(out1[i, ][df0$LG == 0])
  diff_DG_MG[i] <- DG1_MG[i] - DG0_MG[i]
}

# Now get correct result again (i.e., no relationship in local sites, but positive relationship in moralistic sites)
quantile(DG0_LG, c(0.025, 0.5, 0.975))
quantile(DG1_LG, c(0.025, 0.5, 0.975))
quantile(diff_DG_LG, c(0.025, 0.5, 0.975))

quantile(DG0_MG, c(0.025, 0.5, 0.975))
quantile(DG1_MG, c(0.025, 0.5, 0.975))
quantile(diff_DG_MG, c(0.025, 0.5, 0.975))

quantile(diff_DG_MG - diff_DG_LG, c(0.025, 0.5, 0.975))


### Now same random-slopes model, but with interaction term between punishment and omniscience
mod1_int <- brm(DG | thres(10, gr = as.factor(soc_id)) ~ PunAve + OmniAve + PunAve:OmniAve + Age + Male + Edu + FoodSec + Order + PunAve_bar + OmniAve_bar +
               (PunAve + OmniAve + PunAve:OmniAve + Age + Male + Edu + FoodSec + Order | soc_id),
             family = cumulative("logit"), data = dat_df,
             prior = c(prior(normal(0, 1), class = b), # Priors for coefficient slopes
                       prior(normal(0, 2), class = Intercept), # Prior for intercept term
                       prior(exponential(1), class = sd, group = soc_id), # Priors for random intercepts
                       prior(lkj(2), class = cor, group = soc_id)), # Priors for correlation between random effects
             iter = 2000, warmup = 1000,
             chains = 4, cores = 4, seed = 15124)

summary(mod1_int)
#plot(mod1_int)

# Add model fit, so can compare models
mod1_int <- add_criterion(mod1_int, "loo")
loo1_int <- loo(mod1_int)
loo1_int

# Compare to base model without interaction term - No improvement in model fit here (as did not simulate interaction)
loo_compare(loo1, loo1_int)

## As no improvement in model fit, wont bother with g-computation here again (but the steps would be identical to above)



####################################################################################################################
##### Example of combining multiple imputation to handle any missing data

summary(dat_df)

# Add 5% missing data to some of the variables (missing completely at random here, for simplicity)
set.seed(321)

dat_df_miss <- dat_df
dat_df_miss$DG <- ifelse(rbinom(n = nrow(dat_df_miss), size = 1, prob = 0.05) == 1, NA, dat_df_miss$DG)
dat_df_miss$Pun <- ifelse(rbinom(n = nrow(dat_df_miss), size = 1, prob = 0.05) == 1, NA, dat_df_miss$Pun)
dat_df_miss$PunAve[is.na(dat_df_miss$Pun)] <- NA
dat_df_miss$See <- ifelse(rbinom(n = nrow(dat_df_miss), size = 1, prob = 0.05) == 1, NA, dat_df_miss$See)
dat_df_miss$OmniAve[is.na(dat_df_miss$See)] <- NA
dat_df_miss$Edu <- ifelse(rbinom(n = nrow(dat_df_miss), size = 1, prob = 0.05) == 1, NA, dat_df_miss$Edu)

summary(dat_df_miss)

# Number of complete cases now drops from 1,800 to 1,491, losing ~20% of the data
sum(complete.cases(dat_df_miss))


## Make categorical variables factors, as need this for MI to work
dat_df_miss$Male <- as.factor(dat_df_miss$Male)
dat_df_miss$Order <- as.factor(dat_df_miss$Order)
dat_df_miss$Pun <- as.factor(dat_df_miss$Pun)
dat_df_miss$Die <- as.factor(dat_df_miss$Die)
dat_df_miss$Know <- as.factor(dat_df_miss$Know)
dat_df_miss$See <- as.factor(dat_df_miss$See)
dat_df_miss$DG <- factor(dat_df_miss$DG, ordered = TRUE)


## Also drop the average punishment and omniscience exposures, as will calculate these later per imputation per site
dat_df_miss$PunAve_bar <- NULL
dat_df_miss$OmniAve_bar <- NULL


## Split data by society (as easier to impute each society separately, even though lose a bit of information by not pooling data)
soc1 <- subset(dat_df_miss, soc_id == 1)
soc2 <- subset(dat_df_miss, soc_id == 2)
soc3 <- subset(dat_df_miss, soc_id == 3)
soc4 <- subset(dat_df_miss, soc_id == 4)
soc5 <- subset(dat_df_miss, soc_id == 5)
soc6 <- subset(dat_df_miss, soc_id == 6)
soc7 <- subset(dat_df_miss, soc_id == 7)
soc8 <- subset(dat_df_miss, soc_id == 8)
soc9 <- subset(dat_df_miss, soc_id == 9)
soc10 <- subset(dat_df_miss, soc_id == 10)
soc11 <- subset(dat_df_miss, soc_id == 11)
soc12 <- subset(dat_df_miss, soc_id == 12)
soc13 <- subset(dat_df_miss, soc_id == 13)
soc14 <- subset(dat_df_miss, soc_id == 14)
soc15 <- subset(dat_df_miss, soc_id == 15)
soc16 <- subset(dat_df_miss, soc_id == 16)
soc17 <- subset(dat_df_miss, soc_id == 17)
soc18 <- subset(dat_df_miss, soc_id == 18)


## Set up general imputation models to be used for all societies (this can be adapted if different patterns of missing data in each society, or need different imputation models)

# Imputation method for each variable
meth <- make.method(soc1)
meth # As education is continuous change from 'pmm' to 'norm'; Passively impute punishment and omniscience mini-scales; keep others as default (Pun and See as logistic, DG as ordered categorical)
meth["Edu"] <- "norm"
meth["PunAve"] <- "~ I(((as.numeric(Pun) - 1) + (as.numeric(Die) - 1)) / 2)"
meth["OmniAve"] <- "~ I(((as.numeric(Know) - 1) + (as.numeric(See) - 1)) / 2)"
meth

# Specify imputation formulae for each variable with missing data - Will include interaction between Punishment and Omniscience mini-scales to make sure is compatible with all substantive analysis models
form <- make.formulas(soc1)
form
form$Edu <- as.formula(Edu ~ Age + Male + FoodSec + Order + PunAve + OmniAve + DG + PunAve:OmniAve)
form$Pun <- as.formula(Pun ~ Age + Male + Edu + FoodSec + Order + Die + OmniAve + DG + OmniAve:DG)
form$See <- as.formula(See ~ Age + Male + Edu + FoodSec + Order + PunAve + Know + DG + PunAve:DG)
form$DG <- as.formula(DG ~ Age + Male + Edu + FoodSec + Order + PunAve + OmniAve + PunAve:OmniAve)
form

# Visit sequence - Make sure passively-imputed scales are after the individual variables (they are)
visit <- make.visitSequence(soc1)
visit

# Test imputation to check no obvious errors
test <- mice(soc1, m = 10, maxit = 0, method = meth, formulas = form, visitSequence = visit, print = TRUE)
test$loggedEvents # The logged events here are just because some formulae contain constant terms (soc_id and LG) - As these formulae are only for variables without missing data and hence not imputed, this isn't an issue


### Imputation in each society - As very little missing data, will only impute 10 datasets per society with 5 iterations (Are quite a few logged events here, but these are just due to low cell counts in interaction terms with the DG variable, so are benign)
imp1 <- mice(soc1, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 62555)
imp2 <- mice(soc2, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 44523)
imp3 <- mice(soc3, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 66273)
imp4 <- mice(soc4, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 49955)
imp5 <- mice(soc5, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 73485)
imp6 <- mice(soc6, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 20893)
imp7 <- mice(soc7, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 20604)
imp8 <- mice(soc8, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 35131)
imp9 <- mice(soc9, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 41348)
imp10 <- mice(soc10, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 8152)
imp11 <- mice(soc11, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 55220)
imp12 <- mice(soc12, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 77685)
imp13 <- mice(soc13, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 77786)
imp14 <- mice(soc14, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 88242)
imp15 <- mice(soc15, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 39544)
imp16 <- mice(soc16, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 1429)
imp17 <- mice(soc17, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 89452)
imp18 <- mice(soc18, m = 10, maxit = 5, method = meth, formulas = form, visitSequence = visit, printFlag = TRUE, seed = 23985)


## Combine all imputed datasets together
imp_all <- rbind(imp1, imp2)
imp_all <- rbind(imp_all, imp3)
imp_all <- rbind(imp_all, imp4)
imp_all <- rbind(imp_all, imp5)
imp_all <- rbind(imp_all, imp6)
imp_all <- rbind(imp_all, imp7)
imp_all <- rbind(imp_all, imp8)
imp_all <- rbind(imp_all, imp9)
imp_all <- rbind(imp_all, imp10)
imp_all <- rbind(imp_all, imp11)
imp_all <- rbind(imp_all, imp12)
imp_all <- rbind(imp_all, imp13)
imp_all <- rbind(imp_all, imp14)
imp_all <- rbind(imp_all, imp15)
imp_all <- rbind(imp_all, imp16)
imp_all <- rbind(imp_all, imp17)
imp_all <- rbind(imp_all, imp18)

str(imp_all)


## Extract each imputed dataset and convert binary variables back to integer from factors
imp_all <- complete(imp_all, action = "all")

for (i in 1:length(imp_all)) {
  temp <- imp_all[[i]]
  temp$Male <- as.integer(temp$Male) - 1
  temp$Order <- as.integer(temp$Order) - 1
  temp$Pun <- as.integer(temp$Pun) - 1
  temp$Die <- as.integer(temp$Die) - 1
  temp$Know <- as.integer(temp$Know) - 1
  temp$See <- as.integer(temp$See) - 1
  temp$DG <- factor(as.integer(temp$DG) - 1, ordered = TRUE)
  
  # Add in average punishment and omniscience for each site
  temp$PunAve_bar <- with(temp, ave(PunAve, soc_id, FUN = mean))
  temp$OmniAve_bar <- with(temp, ave(OmniAve, soc_id, FUN = mean))
  
  # Replace old imputed data with new one
  imp_all[[i]] <- temp
}

str(imp_all)


### After all this faff, we can just pass these imputed datasets to BRMS to run our model on each imputed dataset

## Will do this using the fully-varying random slopes model with varying thresholds (takes about an hour to run), without pooling together results across datasets
mod1_imp <- brm_multiple(DG | thres(10, gr = as.factor(soc_id)) ~ PunAve + OmniAve + Age + Male + Edu + FoodSec + Order + PunAve_bar + OmniAve_bar +
                           (PunAve + OmniAve + Age + Male + Edu + FoodSec + Order | soc_id),
                         family = cumulative("logit"), data = imp_all,
                         prior = c(prior(normal(0, 1), class = b), # Priors for coefficient slopes
                                   prior(normal(0, 2), class = Intercept), # Prior for intercept term
                                   prior(exponential(1), class = sd, group = soc_id), # Priors for random intercepts
                                   prior(lkj(2), class = cor, group = soc_id)), # Priors for correlation between random effects
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 65456,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod1_imp, check_data = FALSE)


## Model with interaction term
mod1_imp_int <- brm_multiple(DG | thres(10, gr = as.factor(soc_id)) ~ PunAve + OmniAve + PunAve:OmniAve + Age + Male + Edu + FoodSec + Order + PunAve_bar + OmniAve_bar +
                           (PunAve + OmniAve + PunAve:OmniAve + Age + Male + Edu + FoodSec + Order | soc_id),
                         family = cumulative("logit"), data = imp_all,
                         prior = c(prior(normal(0, 1), class = b), # Priors for coefficient slopes
                                   prior(normal(0, 2), class = Intercept), # Prior for intercept term
                                   prior(exponential(1), class = sd, group = soc_id), # Priors for random intercepts
                                   prior(lkj(2), class = cor, group = soc_id)), # Priors for correlation between random effects
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 654567,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod1_imp_int, check_data = FALSE)


### Compare average model fit across all imputed datasets to see whether improves with interaction term
LOO_base <- rep(NA, length(imp_all))
LOO_int <- rep(NA, length(imp_all))
LOO_worseMod <- rep(NA, length(imp_all))
LOO_diff <- rep(NA, length(imp_all))
LOO_SE_diff <- rep(NA, length(imp_all))

for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  # LOO for additive model
  mod1_imp[[i]] <- add_criterion(mod1_imp[[i]], "loo")
  loo1_imp <- loo(mod1_imp[[i]])
  LOO_base[i] <- loo1_imp$estimates["elpd_loo", "Estimate"]
    
  # LOO for interaction model
  mod1_imp_int[[i]] <- add_criterion(mod1_imp_int[[i]], "loo")
  loo1_imp_int <- loo(mod1_imp_int[[i]])
  LOO_int[i] <- loo1_imp_int$estimates["elpd_loo", "Estimate"]
  
  # LOO diff and SE
  loo_comp <- loo_compare(loo1_imp, loo1_imp_int)
  LOO_worseMod[i] <- ifelse(LOO_base[i] < LOO_int[i], "Base", "Int")
  LOO_diff[i] <- loo_comp[2, 1]
  LOO_SE_diff[i] <- loo_comp[2, 2]
}  

# Mean of LOO statistics - For difference, make sure have the same base category of the worse-fitting model (on average)
mean(LOO_base)
mean(LOO_int)
table(LOO_worseMod)
worseMod <- ifelse(mean(LOO_base) < mean(LOO_int), "Base", "Int")
LOO_diff <- ifelse(LOO_worseMod == worseMod, LOO_diff, abs(LOO_diff))
mean(LOO_diff)
mean(LOO_SE_diff)


## Posterior predictions across all imputed datasets (for additive model)

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$PunAve <- 0
  temp$OmniAve <- 0
  sup0_temp <- predict(mod1_imp[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$PunAve <- 1
  temp$OmniAve <- 1
  sup1_temp <- predict(mod1_imp[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup1_temp2 <- sup1_temp
  for(j in 1:length(attr(sup1_temp, "levels"))) {
    sup1_temp2[sup1_temp == j] <- as.numeric(attr(sup1_temp, "levels")[j])
  }
  sup1_temp <- sup1_temp2
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

## Difference between counter-factual states from each of the posterior samples

# Whole sample
DG0 <- rep(NA, nrow(sup0_all))
DG1 <- rep(NA, nrow(sup0_all))
diff_DG <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG0[i] <- mean(sup0_all[i, ])
  DG1[i] <- mean(sup1_all[i, ])
  diff_DG[i] <- DG1[i] - DG0[i]
}

quantile(DG0, c(0.025, 0.5, 0.975))
quantile(DG1, c(0.025, 0.5, 0.975))

hist(diff_DG)
summary(diff_DG)
quantile(diff_DG, c(0.025, 0.5, 0.975))


# Difference between counter-factual states based on posterior samples split by moralistic vs local tradition
DG0_LG <- rep(NA, nrow(sup0_all))
DG1_LG <- rep(NA, nrow(sup0_all))
diff_DG_LG <- rep(NA, nrow(sup0_all))
DG0_MG <- rep(NA, nrow(sup0_all))
DG1_MG <- rep(NA, nrow(sup0_all))
diff_DG_MG <- rep(NA, nrow(sup0_all))

for (i in 1:nrow(sup0_all)) {
  DG0_LG[i] <- mean(sup0_all[i, ][temp$LG == 1])
  DG1_LG[i] <- mean(sup1_all[i, ][temp$LG == 1])
  diff_DG_LG[i] <- DG1_LG[i] - DG0_LG[i]
  
  DG0_MG[i] <- mean(sup0_all[i, ][temp$LG == 0])
  DG1_MG[i] <- mean(sup1_all[i, ][temp$LG == 0])
  diff_DG_MG[i] <- DG1_MG[i] - DG0_MG[i]
}

quantile(DG0_LG, c(0.025, 0.5, 0.975))
quantile(DG1_LG, c(0.025, 0.5, 0.975))
quantile(diff_DG_LG, c(0.025, 0.5, 0.975))

quantile(DG0_MG, c(0.025, 0.5, 0.975))
quantile(DG1_MG, c(0.025, 0.5, 0.975))
quantile(diff_DG_MG, c(0.025, 0.5, 0.975))

quantile(diff_DG_MG - diff_DG_LG, c(0.025, 0.5, 0.975))


### Wont show all other sensitivity analyses here either (e.g., different operationalisations of moralistic vs local traditions, inclusion/exclusion of variables which could be confounders and/or mediators, such as number of children, ritual participation and other gods' beliefs), as these are just variations on the themes above in terms of model specification.

