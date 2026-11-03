### Objective 1 Analysis Code - BaYaka - Study 2 (Supernatural punishment -> Cooperation)
### Created 09/06/2026 by Dan Major-Smith
### R 4.4.1

####################################################################
#### Clear workspace, set working directory and install/load packages
rm(list=ls())
Sys.setenv(LANG = "en")

setwd("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/Projects/Martin_BaYaka/Objective 1 - BaYaka/")
#setwd("")


#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("psych")
library(psych)

## Note that, for brms to work, 'stan' will also need to be installed on your computer - see https://mc-stan.org/install/
#install.packages("brms")
library(brms)

#install.packages("rstan")
library(rstan)

#install.packages("loo")
library(loo)

#install.packages("marginaleffects")
library(marginaleffects)


#####################################################################
### Read in and process BaYaka data (note that this step processes the raw data which is not openly-available. To follow these analyses, read in the data on line 185)

## Demographics first
BaYaka_demo <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Congo - Kocsis/Data/DEMO_bayaka_sessions_corrected.xlsx",
                          sheet = "Demographics")

head(BaYaka_demo)
glimpse(BaYaka_demo)

# Remove some empty variables, unused IDs and empty rows
BaYaka_demo <- BaYaka_demo %>%
  filter(!is.na(PARTID)) %>%
  select(-NAME, -DATE, -TIME, -SEXRA, -SESSION)

# Check data against ERM and make sure named and coded the same
BaYaka_demo <- BaYaka_demo %>%
  rename(CERCID = PARTID, SITE_SPEC = SITE) %>%
  mutate(SITE = "BAYAKA") %>%
  mutate(RESEARCHER = "KOCSIS") %>%
  relocate(CERCID, RESEARCHER, SITE, SITE_SPEC, .before = LOCATION)

# Check the data we need, and edit a couple of vars
table(BaYaka_demo$SEX, useNA = "ifany")
table(BaYaka_demo$AGE, useNA = "ifany")
table(BaYaka_demo$CITYYRS, useNA = "ifany")
table(BaYaka_demo$LIVE, useNA = "ifany")
table(BaYaka_demo$FORMALED, useNA = "ifany")
table(BaYaka_demo$CHILDREN, useNA = "ifany")
table(BaYaka_demo$MAT1, useNA = "ifany")
table(BaYaka_demo$MAT2, useNA = "ifany")
table(BaYaka_demo$MAT3, useNA = "ifany")
table(BaYaka_demo$MAT4, useNA = "ifany")

BaYaka_demo <- BaYaka_demo %>%
  mutate(EVER_CITY = ifelse(CITYYRS == 0, 0, 1)) %>%
  mutate(VILLAGE = as.numeric(as.factor(LIVE))) %>%
  mutate(VILLAGE = ifelse(VILLAGE == 2 | VILLAGE == 3, 0, 1))

table(BaYaka_demo$VILLAGE, useNA = "ifany")

# Make the derived material security variable
BaYaka_demo <- BaYaka_demo %>%
  mutate(MMAT = rowMeans(select(., MAT1, MAT2, MAT3, MAT4), na.rm = TRUE))

table(BaYaka_demo$MMAT, useNA = "ifany")


## Read in the game data
BaYaka_game <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Congo - Kocsis/Data/EXPERIMENTS_bayaka_corrections_2.xlsx",
                          sheet = "Data", skip = 1)

head(BaYaka_game)
glimpse(BaYaka_game)

# Remove some empty variables, post-game text data, and unused IDs
BaYaka_game <- BaYaka_game %>%
  select(-c(SITE, DATE, TIME, LOCATION, SESSNUM, INTERVIEWER, GA1, GA2)) %>%
  rename(CERCID = ID)

# Check the data and edit to match ERM names and coding
table(BaYaka_game$INGROUP...17, useNA = "ifany")
table(BaYaka_game$DISTANT1, useNA = "ifany")
table(BaYaka_game$SELF...26, useNA = "ifany")
table(BaYaka_game$DISTANT2, useNA = "ifany")

BaYaka_game <- BaYaka_game %>%
  rename(INGFIRST = ORDERRAG) %>%
  rename(PRE1_1 = '1P1', PRE1_2 = '1P2', PRE1_3 = '1P3', PRE1_4 = '1P4', PRE1_5 = '1P5',
         PRE2_1 = '2P1', PRE2_2 = '2P2', PRE2_3 = '2P3', PRE2_4 = '2P4', PRE2_5 = '2P5') %>%
  rename(COREL.L = DISTANT1, INGROUP = INGROUP...17, COREL.S = DISTANT2, SELF = SELF...26) %>%
  mutate(COREL.L = as.numeric(COREL.L)) %>%
  mutate(INGROUP = as.numeric(INGROUP)) %>%
  mutate(COREL.S = as.numeric(COREL.S)) %>%
  mutate(SELF = as.numeric(SELF)) %>%
  mutate(SUM1 = COREL.L + INGROUP) %>%
  mutate(SUM2 = COREL.S + SELF) %>%
  relocate(COREL.L, INGROUP, SUM1, .after = EXUSED1) %>%
  relocate(COREL.S, SELF, SUM2, .after = EXUSED2) %>%
  rename(INGFIRST_DG = ORDERDG) %>%
  rename(COREL.L_DG = DISTANT...30, INGROUP_DG = INGROUP...29, COREL.S_DG = DISTANT...32, SELF_DG = SELF...31) %>%
  mutate(SUM1_DG = COREL.L_DG + INGROUP_DG) %>%
  mutate(SUM2_DG = COREL.S_DG + SELF_DG) %>%
  relocate(COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG, .after = INGFIRST_DG)


# Check if any RAG totals do not sum to 30 - All good!
BaYaka_game %>%
  filter(SUM1 != 30 | SUM2 != 30) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L, INGROUP, SUM1, COREL.S, SELF, SUM2)

# Check if any DG totals do not sum to 10 - All good!
BaYaka_game %>%
  filter(SUM1_DG != 10 | SUM2_DG != 10) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG)


### Now for the religiosity interview data
BaYaka_relig <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Congo - Kocsis/Data/RELIGION_bayaka_corrections_1.xlsx",
                           sheet = "RELIGIOSITY", skip = 1)

head(BaYaka_relig)
glimpse(BaYaka_relig)

# Remove some unnecessary, text, and/or empty variables
BaYaka_relig <- BaYaka_relig %>%
  select(-c(DATE, TIME, INTERVIEWER, LOCATION, NAME, BGPUND, BGDIED, LGPUNISHD, LGDIE...52)) %>%
  rename(CERCID = ID)

# Check the data and edit to match ERM names and coding
BaYaka_relig <- BaYaka_relig %>%
  rename(LGDIE = LGDIE...51)

table(BaYaka_relig$BGPUNISH, useNA = "ifany")
table(BaYaka_relig$BGDIE, useNA = "ifany")
table(BaYaka_relig$BGFEEL, useNA = "ifany")
table(BaYaka_relig$BGSEE, useNA = "ifany")
table(BaYaka_relig$LGPUNISH, useNA = "ifany")
table(BaYaka_relig$LGDIE, useNA = "ifany")
table(BaYaka_relig$LGFEEL, useNA = "ifany")
table(BaYaka_relig$LGSEE, useNA = "ifany")

BaYaka_relig <- BaYaka_relig %>%
  mutate(BGFEEL = ifelse(BGFEEL == 2, 1, BGFEEL))


### Combine BaYaka demographics, game and religiosity data together
names(BaYaka_demo)
names(BaYaka_game)
names(BaYaka_relig)

BaYaka_main <- full_join(BaYaka_demo, BaYaka_game, by = "CERCID")
BaYaka_main <- full_join(BaYaka_main, BaYaka_relig, by = "CERCID")


## Keep just variables needed for analyses
BaYaka_main <- BaYaka_main %>%
  select(c(CERCID, SEX, AGE, CHILDREN, FORMALED, MMAT, VILLAGE, EVER_CITY,
           INGFIRST, EXUSED1, COREL.L, EXUSED2, COREL.S,
           INGFIRST_DG, COREL.L_DG, COREL.S_DG,
           LGPUNISH, LGDIE, LGFEEL, LGSEE,
           BGPUNISH, BGDIE, BGFEEL, BGSEE))

## Save this dataset (in both RData and CSV formats)
save(BaYaka_main, file = "./BaYaka/BaYaka_O1Study2.RData")
write_csv(BaYaka_main, file = "./BaYaka/BaYaka_O1Study2.csv")


################################################################################
#### Data analysis

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("BaYaka_data.RData")


### Descriptive stats

## Summary of missing data
cbind(n_miss = sapply(BaYaka_main, function(x) sum(is.na(x))),
      per_miss = sapply(BaYaka_main, function(x) round(sum(is.na(x)) / nrow(BaYaka_main) * 100, 2)))

# Only two variables - the RAG donations - have missing data (and same cases missing)
BaYaka_main %>%
  filter(is.na(COREL.L) | is.na(COREL.S)) %>%
  select(c(CERCID))


## Demographics

# Age (years)
summary(BaYaka_main$AGE)
sd(BaYaka_main$AGE)

plot(density(BaYaka_main$AGE), main = "", xlab = "Age")
hist(BaYaka_main$AGE, main = "", xlab = "Age")

# Sex (1 = male)
addmargins(table(BaYaka_main$SEX))
round(prop.table(table(BaYaka_main$SEX)) * 100, 1)

# Children
addmargins(table(BaYaka_main$CHILDREN))
round(prop.table(table(BaYaka_main$CHILDREN)) * 100, 1)

summary(BaYaka_main$CHILDREN)
sd(BaYaka_main$CHILDREN, na.rm = TRUE)

plot(density(BaYaka_main$CHILDREN, na.rm = TRUE), main = "", xlab = "Number of children")
hist(BaYaka_main$CHILDREN, main = "", xlab = "Number of children")

# Education
addmargins(table(BaYaka_main$FORMALED))
round(prop.table(table(BaYaka_main$FORMALED)) * 100, 1)

summary(BaYaka_main$FORMALED)
sd(BaYaka_main$FORMALED, na.rm = TRUE)

plot(density(BaYaka_main$FORMALED, na.rm = TRUE), main = "", xlab = "Number of years education")
hist(BaYaka_main$FORMALED, main = "", xlab = "Number of years education")

# Material insecurity
addmargins(table(BaYaka_main$MMAT))
round(prop.table(table(BaYaka_main$MMAT)) * 100, 1)

summary(BaYaka_main$MMAT)

plot(density(BaYaka_main$MMAT, na.rm = TRUE), main = "", xlab = "Food insecurity")
hist(BaYaka_main$MMAT, main = "", xlab = "Food insecurity")


## Religiosity

# MG punish
addmargins(table(BaYaka_main$BGPUNISH))
round(prop.table(table(BaYaka_main$BGPUNISH)) * 100, 1)

# LG punish
addmargins(table(BaYaka_main$LGPUNISH))
round(prop.table(table(BaYaka_main$LGPUNISH)) * 100, 1)

# Tetrachoric correlation between MG and LG PUNISH
addmargins(table(BaYaka_main$LGPUNISH, BaYaka_main$BGPUNISH))
round(prop.table(table(BaYaka_main$LGPUNISH, BaYaka_main$BGPUNISH), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$LGPUNISH, BaYaka_main$BGPUNISH))


# MG afterlife
addmargins(table(BaYaka_main$BGDIE))
round(prop.table(table(BaYaka_main$BGDIE)) * 100, 1)

# LG afterlife
addmargins(table(BaYaka_main$LGDIE))
round(prop.table(table(BaYaka_main$LGDIE)) * 100, 1)

# Tetrachoric correlation between MG and LG DIE
addmargins(table(BaYaka_main$LGDIE, BaYaka_main$BGDIE))
round(prop.table(table(BaYaka_main$LGDIE, BaYaka_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$LGDIE, BaYaka_main$BGDIE))


# Tetrachoric correlation between PUNISH and DIE for both MG and LG
addmargins(table(BaYaka_main$LGPUNISH, BaYaka_main$LGDIE))
round(prop.table(table(BaYaka_main$LGPUNISH, BaYaka_main$LGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$LGPUNISH, BaYaka_main$LGDIE))

addmargins(table(BaYaka_main$BGPUNISH, BaYaka_main$BGDIE))
round(prop.table(table(BaYaka_main$BGPUNISH, BaYaka_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$BGPUNISH, BaYaka_main$BGDIE))


# MG and LG punishment mini-scales
BaYaka_main <- BaYaka_main %>%
  mutate(BGPUN_AVE = rowMeans(select(., BGPUNISH, BGDIE), na.rm = FALSE)) %>%
  mutate(LGPUN_AVE = rowMeans(select(., LGPUNISH, LGDIE), na.rm = FALSE))

# MG
addmargins(table(BaYaka_main$BGPUN_AVE))
round(prop.table(table(BaYaka_main$BGPUN_AVE)) * 100, 1)

summary(BaYaka_main$BGPUN_AVE)

# LG
addmargins(table(BaYaka_main$LGPUN_AVE))
round(prop.table(table(BaYaka_main$LGPUN_AVE)) * 100, 1)

summary(BaYaka_main$LGPUN_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(BaYaka_main$BGPUN_AVE)), main = "MG Punish", ylim = c(0, 0.6))
barplot(prop.table(table(BaYaka_main$LGPUN_AVE)), main = "LG Punish", ylim = c(0, 0.6))
dev.off()

# Correlation between punishment mini-scales
addmargins(table(BaYaka_main$LGPUN_AVE, BaYaka_main$BGPUN_AVE))
round(prop.table(table(BaYaka_main$LGPUN_AVE, BaYaka_main$BGPUN_AVE), margin = 1) * 100, 1)

cor.test(BaYaka_main$LGPUN_AVE, BaYaka_main$BGPUN_AVE)
polychoric(cbind(as.factor(BaYaka_main$LGPUN_AVE), as.factor(BaYaka_main$BGPUN_AVE)))


# MG feel
addmargins(table(BaYaka_main$BGFEEL))
round(prop.table(table(BaYaka_main$BGFEEL)) * 100, 1)

# LG feel
addmargins(table(BaYaka_main$LGFEEL))
round(prop.table(table(BaYaka_main$LGFEEL)) * 100, 1)

# Tetrachoric correlation between MG and LG FEEL
addmargins(table(BaYaka_main$LGFEEL, BaYaka_main$BGFEEL))
round(prop.table(table(BaYaka_main$LGFEEL, BaYaka_main$BGFEEL), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$LGFEEL, BaYaka_main$BGFEEL))


# MG see
addmargins(table(BaYaka_main$BGSEE))
round(prop.table(table(BaYaka_main$BGSEE)) * 100, 1)

# LG see
addmargins(table(BaYaka_main$LGSEE))
round(prop.table(table(BaYaka_main$LGSEE)) * 100, 1)

# Tetrachoric correlation between MG and LG SEE
addmargins(table(BaYaka_main$LGSEE, BaYaka_main$BGSEE))
round(prop.table(table(BaYaka_main$LGSEE, BaYaka_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$LGSEE, BaYaka_main$BGSEE))


# Tetrachoric correlation between FEEL and SEE for both MG and LG
addmargins(table(BaYaka_main$LGFEEL, BaYaka_main$LGSEE))
round(prop.table(table(BaYaka_main$LGFEEL, BaYaka_main$LGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$LGFEEL, BaYaka_main$LGSEE))

addmargins(table(BaYaka_main$BGFEEL, BaYaka_main$BGSEE))
round(prop.table(table(BaYaka_main$BGFEEL, BaYaka_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(BaYaka_main$BGFEEL, BaYaka_main$BGSEE))


# MG and LG mini-omniscience scales
BaYaka_main <- BaYaka_main %>%
  mutate(BGOMNI_AVE = rowMeans(select(., BGFEEL, BGSEE), na.rm = FALSE)) %>%
  mutate(LGOMNI_AVE = rowMeans(select(., LGFEEL, LGSEE), na.rm = FALSE))

# MG
addmargins(table(BaYaka_main$BGOMNI_AVE))
round(prop.table(table(BaYaka_main$BGOMNI_AVE)) * 100, 1)

summary(BaYaka_main$BGOMNI_AVE)

# LG
addmargins(table(BaYaka_main$LGOMNI_AVE))
round(prop.table(table(BaYaka_main$LGOMNI_AVE)) * 100, 1)

summary(BaYaka_main$LGOMNI_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(BaYaka_main$BGOMNI_AVE)), main = "MG omniscience", ylim = c(0, 0.6))
barplot(prop.table(table(BaYaka_main$LGOMNI_AVE)), main = "LG omniscience", ylim = c(0, 0.6))
dev.off()

# Correlation between omniscience mini-scales
addmargins(table(BaYaka_main$LGOMNI_AVE, BaYaka_main$BGOMNI_AVE))
round(prop.table(table(BaYaka_main$LGOMNI_AVE, BaYaka_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(BaYaka_main$LGOMNI_AVE, BaYaka_main$BGOMNI_AVE)
polychoric(cbind(as.factor(BaYaka_main$LGOMNI_AVE), as.factor(BaYaka_main$BGOMNI_AVE)))


# Correlation between punishment and omniscience mini-scales for both MG and LG
addmargins(table(BaYaka_main$LGPUN_AVE, BaYaka_main$LGOMNI_AVE))
round(prop.table(table(BaYaka_main$LGPUN_AVE, BaYaka_main$LGOMNI_AVE), margin = 1) * 100, 1)

cor.test(BaYaka_main$LGPUN_AVE, BaYaka_main$LGOMNI_AVE)
polychoric(cbind(as.factor(BaYaka_main$LGPUN_AVE), as.factor(BaYaka_main$LGOMNI_AVE)))

addmargins(table(BaYaka_main$BGPUN_AVE, BaYaka_main$BGOMNI_AVE))
round(prop.table(table(BaYaka_main$BGPUN_AVE, BaYaka_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(BaYaka_main$BGPUN_AVE, BaYaka_main$BGOMNI_AVE)
polychoric(cbind(as.factor(BaYaka_main$BGPUN_AVE), as.factor(BaYaka_main$BGOMNI_AVE)))


### Cooperation

## RAGs

# Order (1 = local game first)
addmargins(table(BaYaka_main$INGFIRST))
round(prop.table(table(BaYaka_main$INGFIRST)) * 100, 1)


## Local RAG (RAG 1)

# Comprehension checks
addmargins(table(BaYaka_main$EXUSED1))
round(prop.table(table(BaYaka_main$EXUSED1)) * 100, 1)

# Tokens to distant co-religionist
summary(BaYaka_main$COREL.L)
sd(BaYaka_main$COREL.L, na.rm = TRUE)


## Self RAG (RAG 2)

# Comprehension checks
addmargins(table(BaYaka_main$EXUSED2))
round(prop.table(table(BaYaka_main$EXUSED2)) * 100, 1)

# Tokens to distant co-religionist
summary(BaYaka_main$COREL.S)
sd(BaYaka_main$COREL.S, na.rm = TRUE)


## Plot of RAG results to theoretical binomial distribution
den <- dbinom(x = 0:30, size = 30, prob = 0.5)
dens <- as.data.frame(cbind(n = 0:30, den = den))

# Local RAG
(p_rag_local <- ggplot(BaYaka_main) +
    geom_histogram(aes(x = COREL.L, y = ..density..), binwidth = 1, center = 0,
                   fill = "red", alpha = 0.3, colour = "black") +
    geom_vline(xintercept = mean(BaYaka_main$COREL.L, na.rm = TRUE), 
               colour = "red", linetype = "dashed", linewidth = 0.5) +
    geom_polygon(data = dens, aes(x = n, y = den), 
                 fill = "blue", alpha = 0.25, colour = "black") +
    geom_vline(xintercept = mean(15, na.rm = TRUE), 
               colour = "blue", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 31), labels = c(0, 5, 10, 15, 20, 25, 30),
                       breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    scale_y_continuous(limits = c(0, 0.2), expand = c(0, 0)) +
    labs(title = "LOCAL RAG", x = "Donations to Distant", y = "Density") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) 
)

# Self RAG
(p_rag_self <- ggplot(BaYaka_main) +
    geom_histogram(aes(x = COREL.S, y = ..density..), binwidth = 1, center = 0,
                   fill = "red", alpha = 0.3, colour = "black") +
    geom_vline(xintercept = mean(BaYaka_main$COREL.S, na.rm = TRUE), 
               colour = "red", linetype = "dashed", linewidth = 0.5) +
    geom_polygon(data = dens, aes(x = n, y = den), 
                 fill = "blue", alpha = 0.25, colour = "black") +
    geom_vline(xintercept = mean(15, na.rm = TRUE), 
               colour = "blue", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 31), labels = c(0, 5, 10, 15, 20, 25, 30),
                       breaks = c(0, 5, 10, 15, 20, 25, 30)) +
    scale_y_continuous(limits = c(0, 0.2), expand = c(0, 0)) +
    labs(title = "SELF RAG", x = "Donations to Distant", y = "Density") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) 
)

pdf(file = "BaYaka_RAGtoBinom.pdf", height = 6, width = 14)
grid.arrange(p_rag_local, p_rag_self, ncol = 2)
dev.off()


### Test whether RAG results differ from unbiased binomial 50/50 distribution

# LOCAL RAG
mod_RAG.L_base <- brm(COREL.L | trials(30) ~ 1,
                      family = "binomial",
                      data = BaYaka_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 20084)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_base)

# 95% CIs of log-odds includes 0/odds include 1, suggesting little bias towards either local or distant recipients (i.e., largely unbiased allocations), although is a slight bias towards distant recipients
exp(fixef(mod_RAG.L_base))


# SELF RAG
mod_RAG.S_base <- brm(COREL.S | trials(30) ~ 1,
                      family = "binomial",
                      data = BaYaka_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 68102)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_base)

# 95% CIs of log-odds includes 0/odds include 1, suggesting little bias towards either self or distant recipients (i.e., largely unbiased allocations), although is a slight bias towards distant recipients
exp(fixef(mod_RAG.S_base))


## Formal test of difference between local and self DG donations
df_temp <- as.data.frame(cbind(RAG = c(BaYaka_main$COREL.L, BaYaka_main$COREL.S), 
                               Cond = c(rep("Local", nrow(BaYaka_main)), rep("Self", nrow(BaYaka_main))),
                               CERCID = c(BaYaka_main$CERCID, BaYaka_main$CERCID)))

str(df_temp)

df_temp$RAG <- as.numeric(df_temp$RAG)

## Repeated-measures binomial model, with random slopes by condition to allow this to vary by participant
mod_RAG.LvsS <- brm(RAG | trials(30) ~ Cond + (Cond | CERCID),
                   family = "binomial",
                   data = df_temp,
                   prior = c(prior(normal(0, 1), class = b),
                             prior(normal(0, 2), class = Intercept),
                             prior(exponential(1), class = sd, group = CERCID),
                             prior(lkj(2), class = cor, group = CERCID)),
                   iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 39242)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_RAG.LvsS, variables = "Cond", type = "prediction")


## DGs

# Order (1 = local game first)
addmargins(table(BaYaka_main$INGFIRST_DG))
round(prop.table(table(BaYaka_main$INGFIRST_DG)) * 100, 1)


## Local DG (DG 1)

# Tokens to distant co-religionist
summary(BaYaka_main$COREL.L_DG)
sd(BaYaka_main$COREL.L_DG, na.rm = TRUE)

addmargins(table(BaYaka_main$COREL.L_DG))
round(prop.table(table(BaYaka_main$COREL.L_DG)) * 100, 1)


## Self DG (RAG 2)

# Tokens to distant co-religionist
summary(BaYaka_main$COREL.S_DG)
sd(BaYaka_main$COREL.S_DG, na.rm = TRUE)

addmargins(table(BaYaka_main$COREL.S_DG))
round(prop.table(table(BaYaka_main$COREL.S_DG)) * 100, 1)


## Plot of DG results

# Local DG
(p_dg_local <- ggplot(BaYaka_main) +
    geom_histogram(aes(x = COREL.L_DG, y = ..density..), binwidth = 1, center = 0,
                   fill = "red", alpha = 0.3, colour = "black") +
    geom_vline(xintercept = mean(BaYaka_main$COREL.L_DG, na.rm = TRUE), 
               colour = "red", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 10.5), labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
    scale_y_continuous(limits = c(0, 0.9), expand = c(0, 0)) +
    labs(title = "LOCAL DG", x = "Donations to Distant", y = "Density") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) 
)

# Self DG
(p_dg_self <- ggplot(BaYaka_main) +
    geom_histogram(aes(x = COREL.S_DG, y = ..density..), binwidth = 1, center = 0,
                   fill = "red", alpha = 0.3, colour = "black") +
    geom_vline(xintercept = mean(BaYaka_main$COREL.S_DG, na.rm = TRUE), 
               colour = "red", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 10.5), labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
    scale_y_continuous(limits = c(0, 0.9), expand = c(0, 0)) +
    labs(title = "SELF DG", x = "Donations to Distant", y = "Density") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) 
)

pdf(file = "BaYaka_DG.pdf", height = 6, width = 14)
grid.arrange(p_dg_local, p_dg_self, ncol = 2)
dev.off()


## Formal test of difference between local and self DG donations
df_temp <- as.data.frame(cbind(DG = c(BaYaka_main$COREL.L_DG, BaYaka_main$COREL.S_DG), 
                               Cond = c(rep("Local", nrow(BaYaka_main)), rep("Self", nrow(BaYaka_main))),
                               CERCID = c(BaYaka_main$CERCID, BaYaka_main$CERCID)))

str(df_temp)

# Make sure outcome is an ordered factor
df_temp$DG <- factor(as.numeric(df_temp$DG), ordered = TRUE)

## Repeated-measures ordinal model, with random slopes by condition to allow this to vary by participant
mod_DG.LvsS <- brm(DG ~ Cond + (Cond | CERCID),
                family = cumulative("logit"),
                data = df_temp,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept),
                          prior(exponential(1), class = sd, group = CERCID),
                          prior(lkj(2), class = cor, group = CERCID)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 53958)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_DG.LvsS, variables = "Cond", type = "prediction")



#################################
### Main regression analyses using causal model (with age, sex, education, number of children, material securityand Bantu integration as confounders, plus comprehension checks [for RAGs] and game order as covariates)

### RAG - DISTANT vs LOCAL

## Stan code for 15-inflated model (adapted from BRMS code for zero-inflated binomial model)
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
  lprior += normal_lpdf(Intercept | 0, 2); // Priors for (centered) intercept
  lprior += normal_lpdf(b | 0, 1); // Priors for covariates
  lprior += normal_lpdf(zi | 0, 2); // Priors for zero-inflated term
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

# Remove missing RAG data first
dat_cc <- BaYaka_main[complete.cases(BaYaka_main), ]

dat_temp_RAG.L <- cbind(Intercept = as.integer(rep(1, nrow(dat_cc))), 
                        LGPUN_AVE = dat_cc$LGPUN_AVE, LGOMNI_AVE = dat_cc$LGOMNI_AVE, 
                        AGE = dat_cc$AGE, SEX = as.integer(dat_cc$SEX), 
                        FORMALED = as.integer(dat_cc$FORMALED), MMAT = dat_cc$MMAT, 
                        CHILDREN = as.integer(dat_cc$CHILDREN), EVER_CITY = as.integer(dat_cc$EVER_CITY),
                        VILLAGE = as.integer(dat_cc$VILLAGE),
                        INGFIRST = as.integer(dat_cc$INGFIRST), EXUSED1 = as.integer(dat_cc$EXUSED1))

stan_dat_RAG.L <- list(
  Y = as.integer(dat_cc$COREL.L),
  X = dat_temp_RAG.L,
  N = as.integer(nrow(dat_cc)),
  trials = as.integer(rep(30, nrow(dat_cc))),
  K = as.integer(ncol(dat_temp_RAG.L)),
  Kc = as.integer(ncol(dat_temp_RAG.L) - 1),
  prior_only = as.integer(0)
)

str(stan_dat_RAG.L)

### Stan model (additive)
mod_RAG.L <- stan(model_code = model_code_15inf, data = stan_dat_RAG.L,
            iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 24876)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod_RAG.L, probs = c(0.025, 0.975))

## Model fit values
log_lik_matrix <- extract_log_lik(mod_RAG.L, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_RAG.L <- loo(log_lik_matrix, r_eff = r_eff)
loo_RAG.L


### Model with interaction between exposures

## Data for Stan code
dat_temp_RAG.L_int <- cbind(Intercept = as.integer(rep(1, nrow(dat_cc))), 
                        LGPUN_AVE = dat_cc$LGPUN_AVE, LGOMNI_AVE = dat_cc$LGOMNI_AVE, 
                        LGPUN_OMNI_INT = dat_cc$LGPUN_AVE * dat_cc$LGOMNI_AVE,
                        AGE = dat_cc$AGE, SEX = as.integer(dat_cc$SEX), 
                        FORMALED = as.integer(dat_cc$FORMALED), MMAT = dat_cc$MMAT, 
                        CHILDREN = as.integer(dat_cc$CHILDREN), EVER_CITY = as.integer(dat_cc$EVER_CITY),
                        VILLAGE = as.integer(dat_cc$VILLAGE),
                        INGFIRST = as.integer(dat_cc$INGFIRST), EXUSED1 = as.integer(dat_cc$EXUSED1))

stan_dat_RAG.L_int <- list(
  Y = as.integer(dat_cc$COREL.L),
  X = dat_temp_RAG.L_int,
  N = as.integer(nrow(dat_cc)),
  trials = as.integer(rep(30, nrow(dat_cc))),
  K = as.integer(ncol(dat_temp_RAG.L_int)),
  Kc = as.integer(ncol(dat_temp_RAG.L_int) - 1),
  prior_only = as.integer(0)
)

str(stan_dat_RAG.L_int)

### Stan model (interaction)
mod_RAG.L_int <- stan(model_code = model_code_15inf, data = stan_dat_RAG.L_int,
                  iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 52454)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod_RAG.L_int, probs = c(0.025, 0.975))

## Model fit values
log_lik_matrix <- extract_log_lik(mod_RAG.L_int, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_RAG.L_int <- loo(log_lik_matrix, r_eff = r_eff)
loo_RAG.L_int


## Compare additive and interaction models - No improvement with inclusion of interaction terms
loo_compare(loo_RAG.L, loo_RAG.L_int)


### G-computation (based on additive model)

# Extract model coefficients
(post_coefs <- as_draws_df(mod_RAG.L))

# Where supernatural beliefs are 0
dat_temp_RAG.L <- data.frame(cbind(Intercept = as.integer(rep(1, nrow(dat_cc))), 
                  LGPUN_AVE = dat_cc$LGPUN_AVE, LGOMNI_AVE = dat_cc$LGOMNI_AVE, 
                  AGE = dat_cc$AGE, SEX = as.integer(dat_cc$SEX), 
                  FORMALED = as.integer(dat_cc$FORMALED), MMAT = dat_cc$MMAT, 
                  CHILDREN = as.integer(dat_cc$CHILDREN), EVER_CITY = as.integer(dat_cc$EVER_CITY),
                  VILLAGE = as.integer(dat_cc$VILLAGE),
                  INGFIRST = as.integer(dat_cc$INGFIRST), EXUSED1 = as.integer(dat_cc$EXUSED1)))

df_sup0 <- dat_temp_RAG.L
df_sup0$LGPUN_AVE <- 0
df_sup0$LGOMNI_AVE <- 0

sup0 <- list()
for (i in 1:nrow(post_coefs)) {
  
  print(paste0("On posterior: ", i))
  
  # Make temp dataset for this
  dat_temp <- df_sup0
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = nrow(dat_temp), size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                    prob = plogis(post_coefs$b_Intercept[i] +
                                                    (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                    (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                    (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                    (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                    (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                    (post_coefs$'b[6]'[i] * dat_temp_noInf$MMAT) +
                                                    (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                    (post_coefs$'b[8]'[i] * dat_temp_noInf$EVER_CITY) +
                                                    (post_coefs$'b[9]'[i] * dat_temp_noInf$VILLAGE) +
                                                    (post_coefs$'b[10]'[i] * dat_temp_noInf$INGFIRST) +
                                                    (post_coefs$'b[11]'[i] * dat_temp_noInf$EXUSED1)))
  
  # Code as '15' if inflated, then merge together - but only if any excess values
  if (sum(dat_temp$inflate_temp) > 0) {
    dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
    dat_temp_inf$RAG_temp <- 15
    
    dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  }
  
  if (sum(dat_temp$inflate_temp) == 0) {
    dat_temp <- dat_temp_noInf
  }
  
  sup0[i] <- as.data.frame(dat_temp$RAG_temp)
}


# Where supernatural beliefs are 1
df_sup1 <- dat_temp_RAG.L
df_sup1$LGPUN_AVE <- 1
df_sup1$LGOMNI_AVE <- 1

sup1 <- list()
for (i in 1:nrow(post_coefs)) {
  
  print(paste0("On posterior: ", i))
  
  # Make temp dataset for this
  dat_temp <- df_sup1
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = nrow(dat_temp), size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                    prob = plogis(post_coefs$b_Intercept[i] +
                                                    (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                    (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                    (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                    (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                    (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                    (post_coefs$'b[6]'[i] * dat_temp_noInf$MMAT) +
                                                    (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                    (post_coefs$'b[8]'[i] * dat_temp_noInf$EVER_CITY) +
                                                    (post_coefs$'b[9]'[i] * dat_temp_noInf$VILLAGE) +
                                                    (post_coefs$'b[10]'[i] * dat_temp_noInf$INGFIRST) +
                                                    (post_coefs$'b[11]'[i] * dat_temp_noInf$EXUSED1)))
  
  # Code as '15' if inflated, then merge together - but only if any excess values
  if (sum(dat_temp$inflate_temp) > 0) {
    dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
    dat_temp_inf$RAG_temp <- 15
    
    dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  }
  
  if (sum(dat_temp$inflate_temp) == 0) {
    dat_temp <- dat_temp_noInf
  }
  
  sup1[i] <- as.data.frame(dat_temp$RAG_temp)
}


## Difference between counter-factual states from each of the posterior samples
RAG.L_0 <- rep(NA, length(sup0))
RAG.L_1 <- rep(NA, length(sup0))
diff_RAG.L <- rep(NA, length(sup0))
for (i in 1:length(sup0)) {
  RAG.L_0[i] <- mean(sup0[[i]])
  RAG.L_1[i] <- mean(sup1[[i]])
  diff_RAG.L[i] <- RAG.L_1[i] - RAG.L_0[i]
}

hist(diff_RAG.L)
summary(diff_RAG.L)
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))

quantile(RAG.L_0, c(0.025, 0.5, 0.975))
quantile(RAG.L_1, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF

## Stan code for 15/16/17 inflated model (adapted from BRMS code for zero-inflated binomial model)
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
  lprior += normal_lpdf(Intercept | 0, 2); // Priors for (centered) intercept
  lprior += normal_lpdf(b | 0, 1); // Priors for covariates
  lprior += normal_lpdf(zi | 0, 2); // Priors for zero-inflated term
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

# Remove missing RAG data first
dat_cc <- BaYaka_main[complete.cases(BaYaka_main), ]

dat_temp_RAG.S <- cbind(Intercept = as.integer(rep(1, nrow(dat_cc))), 
                        LGPUN_AVE = dat_cc$LGPUN_AVE, LGOMNI_AVE = dat_cc$LGOMNI_AVE, 
                        AGE = dat_cc$AGE, SEX = as.integer(dat_cc$SEX), 
                        FORMALED = as.integer(dat_cc$FORMALED), MMAT = dat_cc$MMAT, 
                        CHILDREN = as.integer(dat_cc$CHILDREN), EVER_CITY = as.integer(dat_cc$EVER_CITY),
                        VILLAGE = as.integer(dat_cc$VILLAGE),
                        INGFIRST = as.integer(dat_cc$INGFIRST), EXUSED2 = as.integer(dat_cc$EXUSED2))

stan_dat_RAG.S <- list(
  Y = as.integer(dat_cc$COREL.S),
  X = dat_temp_RAG.S,
  N = as.integer(nrow(dat_cc)),
  trials = as.integer(rep(30, nrow(dat_cc))),
  K = as.integer(ncol(dat_temp_RAG.S)),
  Kc = as.integer(ncol(dat_temp_RAG.S) - 1),
  prior_only = as.integer(0)
)

str(stan_dat_RAG.S)

### Stan model (additive)
mod_RAG.S <- stan(model_code = model_code_151617inf, data = stan_dat_RAG.S,
                  iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12865)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod_RAG.S, probs = c(0.025, 0.975))

## Model fit values
log_lik_matrix <- extract_log_lik(mod_RAG.S, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_RAG.S <- loo(log_lik_matrix, r_eff = r_eff)
loo_RAG.S


### Model with interaction between exposures

## Data for Stan code
dat_temp_RAG.S_int <- cbind(Intercept = as.integer(rep(1, nrow(dat_cc))), 
                            LGPUN_AVE = dat_cc$LGPUN_AVE, LGOMNI_AVE = dat_cc$LGOMNI_AVE, 
                            LGPUN_OMNI_INT = dat_cc$LGPUN_AVE * dat_cc$LGOMNI_AVE,
                            AGE = dat_cc$AGE, SEX = as.integer(dat_cc$SEX), 
                            FORMALED = as.integer(dat_cc$FORMALED), MMAT = dat_cc$MMAT, 
                            CHILDREN = as.integer(dat_cc$CHILDREN), EVER_CITY = as.integer(dat_cc$EVER_CITY),
                            VILLAGE = as.integer(dat_cc$VILLAGE),
                            INGFIRST = as.integer(dat_cc$INGFIRST), EXUSED2 = as.integer(dat_cc$EXUSED2))

stan_dat_RAG.S_int <- list(
  Y = as.integer(dat_cc$COREL.S),
  X = dat_temp_RAG.S_int,
  N = as.integer(nrow(dat_cc)),
  trials = as.integer(rep(30, nrow(dat_cc))),
  K = as.integer(ncol(dat_temp_RAG.S_int)),
  Kc = as.integer(ncol(dat_temp_RAG.S_int) - 1),
  prior_only = as.integer(0)
)

str(stan_dat_RAG.S_int)

### Stan model (interaction)
mod_RAG.S_int <- stan(model_code = model_code_151617inf, data = stan_dat_RAG.S_int,
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 7471)

## Model fit statistics all look good (r-hat = 1, high ESS, and well-mixed trace-plots)
print(mod_RAG.S_int, probs = c(0.025, 0.975))

## Model fit values
log_lik_matrix <- extract_log_lik(mod_RAG.S_int, merge_chains = FALSE)

# Compute LOO
r_eff <- relative_eff(exp(log_lik_matrix))
loo_RAG.S_int <- loo(log_lik_matrix, r_eff = r_eff)
loo_RAG.S_int


## Compare additive and interaction models - No improvement with inclusion of interaction terms
loo_compare(loo_RAG.S, loo_RAG.S_int)


### G-computation (based on additive model)

# Extract model coefficients
(post_coefs <- as_draws_df(mod_RAG.S))

# Where supernatural beliefs are 0
dat_temp_RAG.S <- data.frame(cbind(Intercept = as.integer(rep(1, nrow(dat_cc))), 
                                   LGPUN_AVE = dat_cc$LGPUN_AVE, LGOMNI_AVE = dat_cc$LGOMNI_AVE, 
                                   AGE = dat_cc$AGE, SEX = as.integer(dat_cc$SEX), 
                                   FORMALED = as.integer(dat_cc$FORMALED), MMAT = dat_cc$MMAT, 
                                   CHILDREN = as.integer(dat_cc$CHILDREN), 
                                   EVER_CITY = as.integer(dat_cc$EVER_CITY),
                                   VILLAGE = as.integer(dat_cc$VILLAGE),
                                   INGFIRST = as.integer(dat_cc$INGFIRST), 
                                   EXUSED2 = as.integer(dat_cc$EXUSED2)))

df_sup0 <- dat_temp_RAG.S
df_sup0$LGPUN_AVE <- 0
df_sup0$LGOMNI_AVE <- 0

sup0 <- list()
for (i in 1:nrow(post_coefs)) {
  
  print(paste0("On posterior: ", i))
  
  # Make temp dataset for this
  dat_temp <- df_sup0
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = nrow(dat_temp), size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                    prob = plogis(post_coefs$b_Intercept[i] +
                                                    (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                    (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                    (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                    (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                    (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                    (post_coefs$'b[6]'[i] * dat_temp_noInf$MMAT) +
                                                    (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                    (post_coefs$'b[8]'[i] * dat_temp_noInf$EVER_CITY) +
                                                    (post_coefs$'b[9]'[i] * dat_temp_noInf$VILLAGE) +
                                                    (post_coefs$'b[10]'[i] * dat_temp_noInf$INGFIRST) +
                                                    (post_coefs$'b[11]'[i] * dat_temp_noInf$EXUSED2)))
  
  # Code as '15' if inflated, then merge together - but only if any excess values
  if (sum(dat_temp$inflate_temp) > 0) {
    dat_temp_inf <- subset(dat_temp[dat_temp$inflate_temp == 1,])
    dat_temp_inf$temp <- runif(n = nrow(dat_temp_inf), min = 0, max = 1)
    dat_temp_inf$RAG_temp <- ifelse(dat_temp_inf$temp < 0.33, 15, 
                                          ifelse(dat_temp_inf$temp > 0.67, 16, 17))
    dat_temp_inf$temp <- NULL
    
    dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  }
  
  if (sum(dat_temp$inflate_temp) == 0) {
    dat_temp <- dat_temp_noInf
  }
  
  sup0[i] <- as.data.frame(dat_temp$RAG_temp)
}


# Where supernatural beliefs are 1
df_sup1 <- dat_temp_RAG.S
df_sup1$LGPUN_AVE <- 1
df_sup1$LGOMNI_AVE <- 1

sup1 <- list()
for (i in 1:nrow(post_coefs)) {
  
  print(paste0("On posterior: ", i))
  
  # Make temp dataset for this
  dat_temp <- df_sup1
  
  # Prob of being inflated or not
  dat_temp$inflate_temp <- rbinom(n = nrow(dat_temp), size = 1, prob = post_coefs$zi[i])
  
  # Binomial distribution if not inflated
  dat_temp_noInf <- subset(dat_temp[dat_temp$inflate_temp == 0,])
  dat_temp_noInf$RAG_temp <- rbinom(n = nrow(dat_temp_noInf), size = 30, 
                                    prob = plogis(post_coefs$b_Intercept[i] +
                                                    (post_coefs$'b[1]'[i] * dat_temp_noInf$LGPUN_AVE) +
                                                    (post_coefs$'b[2]'[i] * dat_temp_noInf$LGOMNI_AVE) +
                                                    (post_coefs$'b[3]'[i] * dat_temp_noInf$AGE) +
                                                    (post_coefs$'b[4]'[i] * dat_temp_noInf$SEX) +
                                                    (post_coefs$'b[5]'[i] * dat_temp_noInf$FORMALED) +
                                                    (post_coefs$'b[6]'[i] * dat_temp_noInf$MMAT) +
                                                    (post_coefs$'b[7]'[i] * dat_temp_noInf$CHILDREN) +
                                                    (post_coefs$'b[8]'[i] * dat_temp_noInf$EVER_CITY) +
                                                    (post_coefs$'b[9]'[i] * dat_temp_noInf$VILLAGE) +
                                                    (post_coefs$'b[10]'[i] * dat_temp_noInf$INGFIRST) +
                                                    (post_coefs$'b[11]'[i] * dat_temp_noInf$EXUSED2)))
  
  # Code as '15' if inflated, then merge together - but only if any excess values
  if (sum(dat_temp$inflate_temp) > 0) {
    dat_temp_inf$temp <- runif(n = nrow(dat_temp_inf), min = 0, max = 1)
    dat_temp_inf$RAG_temp <- ifelse(dat_temp_inf$temp < 0.33, 15, 
                                          ifelse(dat_temp_inf$temp > 0.67, 16, 17))
    dat_temp_inf$temp <- NULL
    
    dat_temp <- rbind(dat_temp_noInf, dat_temp_inf)
  }
  
  if (sum(dat_temp$inflate_temp) == 0) {
    dat_temp <- dat_temp_noInf
  }
  
  sup1[i] <- as.data.frame(dat_temp$RAG_temp)
}


## Difference between counter-factual states from each of the posterior samples
RAG.S_0 <- rep(NA, length(sup0))
RAG.S_1 <- rep(NA, length(sup0))
diff_RAG.S <- rep(NA, length(sup0))
for (i in 1:length(sup0)) {
  RAG.S_0[i] <- mean(sup0[[i]])
  RAG.S_1[i] <- mean(sup1[[i]])
  diff_RAG.S[i] <- RAG.S_1[i] - RAG.S_0[i]
}

hist(diff_RAG.S)
summary(diff_RAG.S)
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))

quantile(RAG.S_0, c(0.025, 0.5, 0.975))
quantile(RAG.S_1, c(0.025, 0.5, 0.975))


## Density plots of RAG results
pdf(file = "BaYaka_RAG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.45), xlim = c(-6, 5),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.45), xlim = c(-6, 5),
     main = "SELF RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_RAG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


### DG - DISTANT vs LOCAL

# Make sure outcome is an ordered factor
BaYaka_main$COREL.L_DG <- factor(BaYaka_main$COREL.L_DG, ordered = TRUE)

## Additive model
mod_DG.L <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN + EVER_CITY + VILLAGE +
                  INGFIRST_DG,
                 family = cumulative("logit"),
                 data = BaYaka_main,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 57453)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L)

# Add model fit, so can compare models
mod_DG.L <- add_criterion(mod_DG.L, "loo")
loo_DG.L <- loo(mod_DG.L)
loo_DG.L


## Interaction model
mod_DG.L_int <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN + EVER_CITY + VILLAGE +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = BaYaka_main,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 89101)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_int)

# Add model fit, so can compare models
mod_DG.L_int <- add_criterion(mod_DG.L_int, "loo")
loo_DG.L_int <- loo(mod_DG.L_int)
loo_DG.L_int

# Compare to base model without interaction term - No real improvement in model fit
loo_compare(loo_DG.L, loo_DG.L_int)


## G-computation (based on additive model)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = BaYaka_main$AGE, SEX = BaYaka_main$SEX, 
                               FORMALED = BaYaka_main$FORMALED, MMAT = BaYaka_main$MMAT, 
                               CHILDREN = BaYaka_main$CHILDREN,
                               EVER_CITY = BaYaka_main$EVER_CITY, VILLAGE = BaYaka_main$VILLAGE,
                               INGFIRST_DG = BaYaka_main$INGFIRST_DG))
sup0 <- predict(mod_DG.L, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = BaYaka_main$AGE, SEX = BaYaka_main$SEX, 
                               FORMALED = BaYaka_main$FORMALED, MMAT = BaYaka_main$MMAT, 
                               CHILDREN = BaYaka_main$CHILDREN,
                               EVER_CITY = BaYaka_main$EVER_CITY, VILLAGE = BaYaka_main$VILLAGE,
                               INGFIRST_DG = BaYaka_main$INGFIRST_DG))
sup1 <- predict(mod_DG.L, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1_temp[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.L_0 <- rep(NA, nrow(sup0))
DG.L_1 <- rep(NA, nrow(sup0))
diff_DG.L <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.L_0[i] <- mean(sup0[i, ])
  DG.L_1[i] <- mean(sup1[i, ])
  diff_DG.L[i] <- DG.L_1[i] - DG.L_0[i]
}

plot(density(diff_DG.L), main = "Local DG", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L)
quantile(diff_DG.L, c(0.025, 0.5, 0.975))

quantile(DG.L_0, c(0.025, 0.5, 0.975))
quantile(DG.L_1, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF

# Make sure outcome is an ordered factor
BaYaka_main$COREL.S_DG <- factor(BaYaka_main$COREL.S_DG, ordered = TRUE)

## Additive model
mod_DG.S <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN + EVER_CITY + VILLAGE +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = BaYaka_main,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 56392)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S)

# Add model fit, so can compare models
mod_DG.S <- add_criterion(mod_DG.S, "loo")
loo_DG.S <- loo(mod_DG.S)
loo_DG.S


## Interaction model
mod_DG.S_int <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN + EVER_CITY + VILLAGE +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = BaYaka_main,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 35911)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_int)

# Add model fit, so can compare models
mod_DG.S_int <- add_criterion(mod_DG.S_int, "loo")
loo_DG.S_int <- loo(mod_DG.S_int)
loo_DG.S_int

# Compare to base model without interaction term - No real improvement in model fit
loo_compare(mod_DG.S, mod_DG.S_int)


## G-computation (based on additive model)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = BaYaka_main$AGE, SEX = BaYaka_main$SEX, 
                               FORMALED = BaYaka_main$FORMALED, MMAT = BaYaka_main$MMAT, 
                               CHILDREN = BaYaka_main$CHILDREN,
                               EVER_CITY = BaYaka_main$EVER_CITY, VILLAGE = BaYaka_main$VILLAGE,
                               INGFIRST_DG = BaYaka_main$INGFIRST_DG))
sup0 <- predict(mod_DG.S, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = BaYaka_main$AGE, SEX = BaYaka_main$SEX, 
                               FORMALED = BaYaka_main$FORMALED, MMAT = BaYaka_main$MMAT, 
                               CHILDREN = BaYaka_main$CHILDREN,
                               EVER_CITY = BaYaka_main$EVER_CITY, VILLAGE = BaYaka_main$VILLAGE,
                               INGFIRST_DG = BaYaka_main$INGFIRST_DG))
sup1 <- predict(mod_DG.S, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1_temp[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.S_0 <- rep(NA, nrow(sup0))
DG.S_1 <- rep(NA, nrow(sup0))
diff_DG.S <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.S_0[i] <- mean(sup0[i, ])
  DG.S_1[i] <- mean(sup1[i, ])
  diff_DG.S[i] <- DG.S_1[i] - DG.S_0[i]
}

plot(density(diff_DG.S), main = "Self DG", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S)
quantile(diff_DG.S, c(0.025, 0.5, 0.975))

quantile(DG.S_0, c(0.025, 0.5, 0.975))
quantile(DG.S_1, c(0.025, 0.5, 0.975))


## Density plots of DG results
pdf(file = "BaYaka_DG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 3), xlim = c(-1, 1),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 3), xlim = c(-1, 1),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()



