### Objective 1 Analysis Code - Buryat
### Created 16/2/2026 by Dan Major-Smith
### R 4.4.1

####################################################################
#### Clear workspace, set working directory and install/load packages
rm(list=ls())
Sys.setenv(LANG = "en")

setwd("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/Projects/Byamba_Buryat/Objective 1 - Buryat")
#setwd("")


#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

#install.packages("psych")
library(psych)

## Note that, for brms to work, 'stan' will also need to be installed on your computer - see https://mc-stan.org/install/
#install.packages("brms")
library(brms)

#install.packages("marginaleffects")
library(marginaleffects)


#####################################################################
### Read in and process Buryat data (note that this step processes the raw data which is not openly-available. To follow these analyses, read in the data on line 170)

## Demographics first
buryat_demo <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Ichinkhorloo/Data/1 Demographic Byamba_20240822 rev.xlsx",
                          sheet = "Demographics (2)", skip = 1)

head(buryat_demo)
glimpse(buryat_demo)

# Remove some empty variables, unused IDs and empty rows
buryat_demo <- buryat_demo %>%
  filter(!is.na(PARTID)) %>%
  select(-NAME, -DATE, -TIME, -SEXRA, -SESSION, -...12)

# Check data against ERM and make sure named and coded the same
buryat_demo <- buryat_demo %>%
  rename(CERCID = PARTID, SITE_SPEC = SITE) %>%
  mutate(SITE = "BURYAT") %>%
  mutate(RESEARCHER = "ICHINKHORLOO") %>%
  relocate(CERCID, RESEARCHER, SITE, SITE_SPEC, .before = LOCATION)

# Three people have impossible values above '1' some of the 'MAT' variables - Will check data and recode - No scan for BI041, so will recode as missing
buryat_demo %>%
  filter(MAT1  > 1 | MAT2  > 1 | MAT3 > 1) %>%
  select(CERCID, MAT1:MAT4C)

buryat_demo <- buryat_demo %>%
  mutate(MAT1 = ifelse(CERCID == "BI031", 0, MAT1)) %>%
  mutate(MAT2 = ifelse(CERCID == "BI052", 0, MAT2)) %>%
  mutate(MAT3 = ifelse(CERCID == "BI052", 0, MAT3)) %>%
  mutate(MAT1 = ifelse(CERCID == "BI041", NA, MAT1)) %>%
  mutate(MAT2 = ifelse(CERCID == "BI041", NA, MAT2)) %>%
  mutate(MAT3 = ifelse(CERCID == "BI041", NA, MAT3))


# Make the derived material security variables
buryat_demo <- buryat_demo %>%
  mutate(MMAT = rowMeans(select(., MAT1, MAT2, MAT3, MAT4), na.rm = TRUE)) %>%
  mutate(MMATc = rowMeans(select(., MAT1C, MAT2C, MAT3C, MAT4C), na.rm = TRUE))


## Read in the game data
cols <- buryat_game <- as.character(read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Ichinkhorloo/Data/2 Game Byamba 20240822 rev.xlsx",
                                               sheet = "Data (2)", range = "A2:AM2", col_names = FALSE))

buryat_game <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Ichinkhorloo/Data/2 Game Byamba 20240822 rev.xlsx",
                          sheet = "Data (2)",
                          range = "A4:AM86", col_names = cols)

head(buryat_game)
glimpse(buryat_game)

# Remove some empty variables, post-game text data, and unused IDs
buryat_game <- buryat_game %>%
  filter(!grepl("nobody drew it", ID)) %>%
  select(-c(SITE, DATE, TIME, LOCATION, SESSNUM, GA1...27, NA...28, GA2...29, NA...30, GA1...36, NA...37, GA2...38, NA...39)) %>%
  rename(CERCID = ID, INTERVIEWER_GAMES = INTERVIEWER) %>%
  relocate(CERCID, .before = INTERVIEWER_GAMES)

# Check the data and edit to match ERM names and coding
buryat_game <- buryat_game %>%
  rename(ORDER = ORDERRAG) %>%
  mutate(INGFIRST = ifelse(ORDER == 12, 1, 0)) %>%
  relocate(INGFIRST, .after = ORDER) %>%
  rename(PRE1_1 = '1P1', PRE1_2 = '1P2', PRE1_3 = '1P3', PRE1_4 = '1P4', PRE1_5 = '1P5',
         PRE2_1 = '2P1', PRE2_2 = '2P2', PRE2_3 = '2P3', PRE2_4 = '2P4', PRE2_5 = '2P5') %>%
  rename(COREL.L = DISTANT...17, INGROUP = LOCAL...16, COREL.S = DISTANT...26, SELF = SELF...25) %>%
  mutate(SUM1 = COREL.L + INGROUP) %>%
  mutate(SUM2 = COREL.S + SELF) %>%
  relocate(COREL.L, INGROUP, SUM1, .after = EXUSED1) %>%
  relocate(COREL.S, SELF, SUM2, .after = EXUSED2) %>%
  mutate(INGFIRST_DG = ifelse(ORDERDG == 12, 1, 0)) %>%
  relocate(INGFIRST_DG, .after = ORDERDG) %>%
  rename(COREL.L_DG = DISTANT...33, INGROUP_DG = LOCAL...32, COREL.S_DG = DISTANT...35, SELF_DG = SELF...34) %>%
  mutate(SUM1_DG = COREL.L_DG + INGROUP_DG) %>%
  mutate(SUM2_DG = COREL.S_DG + SELF_DG) %>%
  relocate(COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG, .after = INGFIRST_DG)


# Check if any RAG totals do not sum to 30 - All good!
buryat_game %>%
  filter(SUM1 != 30 | SUM2 != 30) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L, INGROUP, SUM1, COREL.S, SELF, SUM2)

# Check if any DG totals do not sum to 10 - All good!
buryat_game %>%
  filter(SUM1_DG != 10 | SUM2_DG != 10) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG)


### Now for the religiosity interview data
buryat_relig <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Ichinkhorloo/Data/3 Religionsity Byamba 20240822 rev_DMS.xlsx",
                           sheet = "RELIGIOSITY (2)", skip = 1)

head(buryat_relig)
glimpse(buryat_relig)

# Remove some unnecessary, text, and/or empty variables
buryat_relig <- buryat_relig %>%
  select(-c(DATE, TIME, LOCATION, NAME, ...21, ...39, ...40, ...72, BGPUNHO, LGPUNHO, LGDIEHO, CORELSIM, OUTLIST)) %>%
  rename(CERCID = PARTID, INTERVIEWER_RELIG = INTERVIEWER)

# Check the data and edit to match ERM names and coding
buryat_relig <- buryat_relig %>%
  mutate(BGSTEAL = as.numeric(BGSTEAL)) %>%
  mutate(LGFREQW = as.numeric(LGFREQW)) %>%
  mutate(CORELEMO = as.numeric(CORELEMO)) %>%
  mutate(OUTGREMO = as.numeric(OUTGREMO)) %>%
  mutate(INOUTREL = as.numeric(INOUTREL)) %>%
  mutate(LGBLV = ifelse(CERCID == "BI041", NA, LGBLV)) %>% # TO CHECK THIS!
  mutate(LGSEE = ifelse(CERCID == "BI062", NA, LGSEE)) %>% # TO CHECK THIS!
  mutate(CORELEMO = ifelse(CERCID == "BI076", NA, CORELEMO)) # TO CHECK THIS!

# The similarity between co-religionists and in-group, and out-group and in-group also needs some work, but as not needed here will just drop them
buryat_relig <- buryat_relig %>%
  select(-c(CORINSIM:OUTPEOPLE))


### Combine Buryat demographics, game and religiosity data together
names(buryat_demo)
names(buryat_game)
names(buryat_relig)

buryat_main <- full_join(buryat_demo, buryat_game, by = "CERCID")
buryat_main <- full_join(buryat_main, buryat_relig, by = "CERCID")


## Keep just variables needed for analyses
buryat_main <- buryat_main %>%
  select(c(CERCID, SEX, AGE, CHILDREN, FORMALED, MMAT, 
           INGFIRST, EXUSED1, COREL.L, EXUSED2, COREL.S,
           INGFIRST_DG, COREL.L_DG, COREL.S_DG,
           LGPUNISH, LGDIE, LGFEEL, LGSEE,
           BGPUNISH, BGDIE, BGFEEL, BGSEE))

## Save this dataset (in both RData and CSV formats)
save(buryat_main, file = "buryat_data.RData")
write_csv(buryat_main, file = "buryat_data.csv")


################################################################################
#### Data analysis

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("buryat_data.RData")


### Descriptive stats

## Summary of missing data
cbind(n_miss = sapply(buryat_main, function(x) sum(is.na(x))),
      per_miss = sapply(buryat_main, function(x) round(sum(is.na(x)) / nrow(buryat_main) * 100, 2)))

# Only one variable - LGSEE - has missing data (and only one case missing)
buryat_main %>%
  filter(is.na(LGSEE)) %>%
  select(c(CERCID))


## Demographics

# Age (years)
summary(buryat_main$AGE)
sd(buryat_main$AGE)

plot(density(buryat_main$AGE), main = "", xlab = "Age")
hist(buryat_main$AGE, main = "", xlab = "Age")

# Sex (1 = male)
addmargins(table(buryat_main$SEX))
round(prop.table(table(buryat_main$SEX)) * 100, 1)

# Children
addmargins(table(buryat_main$CHILDREN))
round(prop.table(table(buryat_main$CHILDREN)) * 100, 1)

summary(buryat_main$CHILDREN)
sd(buryat_main$CHILDREN, na.rm = TRUE)

plot(density(buryat_main$CHILDREN, na.rm = TRUE), main = "", xlab = "Number of children")
hist(buryat_main$CHILDREN, main = "", xlab = "Number of children")

# Education
addmargins(table(buryat_main$FORMALED))
round(prop.table(table(buryat_main$FORMALED)) * 100, 1)

summary(buryat_main$FORMALED)
sd(buryat_main$FORMALED, na.rm = TRUE)

plot(density(buryat_main$FORMALED, na.rm = TRUE), main = "", xlab = "Number of years education")
hist(buryat_main$FORMALED, main = "", xlab = "Number of years education")

# Material insecurity
addmargins(table(buryat_main$MMAT))
round(prop.table(table(buryat_main$MMAT)) * 100, 1)

summary(buryat_main$MMAT)

plot(density(buryat_main$MMAT, na.rm = TRUE), main = "", xlab = "Food insecurity")
hist(buryat_main$MMAT, main = "", xlab = "Food insecurity")


## Religiosity

# MG punish
addmargins(table(buryat_main$BGPUNISH))
round(prop.table(table(buryat_main$BGPUNISH)) * 100, 1)

# LG punish
addmargins(table(buryat_main$LGPUNISH))
round(prop.table(table(buryat_main$LGPUNISH)) * 100, 1)

# Tetrachoric correlation between MG and LG PUNISH
addmargins(table(buryat_main$LGPUNISH, buryat_main$BGPUNISH))
round(prop.table(table(buryat_main$LGPUNISH, buryat_main$BGPUNISH), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$LGPUNISH, buryat_main$BGPUNISH))


# MG afterlife
addmargins(table(buryat_main$BGDIE))
round(prop.table(table(buryat_main$BGDIE)) * 100, 1)

# LG afterlife
addmargins(table(buryat_main$LGDIE))
round(prop.table(table(buryat_main$LGDIE)) * 100, 1)

# Tetrachoric correlation between MG and LG DIE
addmargins(table(buryat_main$LGDIE, buryat_main$BGDIE))
round(prop.table(table(buryat_main$LGDIE, buryat_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$LGDIE, buryat_main$BGDIE))


# Tetrachoric correlation between PUNISH and DIE for both MG and LG
addmargins(table(buryat_main$LGPUNISH, buryat_main$LGDIE))
round(prop.table(table(buryat_main$LGPUNISH, buryat_main$LGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$LGPUNISH, buryat_main$LGDIE))

addmargins(table(buryat_main$BGPUNISH, buryat_main$BGDIE))
round(prop.table(table(buryat_main$BGPUNISH, buryat_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$BGPUNISH, buryat_main$BGDIE))


# MG and LG punishment mini-scales
buryat_main <- buryat_main %>%
  mutate(BGPUN_AVE = rowMeans(select(., BGPUNISH, BGDIE), na.rm = FALSE)) %>%
  mutate(LGPUN_AVE = rowMeans(select(., LGPUNISH, LGDIE), na.rm = FALSE))

# MG
addmargins(table(buryat_main$BGPUN_AVE))
round(prop.table(table(buryat_main$BGPUN_AVE)) * 100, 1)

summary(buryat_main$BGPUN_AVE)

# LG
addmargins(table(buryat_main$LGPUN_AVE))
round(prop.table(table(buryat_main$LGPUN_AVE)) * 100, 1)

summary(buryat_main$LGPUN_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(buryat_main$BGPUN_AVE)), main = "MG Punish", ylim = c(0, 0.6))
barplot(prop.table(table(buryat_main$LGPUN_AVE)), main = "LG Punish", ylim = c(0, 0.6))
dev.off()

# Correlation between punishment mini-scales
addmargins(table(buryat_main$LGPUN_AVE, buryat_main$BGPUN_AVE))
round(prop.table(table(buryat_main$LGPUN_AVE, buryat_main$BGPUN_AVE), margin = 1) * 100, 1)

cor.test(buryat_main$LGPUN_AVE, buryat_main$BGPUN_AVE)
polychoric(cbind(as.factor(buryat_main$LGPUN_AVE), as.factor(buryat_main$BGPUN_AVE)))


# MG feel
addmargins(table(buryat_main$BGFEEL))
round(prop.table(table(buryat_main$BGFEEL)) * 100, 1)

# LG feel
addmargins(table(buryat_main$LGFEEL))
round(prop.table(table(buryat_main$LGFEEL)) * 100, 1)

# Tetrachoric correlation between MG and LG FEEL
addmargins(table(buryat_main$LGFEEL, buryat_main$BGFEEL))
round(prop.table(table(buryat_main$LGFEEL, buryat_main$BGFEEL), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$LGFEEL, buryat_main$BGFEEL))


# MG see
addmargins(table(buryat_main$BGSEE))
round(prop.table(table(buryat_main$BGSEE)) * 100, 1)

# LG see
addmargins(table(buryat_main$LGSEE))
round(prop.table(table(buryat_main$LGSEE)) * 100, 1)

# Tetrachoric correlation between MG and LG SEE
addmargins(table(buryat_main$LGSEE, buryat_main$BGSEE))
round(prop.table(table(buryat_main$LGSEE, buryat_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$LGSEE, buryat_main$BGSEE))


# Tetrachoric correlation between FEEL and SEE for both MG and LG
addmargins(table(buryat_main$LGFEEL, buryat_main$LGSEE))
round(prop.table(table(buryat_main$LGFEEL, buryat_main$LGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$LGFEEL, buryat_main$LGSEE))

addmargins(table(buryat_main$BGFEEL, buryat_main$BGSEE))
round(prop.table(table(buryat_main$BGFEEL, buryat_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(buryat_main$BGFEEL, buryat_main$BGSEE))


# MG and LG mini-omniscience scales
buryat_main <- buryat_main %>%
  mutate(BGOMNI_AVE = rowMeans(select(., BGFEEL, BGSEE), na.rm = FALSE)) %>%
  mutate(LGOMNI_AVE = rowMeans(select(., LGFEEL, LGSEE), na.rm = FALSE))

# MG
addmargins(table(buryat_main$BGOMNI_AVE))
round(prop.table(table(buryat_main$BGOMNI_AVE)) * 100, 1)

summary(buryat_main$BGOMNI_AVE)

# LG
addmargins(table(buryat_main$LGOMNI_AVE))
round(prop.table(table(buryat_main$LGOMNI_AVE)) * 100, 1)

summary(buryat_main$LGOMNI_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(buryat_main$BGOMNI_AVE)), main = "MG omniscience", ylim = c(0, 0.6))
barplot(prop.table(table(buryat_main$LGOMNI_AVE)), main = "LG omniscience", ylim = c(0, 0.6))
dev.off()

# Correlation between omniscience mini-scales
addmargins(table(buryat_main$LGOMNI_AVE, buryat_main$BGOMNI_AVE))
round(prop.table(table(buryat_main$LGOMNI_AVE, buryat_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(buryat_main$LGOMNI_AVE, buryat_main$BGOMNI_AVE)
polychoric(cbind(as.factor(buryat_main$LGOMNI_AVE), as.factor(buryat_main$BGOMNI_AVE)))


# Correlation between punishment and omniscience mini-scales for both MG and LG
addmargins(table(buryat_main$LGPUN_AVE, buryat_main$LGOMNI_AVE))
round(prop.table(table(buryat_main$LGPUN_AVE, buryat_main$LGOMNI_AVE), margin = 1) * 100, 1)

cor.test(buryat_main$LGPUN_AVE, buryat_main$LGOMNI_AVE)
polychoric(cbind(as.factor(buryat_main$LGPUN_AVE), as.factor(buryat_main$LGOMNI_AVE)))

addmargins(table(buryat_main$BGPUN_AVE, buryat_main$BGOMNI_AVE))
round(prop.table(table(buryat_main$BGPUN_AVE, buryat_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(buryat_main$BGPUN_AVE, buryat_main$BGOMNI_AVE)
polychoric(cbind(as.factor(buryat_main$BGPUN_AVE), as.factor(buryat_main$BGOMNI_AVE)))


### Cooperation

## RAGs

# Order (1 = local game first)
addmargins(table(buryat_main$INGFIRST))
round(prop.table(table(buryat_main$INGFIRST)) * 100, 1)


## Local RAG (RAG 1)

# Comprehension checks
addmargins(table(buryat_main$EXUSED1))
round(prop.table(table(buryat_main$EXUSED1)) * 100, 1)

# Tokens to distant co-religionist
summary(buryat_main$COREL.L)
sd(buryat_main$COREL.L, na.rm = TRUE)


## Self RAG (RAG 2)

# Comprehension checks
addmargins(table(buryat_main$EXUSED2))
round(prop.table(table(buryat_main$EXUSED2)) * 100, 1)

# Tokens to distant co-religionist
summary(buryat_main$COREL.S)
sd(buryat_main$COREL.S, na.rm = TRUE)


## Plot of RAG results to theoretical binomial distribution
pdf(file = "Buryat_RAGtoBinom.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

# Local RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "LOCAL RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.15), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(buryat_main$COREL.L, na.rm = TRUE))
polygon(density(buryat_main$COREL.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(buryat_main$COREL.L, na.rm = TRUE), lty = "dashed", col = "red")

# Self RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "SELF RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.15), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(buryat_main$COREL.S, na.rm = TRUE))
polygon(density(buryat_main$COREL.S, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(buryat_main$COREL.S, na.rm = TRUE), lty = "dashed", col = "red")

dev.off()


### Test whether RAG results differ from unbiased binomial 50/50 distribution

# LOCAL RAG
mod_RAG.L_base <- brm(COREL.L | trials(30) ~ 1,
                      family = "binomial",
                      data = buryat_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11704)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_base)

# 95% CIs of log-odds includes 0/odds include 1, suggesting little bias towards either local or distant recipients (i.e., largely unbiased allocations), although is a slight bias towards local recipients
exp(fixef(mod_RAG.L_base))


# SELF RAG
mod_RAG.S_base <- brm(COREL.S | trials(30) ~ 1,
                      family = "binomial",
                      data = buryat_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 79933)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_base)

# 95% CIs of log-odds includes 0/odds include 1, suggesting little bias towards either self or distant recipients (i.e., largely unbiased allocations), although is a slight bias towards self
exp(fixef(mod_RAG.S_base))


## DGs

# Order (1 = local game first)
addmargins(table(buryat_main$INGFIRST_DG))
round(prop.table(table(buryat_main$INGFIRST_DG)) * 100, 1)


## Local DG (DG 1)

# Tokens to distant co-religionist
summary(buryat_main$COREL.L_DG)
sd(buryat_main$COREL.L_DG, na.rm = TRUE)

addmargins(table(buryat_main$COREL.L_DG))
round(prop.table(table(buryat_main$COREL.L_DG)) * 100, 1)


## Self DG (RAG 2)

# Tokens to distant co-religionist
summary(buryat_main$COREL.S_DG)
sd(buryat_main$COREL.S_DG, na.rm = TRUE)

addmargins(table(buryat_main$COREL.S_DG))
round(prop.table(table(buryat_main$COREL.S_DG)) * 100, 1)


## Plot of DG results
pdf(file = "Buryat_DGDescriptives.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(buryat_main$COREL.L_DG, na.rm = TRUE), 
     main = "LOCAL DG", xlab = "Donations to Distant",
     ylim = c(0, 0.65), xlim = c(-1, 11))
polygon(density(buryat_main$COREL.L_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(buryat_main$COREL.L_DG, na.rm = TRUE), lty = "dashed")

plot(density(buryat_main$COREL.S_DG, na.rm = TRUE), 
     main = "SELF DG", xlab = "Donations to Distant",
     ylim = c(0, 0.65), xlim = c(-1, 11))
polygon(density(buryat_main$COREL.S_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(buryat_main$COREL.S_DG, na.rm = TRUE), lty = "dashed")

dev.off()


## Formal test of difference between local and self DG donations
df_temp <- as.data.frame(cbind(DG = c(buryat_main$COREL.L_DG, buryat_main$COREL.S_DG), 
                               Cond = c(rep("Local", nrow(buryat_main)), rep("Self", nrow(buryat_main))),
                               CERCID = c(buryat_main$CERCID, buryat_main$CERCID)))

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
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 9327)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_DG.LvsS, variables = "Cond", type = "prediction")



#################################
### Main regression analyses using causal model (with age, sex, education, number of children and material security as confounders, plus comprehension checks [for RAGs] and game order as covariates)

## Remove missing data
dat_cc <- buryat_main[complete.cases(buryat_main$COREL.L, buryat_main$COREL.S,
                                    buryat_main$COREL.L_DG, buryat_main$COREL.S_DG,
                                    buryat_main$LGPUN_AVE, buryat_main$LGOMNI_AVE,
                                    buryat_main$LGPUNISH, buryat_main$LGFEEL,
                                    buryat_main$BGPUN_AVE, buryat_main$BGOMNI_AVE,
                                    buryat_main$BGPUNISH, buryat_main$BGFEEL,
                                    buryat_main$AGE, buryat_main$SEX, buryat_main$FORMALED,
                                    buryat_main$MMAT, buryat_main$CHILDREN,
                                    buryat_main$EXUSED1, buryat_main$EXUSED2,
                                    buryat_main$INGFIRST, buryat_main$INGFIRST_DG), ]

### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN +
                   EXUSED1 + INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 18268)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L)

# Add model fit, so can compare models
mod_RAG.L <- add_criterion(mod_RAG.L, "loo")
loo_RAG.L <- loo(mod_RAG.L)
loo_RAG.L


## Interaction model
mod_RAG.L_int <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN +
                   EXUSED1 + INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 85939)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_int)

# Add model fit, so can compare models
mod_RAG.L_int <- add_criterion(mod_RAG.L_int, "loo")
loo_RAG.L_int <- loo(mod_RAG.L_int)
loo_RAG.L_int

# Compare to base model without interaction term - Interaction model is a marginally better fit to the data, but not by much (elpd diff = -1.4, SE diff = 2.2). Either way, to be on safe side will perform g-computation using both model specs
loo_compare(loo_RAG.L, loo_RAG.L_int)


## G-computation (based on additive model)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.L, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.L_0 <- rep(NA, nrow(sup0))
RAG.L_1 <- rep(NA, nrow(sup0))
diff_RAG.L <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.L_0[i] <- mean(sup0[i, ])
  RAG.L_1[i] <- mean(sup1[i, ])
  diff_RAG.L[i] <- RAG.L_1[i] - RAG.L_0[i]
}

plot(density(diff_RAG.L), main = "Local RAG (additive model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L)
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))

quantile(RAG.L_0, c(0.025, 0.5, 0.975))
quantile(RAG.L_1, c(0.025, 0.5, 0.975))


## G-computation (based on interaction model)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_int, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.L_int, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.L_0 <- rep(NA, nrow(sup0))
RAG.L_1 <- rep(NA, nrow(sup0))
diff_RAG.L <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.L_0[i] <- mean(sup0[i, ])
  RAG.L_1[i] <- mean(sup1[i, ])
  diff_RAG.L[i] <- RAG.L_1[i] - RAG.L_0[i]
}

plot(density(diff_RAG.L), main = "Local RAG (Interaction model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L)
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))

quantile(RAG.L_0, c(0.025, 0.5, 0.975))
quantile(RAG.L_1, c(0.025, 0.5, 0.975))

### No meaningful difference in results between additive and non-additive model


### RAG - DISTANT vs SELF

## Additive model
mod_RAG.S <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN +
                   EXUSED2 + INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 6785)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S)

# Add model fit, so can compare models
mod_RAG.S <- add_criterion(mod_RAG.S, "loo")
loo_RAG.S <- loo(mod_RAG.S)
loo_RAG.S


## Interaction model
mod_RAG.S_int <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN +
                       EXUSED2 + INGFIRST,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 76209)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_int)

# Add model fit, so can compare models
mod_RAG.S_int <- add_criterion(mod_RAG.S_int, "loo")
loo_RAG.S_int <- loo(mod_RAG.S_int)
loo_RAG.S_int

# Compare to base model without interaction term - No improvement in model fit with interaction
loo_compare(loo_RAG.S, loo_RAG.S_int)


## G-computation (based on additive model)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED2 = dat_cc$EXUSED2, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED2 = dat_cc$EXUSED2, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.S, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.S_0 <- rep(NA, nrow(sup0))
RAG.S_1 <- rep(NA, nrow(sup0))
diff_RAG.S <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.S_0[i] <- mean(sup0[i, ])
  RAG.S_1[i] <- mean(sup1[i, ])
  diff_RAG.S[i] <- RAG.S_1[i] - RAG.S_0[i]
}

plot(density(diff_RAG.S), main = "Self RAG", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S)
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))

quantile(RAG.S_0, c(0.025, 0.5, 0.975))
quantile(RAG.S_1, c(0.025, 0.5, 0.975))


## Density plots of RAG results
pdf(file = "Buryat_RAG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.4), xlim = c(-6, 3),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.4), xlim = c(-6, 3),
     main = "SELF RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_RAG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


### DG - DISTANT vs LOCAL

# Make sure outcome is an ordered factor
dat_cc$COREL.L_DG <- factor(dat_cc$COREL.L_DG, ordered = TRUE)

## Additive model
mod_DG.L <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                  INGFIRST_DG,
                 family = cumulative("logit"),
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 28169)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L)

# Add model fit, so can compare models
mod_DG.L <- add_criterion(mod_DG.L, "loo")
loo_DG.L <- loo(mod_DG.L)
loo_DG.L


## Interaction model
mod_DG.L_int <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = dat_cc,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 80103)

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
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup0, "levels"))) {
  sup0[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}

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
dat_cc$COREL.S_DG <- factor(dat_cc$COREL.S_DG, ordered = TRUE)

## Additive model
mod_DG.S <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = dat_cc,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 61939)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S)

# Add model fit, so can compare models
mod_DG.S <- add_criterion(mod_DG.S, "loo")
loo_DG.S <- loo(mod_DG.S)
loo_DG.S


## Interaction model
mod_DG.S_int <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = dat_cc,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 56354)

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
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup0, "levels"))) {
  sup0[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.S, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}

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
pdf(file = "Buryat_DG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 1), xlim = c(-3, 4),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 1), xlim = c(-3, 4),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


#################################################################
#### Sensitivity analysis 1: Inclusion of moralistic god (Buddha) terms in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         EXUSED1 + INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 86419)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.L_sens1, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.L_0_sens1 <- rep(NA, nrow(sup0))
RAG.L_1_sens1 <- rep(NA, nrow(sup0))
diff_RAG.L_sens1 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.L_0_sens1[i] <- mean(sup0[i, ])
  RAG.L_1_sens1[i] <- mean(sup1[i, ])
  diff_RAG.L_sens1[i] <- RAG.L_1_sens1[i] - RAG.L_0_sens1[i]
}

plot(density(diff_RAG.L_sens1), main = "Local RAG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens1)
quantile(diff_RAG.L_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens1, c(0.025, 0.5, 0.975))

# Less negative than original model, but no real difference in interpretation (wider CIs though)
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   BGPUN_AVE + BGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN +
                   EXUSED2 + INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 21124)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED2 = dat_cc$EXUSED2, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED2 = dat_cc$EXUSED2, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.S_sens1, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.S_0_sens1 <- rep(NA, nrow(sup0))
RAG.S_1_sens1 <- rep(NA, nrow(sup0))
diff_RAG.S_sens1 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.S_0_sens1[i] <- mean(sup0[i, ])
  RAG.S_1_sens1[i] <- mean(sup1[i, ])
  diff_RAG.S_sens1[i] <- RAG.S_1_sens1[i] - RAG.S_0_sens1[i]
}

plot(density(diff_RAG.S_sens1), main = "Self RAG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens1)
quantile(diff_RAG.S_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens1, c(0.025, 0.5, 0.975))

# Less negative than original model, but no real difference in interpretation (slightly wider CIs though)
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens1 <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE +
                        AGE + SEX + FORMALED + MMAT + CHILDREN +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 27227)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L_sens1, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup0, "levels"))) {
  sup0[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L_sens1, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}

## Difference between counter-factual states from each of the posterior samples
DG.L_0_sens1 <- rep(NA, nrow(sup0))
DG.L_1_sens1 <- rep(NA, nrow(sup0))
diff_DG.L_sens1 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.L_0_sens1[i] <- mean(sup0[i, ])
  DG.L_1_sens1[i] <- mean(sup1[i, ])
  diff_DG.L_sens1[i] <- DG.L_1_sens1[i] - DG.L_0_sens1[i]
}

plot(density(diff_DG.L_sens1), main = "Local DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens1)
quantile(diff_DG.L_sens1, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens1, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens1 <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE +
                        AGE + SEX + FORMALED + MMAT + CHILDREN +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 57237)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S_sens1, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup0, "levels"))) {
  sup0[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.S_sens1, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}

## Difference between counter-factual states from each of the posterior samples
DG.S_0_sens1 <- rep(NA, nrow(sup0))
DG.S_1_sens1 <- rep(NA, nrow(sup0))
diff_DG.S_sens1 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.S_0_sens1[i] <- mean(sup0[i, ])
  DG.S_1_sens1[i] <- mean(sup1[i, ])
  diff_DG.S_sens1[i] <- DG.S_1_sens1[i] - DG.S_0_sens1[i]
}

plot(density(diff_DG.S_sens1), main = "Self DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens1)
quantile(diff_DG.S_sens1, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens1, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))


#################################################################
#### Sensitivity analysis 2: Use of single variables for punishment and omniscience, rather than mini-scales

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens2 <- brm(COREL.L | trials(30) ~ LGPUNISH + LGFEEL +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         EXUSED1 + INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 89136)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_sens2, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED1 = dat_cc$EXUSED1, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.L_sens2, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.L_0_sens2 <- rep(NA, nrow(sup0))
RAG.L_1_sens2 <- rep(NA, nrow(sup0))
diff_RAG.L_sens2 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.L_0_sens2[i] <- mean(sup0[i, ])
  RAG.L_1_sens2[i] <- mean(sup1[i, ])
  diff_RAG.L_sens2[i] <- RAG.L_1_sens2[i] - RAG.L_0_sens2[i]
}

plot(density(diff_RAG.L_sens2), main = "Local RAG (Single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens2)
quantile(diff_RAG.L_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens2, c(0.025, 0.5, 0.975))

# Less negative than original model, but no real difference in interpretation
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens2 <- brm(COREL.S | trials(30) ~ LGPUNISH + LGFEEL +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         EXUSED2 + INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 18474)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED2 = dat_cc$EXUSED2, INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S_sens2, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               EXUSED2 = dat_cc$EXUSED2, INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.S_sens2, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.S_0_sens2 <- rep(NA, nrow(sup0))
RAG.S_1_sens2 <- rep(NA, nrow(sup0))
diff_RAG.S_sens2 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.S_0_sens2[i] <- mean(sup0[i, ])
  RAG.S_1_sens2[i] <- mean(sup1[i, ])
  diff_RAG.S_sens2[i] <- RAG.S_1_sens2[i] - RAG.S_0_sens2[i]
}

plot(density(diff_RAG.S_sens2), main = "Self RAG (Single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens2)
quantile(diff_RAG.S_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens2 <- brm(COREL.L_DG ~ LGPUNISH + LGFEEL +
                        AGE + SEX + FORMALED + MMAT + CHILDREN +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 90436)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L_sens2, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup0, "levels"))) {
  sup0[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L_sens2, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}

## Difference between counter-factual states from each of the posterior samples
DG.L_0_sens2 <- rep(NA, nrow(sup0))
DG.L_1_sens2 <- rep(NA, nrow(sup0))
diff_DG.L_sens2 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.L_0_sens2[i] <- mean(sup0[i, ])
  DG.L_1_sens2[i] <- mean(sup1[i, ])
  diff_DG.L_sens2[i] <- DG.L_1_sens2[i] - DG.L_0_sens2[i]
}

plot(density(diff_DG.L_sens2), main = "Local DG (Single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens2)
quantile(diff_DG.L_sens2, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens2 <- brm(COREL.S_DG ~ LGPUNISH + LGFEEL +
                        AGE + SEX + FORMALED + MMAT + CHILDREN +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 30695)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S_sens2, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup0, "levels"))) {
  sup0[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.S_sens2, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}

## Difference between counter-factual states from each of the posterior samples
DG.S_0_sens2 <- rep(NA, nrow(sup0))
DG.S_1_sens2 <- rep(NA, nrow(sup0))
diff_DG.S_sens2 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.S_0_sens2[i] <- mean(sup0[i, ])
  DG.S_1_sens2[i] <- mean(sup1[i, ])
  diff_DG.S_sens2[i] <- DG.S_1_sens2[i] - DG.S_0_sens2[i]
}

plot(density(diff_DG.S_sens2), main = "Self DG (Single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens2)
quantile(diff_DG.S_sens2, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))

