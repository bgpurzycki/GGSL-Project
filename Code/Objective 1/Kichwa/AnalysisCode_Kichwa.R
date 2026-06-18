### Objective 1 Analysis Code - Kichwa
### Created 02/06/2026 by Dan Major-Smith
### R 4.4.1

####################################################################
#### Clear workspace, set working directory and install/load packages
rm(list=ls())
Sys.setenv(LANG = "en")

setwd("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/Projects/Connor_Kichwa/Objective 1 - Kichwa/")
#setwd("")


#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

#install.packages("psych")
library(psych)

#install.packages("mice")
library(mice)

## Note that, for brms to work, 'stan' will also need to be installed on your computer - see https://mc-stan.org/install/
#install.packages("brms")
library(brms)

#install.packages("marginaleffects")
library(marginaleffects)


#####################################################################
### Read in and process Kichwa data (note that this step processes the raw data which is not openly-available. To follow these analyses, read in the data on line 240)

## Demographics first
kichwa_demo <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Ecuador - Wood/Data_v3/English/DEMOGRAPHICS English Anon FINAL_DMS.xlsx",
                          sheet = "Demographics")

head(kichwa_demo)
glimpse(kichwa_demo)

# Remove some empty variables, unused IDs and empty rows
kichwa_demo <- kichwa_demo %>%
  filter(!is.na(PARTID)) %>%
  select(-DATE, -TIME, -SEXRA, -SESSION)

# Check data against ERM and make sure named and coded the same
kichwa_demo <- kichwa_demo %>%
  rename(CERCID = PARTID, SITE_SPEC = SITE) %>%
  mutate(SITE = "Kichwa") %>%
  mutate(RESEARCHER = "Wood") %>%
  relocate(CERCID, RESEARCHER, SITE, SITE_SPEC, .before = LOCATION)

# Go through variables, check sensible, and convert to numeric if needed
table(kichwa_demo$SEX)
table(kichwa_demo$AGE)
table(kichwa_demo$FAMILY)
table(kichwa_demo$CHILDREN)
table(kichwa_demo$FORMALED)
table(kichwa_demo$MAT1)
table(kichwa_demo$MAT1C)
table(kichwa_demo$MAT2)
table(kichwa_demo$MAT2C)
table(kichwa_demo$MAT3)
table(kichwa_demo$MAT3C)
table(kichwa_demo$MAT4)
table(kichwa_demo$MAT4C)

kichwa_demo <- kichwa_demo %>%
  mutate(FAMILY = ifelse(FAMILY == "6 (free union)", "6", FAMILY)) %>%
  mutate(FAMILY = as.numeric(FAMILY)) %>%
  mutate(CHILDREN = ifelse(str_length(CHILDREN) > 3, str_sub(CHILDREN, 1, 1), CHILDREN)) %>%
  mutate(CHILDREN = as.numeric(CHILDREN)) %>%
  mutate(FORMALED = as.numeric(FORMALED))


# Make the derived material security variables
kichwa_demo <- kichwa_demo %>%
  mutate(MMAT = rowMeans(select(., MAT1, MAT2, MAT3, MAT4), na.rm = TRUE)) %>%
  mutate(MMATc = rowMeans(select(., MAT1C, MAT2C, MAT3C, MAT4C), na.rm = TRUE))


## Read in the game data
kichwa_game <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Ecuador - Wood/Data_v3/English/EXPERIMENTS English FINAL.xlsx",
                          sheet = "Data",
                          skip = 1)

head(kichwa_game)
glimpse(kichwa_game)

# Remove some empty variables, post-game text data, and unused IDs
kichwa_game <- kichwa_game %>%
  select(-c(SITE, DATE, TIME, LOCATION, SESSNUM, GA1, GA2)) %>%
  rename(CERCID = ID, INTERVIEWER_GAMES = INTERVIEWER) %>%
  relocate(CERCID, .before = INTERVIEWER_GAMES)

# Check the data and edit to match ERM names and coding
kichwa_game <- kichwa_game %>%
  rename(ORDER = ORDERRAG) %>%
  mutate(INGFIRST = ifelse(ORDER == "1", 1, 0)) %>%
  relocate(INGFIRST, .after = ORDER) %>%
  rename(PRE1_1 = '1P1', PRE1_2 = '1P2', PRE1_3 = '1P3', PRE1_4 = '1P4', PRE1_5 = '1P5',
         PRE2_1 = '2P1', PRE2_2 = '2P2', PRE2_3 = '2P3', PRE2_4 = '2P4', PRE2_5 = '2P5') %>%
  rename(COREL.L = DISTANT1, INGROUP = INGROUP...18, COREL.S = DISTANT2, SELF = SELF...28) %>%
  mutate(SUM1 = COREL.L + INGROUP) %>%
  mutate(SUM2 = COREL.S + SELF) %>%
  relocate(COREL.L, INGROUP, SUM1, .after = EXUSED1) %>%
  relocate(COREL.S, SELF, SUM2, .after = EXUSED2) %>%
  mutate(INGFIRST_DG = ifelse(ORDERDG == "1", 1, 0)) %>%
  relocate(INGFIRST_DG, .after = ORDERDG) %>%
  rename(COREL.L_DG = DISTANT...32, INGROUP_DG = INGROUP...31, COREL.S_DG = DISTANT...34, SELF_DG = SELF...33) %>%
  mutate(SUM1_DG = COREL.L_DG + INGROUP_DG) %>%
  mutate(SUM2_DG = COREL.S_DG + SELF_DG) %>%
  relocate(COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG, .after = INGFIRST_DG)


# Check if any RAG totals do not sum to 30 - Six do not, but we know this already
kichwa_game %>%
  filter(SUM1 != 30 | SUM2 != 30) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L, INGROUP, SUM1, COREL.S, SELF, SUM2)

# Check if any DG totals do not sum to 10 - Two do not, but we know this already
kichwa_game %>%
  filter(SUM1_DG != 10 | SUM2_DG != 10) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG)

# If totals not correct, code data as missing
kichwa_game <- kichwa_game %>%
  mutate(COREL.L = ifelse(SUM1 != 30, NA, COREL.L)) %>%
  mutate(INGROUP = ifelse(SUM1 != 30, NA, INGROUP)) %>%
  mutate(SUM1 = ifelse(SUM1 != 30, NA, SUM1)) %>%
  mutate(COREL.S = ifelse(SUM2 != 30, NA, COREL.S)) %>%
  mutate(SELF = ifelse(SUM2 != 30, NA, SELF)) %>%
  mutate(SUM2 = ifelse(SUM2 != 30, NA, SUM2)) %>%
  mutate(COREL.L_DG = ifelse(SUM1_DG != 10, NA, COREL.L_DG)) %>%
  mutate(INGROUP_DG = ifelse(SUM1_DG != 10, NA, INGROUP_DG)) %>%
  mutate(SUM1_DG = ifelse(SUM1_DG != 10, NA, SUM1_DG)) %>%
  mutate(COREL.S_DG = ifelse(SUM2_DG != 10, NA, COREL.S_DG)) %>%
  mutate(SELF_DG = ifelse(SUM2_DG != 10, NA, SELF_DG)) %>%
  mutate(SUM2_DG = ifelse(SUM2_DG != 10, NA, SUM2_DG))


### Now for the religiosity interview data
kichwa_relig <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Ecuador - Wood/Data_v3/English/RELIGIOSITY English Anon FINAL_DMS.xlsx",
                           sheet = "RELIGIOSITY", skip = 2)

head(kichwa_relig)
glimpse(kichwa_relig)


# Remove some unnecessary, text, and/or empty variables
kichwa_relig <- kichwa_relig %>%
  select(-c(DATE, TIME, LOCATION, BGPUND, BGDIED, LGPUNISHD, `LGDIE 2`, CORELD, OUTGD)) %>%
  rename(CERCID = ID, INTERVIEWER_RELIG = INTERVIEWER)

# Drop one person who refused the religiosity questions
kichwa_relig <- kichwa_relig %>%
  filter(BGPUNISH != "DENIED")

# Check the data and edit to match ERM names and coding (focus on the relevant variables here, for now)
table(kichwa_relig$BGPUNISH)
table(kichwa_relig$BGDIE)
table(kichwa_relig$BGFEEL)
table(kichwa_relig$BGSEE)
table(kichwa_relig$LGPUNISH)
table(kichwa_relig$LGDIE)
table(kichwa_relig$LGFEEL)
table(kichwa_relig$LGSEE)
table(kichwa_relig$BGPERFHO)
table(kichwa_relig$LGPERFHO)
table(kichwa_relig$BGBLV)
table(kichwa_relig$BGTHINK)
table(kichwa_relig$LGBLV)
table(kichwa_relig$LGTHINK)


kichwa_relig <- kichwa_relig %>%
  mutate(BGPUNISH = as.numeric(BGPUNISH)) %>%
  mutate(BGDIE = as.numeric(BGDIE)) %>%
  mutate(BGFEEL = as.numeric(BGFEEL)) %>%
  mutate(BGSEE = as.numeric(BGSEE)) %>%
  mutate(LGPUNISH = as.numeric(LGPUNISH)) %>%
  mutate(LGDIE = as.numeric(LGDIE)) %>%
  mutate(LGFEEL = as.numeric(LGFEEL)) %>%
  mutate(LGSEE = as.numeric(LGSEE)) %>%
  mutate(BGPERFHO = as.numeric(BGPERFHO)) %>%
  mutate(LGPERFHO = as.numeric(LGPERFHO)) %>%
  mutate(BGBLV = as.numeric(BGBLV)) %>%
  mutate(LGBLV = as.numeric(LGBLV))


## Extra 'market integration' data from O2

### Read in O2 data
kichwa_market <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Ecuador - Wood/Data_v3/English/EXTRA O2 QUESTIONS FINAL English.xlsx")

head(kichwa_market)
glimpse(kichwa_market)

## Keep necessary columns and rename the ID variable
kichwa_market <- kichwa_market %>%
  select(c(ID, MARKET1, MARKET12)) %>%
  rename(CERCID = ID)

# Tidy the market variables
table(kichwa_market$MARKET1, useNA = "ifany")
table(kichwa_market$MARKET12, useNA = "ifany")

kichwa_market <- kichwa_market %>%
  mutate(MARKET12 = ifelse(is.na(MARKET12), NA,
                           ifelse(MARKET12 == "Si" | MARKET12 == "Yes", MARKET1, MARKET12))) %>%
  mutate(MARKET1 = as.numeric(MARKET1)) %>%
  mutate(MARKET12 = as.numeric(MARKET12)) %>%
  rename(MARKET2 = MARKET12)


### Combine Kichwa demographics, market, game and religiosity data together
names(kichwa_demo)
names(kichwa_market)
names(kichwa_game)
names(kichwa_relig)

kichwa_main <- full_join(kichwa_demo, kichwa_market, by = "CERCID")
kichwa_main <- full_join(kichwa_main, kichwa_game, by = "CERCID")
kichwa_main <- full_join(kichwa_main, kichwa_relig, by = "CERCID")

# Keep only those with religiosity and game data (as around 20 folks with only RLI data are in the demographics, and one person has no religiosity data)
kichwa_main <- kichwa_main %>%
  filter(!is.na(BGFEEL))


## Keep just variables needed for analyses
kichwa_main <- kichwa_main %>%
  select(c(CERCID, SEX, AGE, CHILDREN, FORMALED, MMAT, MARKET1, MARKET2,
           INGFIRST, EXUSED1, COREL.L, EXUSED2, COREL.S,
           INGFIRST_DG, COREL.L_DG, COREL.S_DG,
           LGPUNISH, LGDIE, LGFEEL, LGSEE, LGPERFHO, LGBLV, LGTHINK,
           BGPUNISH, BGDIE, BGFEEL, BGSEE, BGPERFHO, BGBLV, BGTHINK))

## Save this dataset (in both RData and CSV formats)
save(kichwa_main, file = "kichwa_data.RData")
write_csv(kichwa_main, file = "kichwa_data.csv")


################################################################################
#### Data analysis

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("kichwa_data.RData")


### Descriptive stats

## Summary of missing data
cbind(n_miss = sapply(kichwa_main, function(x) sum(is.na(x))),
      per_miss = sapply(kichwa_main, function(x) round(sum(is.na(x)) / nrow(kichwa_main) * 100, 2)))

# 57 cases with complete data (67.1%)
sum(complete.cases(kichwa_main))
round(sum(complete.cases(kichwa_main)) / nrow(kichwa_main) * 100, 1)

# 67 cases (78.8%) with complete data if exclude Catholic and auxiliary vars
temp <- kichwa_main %>%
  select(-c(BGPUNISH, BGDIE, BGFEEL, BGSEE, LGPERFHO, LGBLV, LGTHINK, BGPERFHO, BGBLV, BGTHINK, MARKET2))

sum(complete.cases(temp))
round(sum(complete.cases(temp)) / nrow(temp) * 100, 1)


## Demographics

# Age (years)
summary(kichwa_main$AGE)
sd(kichwa_main$AGE)

plot(density(kichwa_main$AGE), main = "", xlab = "Age")
hist(kichwa_main$AGE, main = "", xlab = "Age")

# Sex (1 = male)
addmargins(table(kichwa_main$SEX))
round(prop.table(table(kichwa_main$SEX)) * 100, 1)

# Children
addmargins(table(kichwa_main$CHILDREN))
round(prop.table(table(kichwa_main$CHILDREN)) * 100, 1)

summary(kichwa_main$CHILDREN)
sd(kichwa_main$CHILDREN, na.rm = TRUE)

plot(density(kichwa_main$CHILDREN, na.rm = TRUE), main = "", xlab = "Number of children")
hist(kichwa_main$CHILDREN, main = "", xlab = "Number of children")

# Education
addmargins(table(kichwa_main$FORMALED))
round(prop.table(table(kichwa_main$FORMALED)) * 100, 1)

summary(kichwa_main$FORMALED)
sd(kichwa_main$FORMALED, na.rm = TRUE)

plot(density(kichwa_main$FORMALED, na.rm = TRUE), main = "", xlab = "Number of years education")
hist(kichwa_main$FORMALED, main = "", xlab = "Number of years education")

# Material insecurity
addmargins(table(kichwa_main$MMAT))
round(prop.table(table(kichwa_main$MMAT)) * 100, 1)

summary(kichwa_main$MMAT)
sd(kichwa_main$MMAT, na.rm = TRUE)

plot(density(kichwa_main$MMAT, na.rm = TRUE), main = "", xlab = "Food insecurity")
hist(kichwa_main$MMAT, main = "", xlab = "Food insecurity")

# Market integration 1 (past day)
summary(kichwa_main$MARKET1)
sd(kichwa_main$MARKET1, na.rm = TRUE)

plot(density(kichwa_main$MARKET1, na.rm = TRUE), main = "", xlab = "Market integration (yesterday)")
hist(kichwa_main$MARKET1, main = "", xlab = "Market integration (yesterday)")

# Market integration 2 (past month)
summary(kichwa_main$MARKET2)
sd(kichwa_main$MARKET2, na.rm = TRUE)

plot(density(kichwa_main$MARKET2, na.rm = TRUE), main = "", xlab = "Market integration (month)")
hist(kichwa_main$MARKET2, main = "", xlab = "Market integration (month)")


## Religiosity

# MG punish
addmargins(table(kichwa_main$BGPUNISH))
round(prop.table(table(kichwa_main$BGPUNISH)) * 100, 1)

# LG punish
addmargins(table(kichwa_main$LGPUNISH))
round(prop.table(table(kichwa_main$LGPUNISH)) * 100, 1)

# Tetrachoric correlation between MG and LG PUNISH
addmargins(table(kichwa_main$LGPUNISH, kichwa_main$BGPUNISH))
round(prop.table(table(kichwa_main$LGPUNISH, kichwa_main$BGPUNISH), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$LGPUNISH, kichwa_main$BGPUNISH))


# MG afterlife
addmargins(table(kichwa_main$BGDIE))
round(prop.table(table(kichwa_main$BGDIE)) * 100, 1)

# LG afterlife
addmargins(table(kichwa_main$LGDIE))
round(prop.table(table(kichwa_main$LGDIE)) * 100, 1)

# Tetrachoric correlation between MG and LG DIE
addmargins(table(kichwa_main$LGDIE, kichwa_main$BGDIE))
round(prop.table(table(kichwa_main$LGDIE, kichwa_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$LGDIE, kichwa_main$BGDIE))


# Tetrachoric correlation between PUNISH and DIE for both MG and LG
addmargins(table(kichwa_main$LGPUNISH, kichwa_main$LGDIE))
round(prop.table(table(kichwa_main$LGPUNISH, kichwa_main$LGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$LGPUNISH, kichwa_main$LGDIE))

addmargins(table(kichwa_main$BGPUNISH, kichwa_main$BGDIE))
round(prop.table(table(kichwa_main$BGPUNISH, kichwa_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$BGPUNISH, kichwa_main$BGDIE))


# MG and LG mini-punishment scales
kichwa_main <- kichwa_main %>%
  mutate(BGPUN_AVE = rowMeans(select(., BGPUNISH, BGDIE), na.rm = FALSE)) %>%
  mutate(LGPUN_AVE = rowMeans(select(., LGPUNISH, LGDIE), na.rm = FALSE))

# MG
addmargins(table(kichwa_main$BGPUN_AVE))
round(prop.table(table(kichwa_main$BGPUN_AVE)) * 100, 1)

summary(kichwa_main$BGPUN_AVE)

# LG
addmargins(table(kichwa_main$LGPUN_AVE))
round(prop.table(table(kichwa_main$LGPUN_AVE)) * 100, 1)

summary(kichwa_main$LGPUN_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(kichwa_main$BGPUN_AVE)), main = "MG Punish", ylim = c(0, 0.7))
barplot(prop.table(table(kichwa_main$LGPUN_AVE)), main = "LG Punish", ylim = c(0, 0.7))
dev.off()

# Correlation between punishment mini-scales
addmargins(table(kichwa_main$LGPUN_AVE, kichwa_main$BGPUN_AVE))
round(prop.table(table(kichwa_main$LGPUN_AVE, kichwa_main$BGPUN_AVE), margin = 1) * 100, 1)

cor.test(kichwa_main$LGPUN_AVE, kichwa_main$BGPUN_AVE)
polychoric(cbind(as.factor(kichwa_main$LGPUN_AVE), as.factor(kichwa_main$BGPUN_AVE)))


# MG feel
addmargins(table(kichwa_main$BGFEEL))
round(prop.table(table(kichwa_main$BGFEEL)) * 100, 1)

# LG feel
addmargins(table(kichwa_main$LGFEEL))
round(prop.table(table(kichwa_main$LGFEEL)) * 100, 1)

# Tetrachoric correlation between MG and LG FEEL
addmargins(table(kichwa_main$LGFEEL, kichwa_main$BGFEEL))
round(prop.table(table(kichwa_main$LGFEEL, kichwa_main$BGFEEL), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$LGFEEL, kichwa_main$BGFEEL))


# MG see
addmargins(table(kichwa_main$BGSEE))
round(prop.table(table(kichwa_main$BGSEE)) * 100, 1)

# LG see
addmargins(table(kichwa_main$LGSEE))
round(prop.table(table(kichwa_main$LGSEE)) * 100, 1)

# Tetrachoric correlation between MG and LG SEE
addmargins(table(kichwa_main$LGSEE, kichwa_main$BGSEE))
round(prop.table(table(kichwa_main$LGSEE, kichwa_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$LGSEE, kichwa_main$BGSEE))


# Tetrachoric correlation between FEEL and SEE for both MG and LG
addmargins(table(kichwa_main$LGFEEL, kichwa_main$LGSEE))
round(prop.table(table(kichwa_main$LGFEEL, kichwa_main$LGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$LGFEEL, kichwa_main$LGSEE))

addmargins(table(kichwa_main$BGFEEL, kichwa_main$BGSEE))
round(prop.table(table(kichwa_main$BGFEEL, kichwa_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(kichwa_main$BGFEEL, kichwa_main$BGSEE))


# MG and LG mini-omniscience scales
kichwa_main <- kichwa_main %>%
  mutate(BGOMNI_AVE = rowMeans(select(., BGFEEL, BGSEE), na.rm = FALSE)) %>%
  mutate(LGOMNI_AVE = rowMeans(select(., LGFEEL, LGSEE), na.rm = FALSE))

# MG
addmargins(table(kichwa_main$BGOMNI_AVE))
round(prop.table(table(kichwa_main$BGOMNI_AVE)) * 100, 1)

summary(kichwa_main$BGOMNI_AVE)

# LG
addmargins(table(kichwa_main$LGOMNI_AVE))
round(prop.table(table(kichwa_main$LGOMNI_AVE)) * 100, 1)

summary(kichwa_main$LGOMNI_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(kichwa_main$BGOMNI_AVE)), main = "MG omniscience", ylim = c(0, 0.9))
barplot(prop.table(table(kichwa_main$LGOMNI_AVE)), main = "LG omniscience", ylim = c(0, 0.9))
dev.off()

# Correlation between omniscience mini-scales
addmargins(table(kichwa_main$LGOMNI_AVE, kichwa_main$BGOMNI_AVE))
round(prop.table(table(kichwa_main$LGOMNI_AVE, kichwa_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(kichwa_main$LGOMNI_AVE, kichwa_main$BGOMNI_AVE)
polychoric(cbind(as.factor(kichwa_main$LGOMNI_AVE), as.factor(kichwa_main$BGOMNI_AVE)))


# Correlation between punishment and omniscience mini-scales for both MG and LG
addmargins(table(kichwa_main$LGPUN_AVE, kichwa_main$LGOMNI_AVE))
round(prop.table(table(kichwa_main$LGPUN_AVE, kichwa_main$LGOMNI_AVE), margin = 1) * 100, 1)

cor.test(kichwa_main$LGPUN_AVE, kichwa_main$LGOMNI_AVE)
polychoric(cbind(as.factor(kichwa_main$LGPUN_AVE), as.factor(kichwa_main$LGOMNI_AVE)))

addmargins(table(kichwa_main$BGPUN_AVE, kichwa_main$BGOMNI_AVE))
round(prop.table(table(kichwa_main$BGPUN_AVE, kichwa_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(kichwa_main$BGPUN_AVE, kichwa_main$BGOMNI_AVE)
polychoric(cbind(as.factor(kichwa_main$BGPUN_AVE), as.factor(kichwa_main$BGOMNI_AVE)))


## Descriptives of auxiliary religiosity variables for imputation

# Believes in LG
addmargins(table(kichwa_main$LGBLV))
round(prop.table(table(kichwa_main$LGBLV)) * 100, 1)

# Freq thinks about LG
addmargins(table(kichwa_main$LGTHINK))
round(prop.table(table(kichwa_main$LGTHINK)) * 100, 1)

# Performs rituals towards LG
addmargins(table(kichwa_main$LGPERFHO))
round(prop.table(table(kichwa_main$LGPERFHO)) * 100, 1)

# Believes in MG
addmargins(table(kichwa_main$BGBLV))
round(prop.table(table(kichwa_main$BGBLV)) * 100, 1)

# Freq thinks about MG
addmargins(table(kichwa_main$BGTHINK))
round(prop.table(table(kichwa_main$BGTHINK)) * 100, 1)

# Performs rituals towards MG
addmargins(table(kichwa_main$BGPERFHO))
round(prop.table(table(kichwa_main$BGPERFHO)) * 100, 1)


### Cooperation

## RAGs

# Order (1 = local game first)
addmargins(table(kichwa_main$INGFIRST))
round(prop.table(table(kichwa_main$INGFIRST)) * 100, 1)


## Local RAG (RAG 1)

# Comprehension checks
addmargins(table(kichwa_main$EXUSED1))
round(prop.table(table(kichwa_main$EXUSED1)) * 100, 1)


# Tokens to distant co-religionist
summary(kichwa_main$COREL.L)
sd(kichwa_main$COREL.L, na.rm = TRUE)


## Self RAG (RAG 2)

# Comprehension checks
addmargins(table(kichwa_main$EXUSED2))
round(prop.table(table(kichwa_main$EXUSED2)) * 100, 1)

# Tokens to distant co-religionist
summary(kichwa_main$COREL.S)
sd(kichwa_main$COREL.S, na.rm = TRUE)


## Plot of RAG results to theoretical binomial distribution
pdf(file = "kichwa_RAGtoBinom.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

# Local RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "LOCAL RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.16), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(kichwa_main$COREL.L, na.rm = TRUE))
polygon(density(kichwa_main$COREL.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(kichwa_main$COREL.L, na.rm = TRUE), lty = "dashed", col = "red")

# Self RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "SELF RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.16), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(kichwa_main$COREL.S, na.rm = TRUE))
polygon(density(kichwa_main$COREL.S, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(kichwa_main$COREL.S, na.rm = TRUE), lty = "dashed", col = "red")

dev.off()


### Test whether RAG results differ from unbiased binomial 50/50 distribution

# LOCAL RAG
mod_RAG.L_base <- brm(COREL.L | trials(30) ~ 1,
                      family = "binomial",
                      data = kichwa_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 29424)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_base)

# 95% CIs of log-odds includes 0/odds excludes 0, suggesting bias towards local recipients
exp(fixef(mod_RAG.L_base))


# SELF RAG
mod_RAG.S_base <- brm(COREL.S | trials(30) ~ 1,
                      family = "binomial",
                      data = kichwa_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 2937)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_base)

# 95% CIs of log-odds exclude 0/odds exclude 1, suggesting bias towards self rather than distant
exp(fixef(mod_RAG.S_base))


## Formal test of difference between local and self RAG donations
df_temp <- as.data.frame(cbind(RAG = c(kichwa_main$COREL.L, kichwa_main$COREL.S), 
                               Cond = c(rep("Local", nrow(kichwa_main)), rep("Self", nrow(kichwa_main))),
                               CERCID = c(kichwa_main$CERCID, kichwa_main$CERCID)))

str(df_temp)

df_temp$RAG <- as.numeric(df_temp$RAG)

## Repeated-measures aggregated binomial model, with random slopes by condition to allow this to vary by participant
mod_RAG.LvsS <- brm(RAG | trials(30) ~ Cond + (Cond | CERCID),
                    family = binomial(),
                    data = df_temp,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept),
                              prior(exponential(1), class = sd, group = CERCID),
                              prior(lkj(2), class = cor, group = CERCID)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 49511)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_RAG.LvsS, variables = "Cond", type = "prediction")


## DGs

# Order (1 = local game first)
addmargins(table(kichwa_main$INGFIRST_DG))
round(prop.table(table(kichwa_main$INGFIRST_DG)) * 100, 1)


## Local DG (DG 1)

# Tokens to distant co-religionist
summary(kichwa_main$COREL.L_DG)
sd(kichwa_main$COREL.L_DG, na.rm = TRUE)

addmargins(table(kichwa_main$COREL.L_DG))
round(prop.table(table(kichwa_main$COREL.L_DG)) * 100, 1)


## Self DG (RAG 2)

# Tokens to distant co-religionist
summary(kichwa_main$COREL.S_DG)
sd(kichwa_main$COREL.S_DG, na.rm = TRUE)

addmargins(table(kichwa_main$COREL.S_DG))
round(prop.table(table(kichwa_main$COREL.S_DG)) * 100, 1)


## Plot of DG results
pdf(file = "kichwa_DGDescriptives.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(kichwa_main$COREL.L_DG, na.rm = TRUE, adjust = 1.5), 
     main = "LOCAL DG", xlab = "Donations to Distant",
     ylim = c(0, 0.5), xlim = c(-1, 11))
polygon(density(kichwa_main$COREL.L_DG, na.rm = TRUE, adjust = 1.5), col=rgb(1, 0, 0, 0.25))
abline(v = mean(kichwa_main$COREL.L_DG, na.rm = TRUE), lty = "dashed")

plot(density(kichwa_main$COREL.S_DG, na.rm = TRUE), 
     main = "SELF DG", xlab = "Donations to Distant",
     ylim = c(0, 0.5), xlim = c(-1, 11))
polygon(density(kichwa_main$COREL.S_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(kichwa_main$COREL.S_DG, na.rm = TRUE), lty = "dashed")

dev.off()


## Formal test of difference between local and self DG donations
df_temp <- as.data.frame(cbind(DG = c(kichwa_main$COREL.L_DG, kichwa_main$COREL.S_DG), 
                               Cond = c(rep("Local", nrow(kichwa_main)), rep("Self", nrow(kichwa_main))),
                               CERCID = c(kichwa_main$CERCID, kichwa_main$CERCID)))

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
                   iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 57853)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_DG.LvsS, variables = "Cond", type = "prediction")


###########################################################################################
### Complete-case analyses: Main regression analyses using causal model (with age, sex, education, number of children, material security and market integration as confounders, plus game order and comprehension checks as covariates)

## Remove missing data
dat_cc <- kichwa_main[complete.cases(kichwa_main$COREL.L, kichwa_main$COREL.S,
                                    kichwa_main$COREL.L_DG, kichwa_main$COREL.S_DG,
                                    kichwa_main$LGPUN_AVE, kichwa_main$LGOMNI_AVE,
                                    kichwa_main$BGPUN_AVE, kichwa_main$BGOMNI_AVE,
                                    kichwa_main$BGPERFHO, kichwa_main$LGPERFHO,
                                    kichwa_main$AGE, kichwa_main$SEX, kichwa_main$FORMALED,
                                    kichwa_main$MMAT, kichwa_main$CHILDREN,
                                    kichwa_main$MARKET1, kichwa_main$MARKET2,
                                    kichwa_main$EXUSED1, kichwa_main$EXUSED2,
                                    kichwa_main$INGFIRST, kichwa_main$INGFIRST_DG), ]

### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                   INGFIRST + EXUSED1,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 92196)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L)

# Add model fit, so can compare models
mod_RAG.L <- add_criterion(mod_RAG.L, "loo")
loo_RAG.L <- loo(mod_RAG.L)
loo_RAG.L


## Interaction model
mod_RAG.L_int <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                       INGFIRST + EXUSED1,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 37230)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_int)

# Add model fit, so can compare models
mod_RAG.L_int <- add_criterion(mod_RAG.L_int, "loo")
loo_RAG.L_int <- loo(mod_RAG.L_int)
loo_RAG.L_int

# Compare to base model without interaction term - No improvement in model fit with interaction
loo_compare(loo_RAG.L, loo_RAG.L_int)


## G-computation (based on additive model)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
sup0 <- predict(mod_RAG.L, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
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


### RAG - DISTANT vs SELF

## Additive model
mod_RAG.S <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                   INGFIRST + EXUSED2,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 72264)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S)

# Add model fit, so can compare models
mod_RAG.S <- add_criterion(mod_RAG.S, "loo")
loo_RAG.S <- loo(mod_RAG.S)
loo_RAG.S


## Interaction model
mod_RAG.S_int <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                       INGFIRST + EXUSED2,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 62595)

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
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
sup0 <- predict(mod_RAG.S, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
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
pdf(file = "kichwa_RAG_gcomp_CCA.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.35), xlim = c(-6, 5),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.35), xlim = c(-6, 5),
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
                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = dat_cc,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 83333)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L)

# Add model fit, so can compare models
mod_DG.L <- add_criterion(mod_DG.L, "loo")
loo_DG.L <- loo(mod_DG.L)
loo_DG.L


## Interaction model
mod_DG.L_int <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = dat_cc,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 72567)

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
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
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
dat_cc$COREL.S_DG <- factor(dat_cc$COREL.S_DG, ordered = TRUE)

## Additive model
mod_DG.S <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = dat_cc,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 1164)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S)

# Add model fit, so can compare models
mod_DG.S <- add_criterion(mod_DG.S, "loo")
loo_DG.S <- loo(mod_DG.S)
loo_DG.S


## Interaction model
mod_DG.S_int <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = dat_cc,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 48302)

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
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
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
pdf(file = "kichwa_DG_gcomp_CCA.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 0.7), xlim = c(-5, 3),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 0.7), xlim = c(-5, 3),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


#################################################################
#### Sensitivity analysis 1: Inclusion of moralistic god (Catholic god) terms in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                         AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                         INGFIRST + EXUSED1,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 38080)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
sup0 <- predict(mod_RAG.L_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
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

plot(density(diff_RAG.L_sens1), main = "Local RAG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens1)
quantile(diff_RAG.L_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens1, c(0.025, 0.5, 0.975))

# More negative than original model, but no real difference in interpretation
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                         AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                         INGFIRST + EXUSED2,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 89145)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
sup0 <- predict(mod_RAG.S_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
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

plot(density(diff_RAG.S_sens1), main = "Self RAG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens1)
quantile(diff_RAG.S_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens1, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens1 <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                        AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 64440)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L_sens1, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L_sens1, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.L_0_sens1 <- rep(NA, nrow(sup0))
DG.L_1_sens1 <- rep(NA, nrow(sup0))
diff_DG.L_sens1 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.L_0_sens1[i] <- mean(sup0[i, ])
  DG.L_1_sens1[i] <- mean(sup1[i, ])
  diff_DG.L_sens1[i] <- DG.L_1_sens1[i] - DG.L_0_sens1[i]
}

plot(density(diff_DG.L_sens1), main = "Local DG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens1)
quantile(diff_DG.L_sens1, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens1, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens1 <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                        AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 60039)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S_sens1, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.S_sens1, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.S_0_sens1 <- rep(NA, nrow(sup0))
DG.S_1_sens1 <- rep(NA, nrow(sup0))
diff_DG.S_sens1 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.S_0_sens1[i] <- mean(sup0[i, ])
  DG.S_1_sens1[i] <- mean(sup1[i, ])
  diff_DG.S_sens1[i] <- DG.S_1_sens1[i] - DG.S_0_sens1[i]
}

plot(density(diff_DG.S_sens1), main = "Self DG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens1)
quantile(diff_DG.S_sens1, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens1, c(0.025, 0.5, 0.975))

# Point estimate is now positive, but no real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))


#################################################################
#### Sensitivity analysis 2: Inclusion of Pachamama ritual participation

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens2 <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         LGPERFHO +
                         AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                         INGFIRST + EXUSED1,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11102)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
sup0 <- predict(mod_RAG.L_sens2, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
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

plot(density(diff_RAG.L_sens2), main = "Local RAG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens2)
quantile(diff_RAG.L_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens2 <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         LGPERFHO +
                         AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                         INGFIRST + EXUSED2,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 51554)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
sup0 <- predict(mod_RAG.S_sens2, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
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

plot(density(diff_RAG.S_sens2), main = "Self RAG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens2)
quantile(diff_RAG.S_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens2 <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        LGPERFHO +
                        AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 89999)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L_sens2, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L_sens2, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.L_0_sens2 <- rep(NA, nrow(sup0))
DG.L_1_sens2 <- rep(NA, nrow(sup0))
diff_DG.L_sens2 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.L_0_sens2[i] <- mean(sup0[i, ])
  DG.L_1_sens2[i] <- mean(sup1[i, ])
  diff_DG.L_sens2[i] <- DG.L_1_sens2[i] - DG.L_0_sens2[i]
}

plot(density(diff_DG.L_sens2), main = "Local DG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens2)
quantile(diff_DG.L_sens2, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens2 <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        LGPERFHO +
                        AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 65024)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S_sens2, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.S_sens2, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.S_0_sens2 <- rep(NA, nrow(sup0))
DG.S_1_sens2 <- rep(NA, nrow(sup0))
diff_DG.S_sens2 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.S_0_sens2[i] <- mean(sup0[i, ])
  DG.S_1_sens2[i] <- mean(sup1[i, ])
  diff_DG.S_sens2[i] <- DG.S_1_sens2[i] - DG.S_0_sens2[i]
}

plot(density(diff_DG.S_sens2), main = "Self DG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens2)
quantile(diff_DG.S_sens2, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens2, c(0.025, 0.5, 0.975))

# Point estimate is now positive, but no real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))



#################################################################
#### Sensitivity analysis 3: Inclusion of Catholic beliefs/rituals and Pachamama ritual participation

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens3 <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                         AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                         INGFIRST + EXUSED1,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 11439)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens3)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
sup0 <- predict(mod_RAG.L_sens3, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED1 = dat_cc$EXUSED1))
sup1 <- predict(mod_RAG.L_sens3, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.L_0_sens3 <- rep(NA, nrow(sup0))
RAG.L_1_sens3 <- rep(NA, nrow(sup0))
diff_RAG.L_sens3 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.L_0_sens3[i] <- mean(sup0[i, ])
  RAG.L_1_sens3[i] <- mean(sup1[i, ])
  diff_RAG.L_sens3[i] <- RAG.L_1_sens3[i] - RAG.L_0_sens3[i]
}

plot(density(diff_RAG.L_sens3), main = "Local RAG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens3)
quantile(diff_RAG.L_sens3, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens3, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens3, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens3 <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                         AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                         INGFIRST + EXUSED2,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 62691)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens3)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
sup0 <- predict(mod_RAG.S_sens3, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST = dat_cc$INGFIRST, EXUSED2 = dat_cc$EXUSED2))
sup1 <- predict(mod_RAG.S_sens3, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.S_0_sens3 <- rep(NA, nrow(sup0))
RAG.S_1_sens3 <- rep(NA, nrow(sup0))
diff_RAG.S_sens3 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.S_0_sens3[i] <- mean(sup0[i, ])
  RAG.S_1_sens3[i] <- mean(sup1[i, ])
  diff_RAG.S_sens3[i] <- RAG.S_1_sens3[i] - RAG.S_0_sens3[i]
}

plot(density(diff_RAG.S_sens3), main = "Self RAG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens3)
quantile(diff_RAG.S_sens3, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens3, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens3, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens3 <- brm(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                        AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 32620)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_sens3)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L_sens3, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.L_sens3, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.L_0_sens3 <- rep(NA, nrow(sup0))
DG.L_1_sens3 <- rep(NA, nrow(sup0))
diff_DG.L_sens3 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.L_0_sens3[i] <- mean(sup0[i, ])
  DG.L_1_sens3[i] <- mean(sup1[i, ])
  diff_DG.L_sens3[i] <- DG.L_1_sens3[i] - DG.L_0_sens3[i]
}

plot(density(diff_DG.L_sens3), main = "Local DG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens3)
quantile(diff_DG.L_sens3, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens3, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens3, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens3 <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                        AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 95626)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_sens3)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S_sens3, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               BGPERFHO = dat_cc$BGPERFHO, LGPERFHO = dat_cc$LGPERFHO,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               MARKET1 = dat_cc$MARKET1, MARKET2 = dat_cc$MARKET2,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup1 <- predict(mod_DG.S_sens3, newdata = df_sup1, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup1_temp <- sup1
for(i in 1:length(attr(sup1, "levels"))) {
  sup1[sup1 == i] <- as.numeric(attr(sup1, "levels")[i])
}
sup1 <- sup1_temp

## Difference between counter-factual states from each of the posterior samples
DG.S_0_sens3 <- rep(NA, nrow(sup0))
DG.S_1_sens3 <- rep(NA, nrow(sup0))
diff_DG.S_sens3 <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  DG.S_0_sens3[i] <- mean(sup0[i, ])
  DG.S_1_sens3[i] <- mean(sup1[i, ])
  diff_DG.S_sens3[i] <- DG.S_1_sens3[i] - DG.S_0_sens3[i]
}

plot(density(diff_DG.S_sens3), main = "Self DG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens3)
quantile(diff_DG.S_sens3, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens3, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens3, c(0.025, 0.5, 0.975))

# Point estimate is now positive, but no real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))



###########################################################################################
### Multiple imputation analyses

str(kichwa_main)
summary(kichwa_main)

## Summary of missing data
cbind(n_miss = sapply(kichwa_main, function(x) sum(is.na(x))),
      per_miss = sapply(kichwa_main, function(x) round(sum(is.na(x)) / nrow(kichwa_main) * 100, 2)))

# Code binary and categorical variables as factors (for MICE to work with logistic regression)
kichwa_main$SEX <- as.factor(kichwa_main$SEX)
kichwa_main$INGFIRST <- as.factor(kichwa_main$INGFIRST)
kichwa_main$INGFIRST_DG <- as.factor(kichwa_main$INGFIRST_DG)
kichwa_main$LGPUNISH <- as.factor(kichwa_main$LGPUNISH)
kichwa_main$LGDIE <- as.factor(kichwa_main$LGDIE)
kichwa_main$LGFEEL <- as.factor(kichwa_main$LGFEEL)
kichwa_main$LGSEE <- as.factor(kichwa_main$LGSEE)
kichwa_main$BGPUNISH <- as.factor(kichwa_main$BGPUNISH)
kichwa_main$BGDIE <- as.factor(kichwa_main$BGDIE)
kichwa_main$BGFEEL <- as.factor(kichwa_main$BGFEEL)
kichwa_main$BGSEE <- as.factor(kichwa_main$BGSEE)
kichwa_main$LGPERFHO <- as.factor(kichwa_main$LGPERFHO)
kichwa_main$LGBLV <- as.factor(kichwa_main$LGBLV)
kichwa_main$LGTHINK <- as.factor(kichwa_main$LGTHINK)
kichwa_main$BGPERFHO <- as.factor(kichwa_main$BGPERFHO)
kichwa_main$BGBLV <- as.factor(kichwa_main$BGBLV)
kichwa_main$BGTHINK <- as.factor(kichwa_main$BGTHINK)

str(kichwa_main)
summary(kichwa_main)

# Drop BG believe, as no variation
kichwa_main$BGBLV <- NULL


## Set up imputations - Change mini-scales from PMM to passively impute (as is just based on means of two prior variables), and make scales ordinal model rather than multinomial
meth <- make.method(kichwa_main)
meth["LGPUN_AVE"] <- "~ I(((as.numeric(LGPUNISH) - 1) + (as.numeric(LGDIE) - 1)) / 2)"
meth["LGOMNI_AVE"] <- "~ I(((as.numeric(LGFEEL) - 1) + (as.numeric(LGSEE) - 1)) / 2)"
meth["BGPUN_AVE"] <- "~ I(((as.numeric(BGPUNISH) - 1) + (as.numeric(BGDIE) - 1)) / 2)"
#meth["BGOMNI_AVE"] <- "~ I(((as.numeric(BGFEEL) - 1) + (as.numeric(BGSEE) - 1)) / 2)"
meth["LGTHINK"] <- "polr"
meth["BGPERFHO"] <- "polr"
meth

# Predictor matrix - Exclude punishment and omniscience scores as predictors, and ID
pred <- make.predictorMatrix(kichwa_main)
pred[, "LGPUN_AVE"] <- 0
pred[, "LGOMNI_AVE"] <- 0
pred[, "BGPUN_AVE"] <- 0
pred[, "BGOMNI_AVE"] <- 0
pred[, "CERCID"] <- 0
pred["CERCID", ] <- 0
pred

# Visit sequence - Make sure passively-imputed scales are after the individual variables (they are)
visit <- make.visitSequence(kichwa_main)
visit

# Test imputation to check formulas and that no obvious errors
test <- mice(kichwa_main, m = 5, maxit = 0, method = meth, predictorMatrix = pred, visitSequence = visit)
test$formulas

## Run imputations - 20 imputations as approx. this amount of missing data (and to avoid having too many datasets, which slows down computation when running Bayesian models on these data)
imp <- mice(kichwa_main, m = 20, maxit = 10, 
            method = meth, predictorMatrix = pred, visitSequence = visit,
            seed = 98998, print = TRUE)

## Check imputations worked correctly
imp1 <- complete(imp, 1)

head(kichwa_main, n = 20L)
head(imp1, n = 20L)


## Save these imputed datasets, to save time if need them later
save(imp, file = "kichwa_imp.RData")
#load("kichwa_imp.RData")


#### Descriptive statistics for variables with missing data
temp_imp <- complete(imp, "long")

## Education
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(FORMALED), iqr25 = quantile(FORMALED, 0.25), iqr75 = quantile(FORMALED, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## Market integration 1
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(MARKET1), iqr25 = quantile(MARKET1, 0.25), iqr75 = quantile(MARKET1, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## Market integration 2
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(MARKET2), iqr25 = quantile(MARKET2, 0.25), iqr75 = quantile(MARKET2, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## LGPUNISH
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGPUNISH == "1"), per = N / nrow(kichwa_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGDIE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGDIE == "1"), per = N / nrow(kichwa_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGFEEL
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGFEEL == "1"), per = N / nrow(kichwa_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGSEE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGSEE == "1"), per = N / nrow(kichwa_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGPUNISH
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGPUNISH == "1"), per = N / nrow(kichwa_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGDIE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGDIE == "1"), per = N / nrow(kichwa_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LG punishment mini-scale
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(LGPUN_AVE)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean))

## LG omniscience mini-scale
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(LGOMNI_AVE)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean))

## MG punishment mini-scale
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(BGPUN_AVE)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean))

## RAG local
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(COREL.L), sd = sd(COREL.L), median = median(COREL.L), iqr25 = quantile(COREL.L, 0.25), iqr75 = quantile(COREL.L, 0.75)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean), mean_sd = mean(sd), mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## RAG self
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(COREL.S), sd = sd(COREL.S), median = median(COREL.S), iqr25 = quantile(COREL.S, 0.25), iqr75 = quantile(COREL.S, 0.75)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean), mean_sd = mean(sd), mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## DG local
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(COREL.L_DG), sd = sd(COREL.L_DG), median = median(COREL.L_DG), iqr25 = quantile(COREL.L_DG, 0.25), iqr75 = quantile(COREL.L_DG, 0.75)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean), mean_sd = mean(sd), mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## DG self
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(COREL.S_DG), sd = sd(COREL.S_DG), median = median(COREL.S_DG), iqr25 = quantile(COREL.S_DG, 0.25), iqr75 = quantile(COREL.S_DG, 0.75)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean), mean_sd = mean(sd), mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))


## Correlations between religiosity variables
# Extract imputed datasets
imp_all <- complete(imp, action = "all")

# Set up lists to store results in
MGLG_pun <- rep(NA, length(imp_all))
MGLG_die <- rep(NA, length(imp_all))
LG_pundie <- rep(NA, length(imp_all))
MG_pundie <- rep(NA, length(imp_all))
MGLG_pun_scale <- rep(NA, length(imp_all))
MGLG_feel <- rep(NA, length(imp_all))
MGLG_see <- rep(NA, length(imp_all))
LG_feelsee <- rep(NA, length(imp_all))
MG_feelsee <- rep(NA, length(imp_all))
MGLG_omni_scale <- rep(NA, length(imp_all))
LG_punomni_scale <- rep(NA, length(imp_all))
MG_punomni_scale <- rep(NA, length(imp_all))

# Loop over each imputed dataset and run correlations
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith imputed dataset
  df_temp <- imp_all[[i]]
  
  # Run correlations on ith imputed dataset adn store results
  MGLG_pun[i] <- tetrachoric(cbind(df_temp$LGPUNISH, df_temp$BGPUNISH))$rho[1, 2]
  MGLG_die[i] <- tetrachoric(cbind(df_temp$LGDIE, df_temp$BGDIE))$rho[1, 2]
  LG_pundie[i] <- tetrachoric(cbind(df_temp$LGPUNISH, df_temp$LGDIE))$rho[1, 2]
  MG_pundie[i] <- tetrachoric(cbind(df_temp$BGPUNISH, df_temp$BGDIE))$rho[1, 2]
  MGLG_pun_scale[i] <- polychoric(cbind(as.factor(df_temp$LGPUN_AVE), as.factor(df_temp$BGPUN_AVE)))$rho[1, 2]
  MGLG_feel[i] <- tetrachoric(cbind(df_temp$LGFEEL, df_temp$BGFEEL))$rho[1, 2]
  MGLG_see[i] <- tetrachoric(cbind(df_temp$LGSEE, df_temp$BGSEE))$rho[1, 2]
  LG_feelsee[i] <- tetrachoric(cbind(df_temp$LGFEEL, df_temp$LGSEE))$rho[1, 2]
  MG_feelsee[i] <- tetrachoric(cbind(df_temp$BGFEEL, df_temp$BGSEE))$rho[1, 2]
  MGLG_omni_scale[i] <- polychoric(cbind(as.factor(df_temp$LGOMNI_AVE), as.factor(df_temp$BGOMNI_AVE)))$rho[1, 2]
  LG_punomni_scale[i] <- polychoric(cbind(as.factor(df_temp$LGPUN_AVE), as.factor(df_temp$LGOMNI_AVE)))$rho[1, 2]
  MG_punomni_scale[i] <- polychoric(cbind(as.factor(df_temp$BGPUN_AVE), as.factor(df_temp$BGOMNI_AVE)))$rho[1, 2]
  
}


## Get mean of correlations
mean(MGLG_pun)
mean(MGLG_die)
mean(LG_pundie)
mean(MG_pundie)
mean(MGLG_pun_scale)
mean(MGLG_feel)
mean(MGLG_see)
mean(LG_feelsee)
mean(MG_feelsee)
mean(MGLG_omni_scale)
mean(LG_punomni_scale)
mean(MG_punomni_scale)


#### Main regression analyses using causal model (with age, sex, education, number of children, material security and market integration as confounders, plus game order and RAG comphrehension checks as covariates) #####

## Using 'brm_multiple' to perform analyses on each imputed dataset and then pool results together (see https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html)

## Extract each imputed dataset and convert binary variables back to integer from factors
imp_all <- complete(imp, action = "all")

for (i in 1:length(imp_all)) {
  temp <- imp_all[[i]]
  temp$SEX <- as.integer(temp$SEX) - 1
  temp$INGFIRST <- as.integer(temp$INGFIRST) - 1
  temp$INGFIRST_DG <- as.integer(temp$INGFIRST_DG) - 1
  temp$LGPERFHO <- as.integer(temp$LGPERFHO) - 1
  temp$BGPERFHO <- as.integer(temp$BGPERFHO) - 1
  temp$COREL.L_DG <- factor(temp$COREL.L_DG, ordered = TRUE)
  temp$COREL.S_DG <- factor(temp$COREL.S_DG, ordered = TRUE)
  
  # Replace old imputed data with new one
  imp_all[[i]] <- temp
}

str(imp_all)


### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                            INGFIRST + EXUSED1,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 82894,
                          combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L, check_data = FALSE)

## To note: r-hat and effective sample size values may indicate poor fitting and convergence issues, but this is because different imputed datasets have different values, so may not give exactly the same results - This is completely normal (and to be expected with MI!), so is likely a false positive. Still, worth checking that each individual chain converged, just to make sure
summary(mod_RAG.L[[1]])
summary(mod_RAG.L[[2]])


## Model with interaction between exposures
mod_RAG.L_int <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                                AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                INGFIRST + EXUSED1,
                              family = "binomial",
                              data = imp_all,
                              prior = c(prior(normal(0, 1), class = b),
                                        prior(normal(0, 2), class = Intercept)),
                              iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 66190,
                              combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L_int, check_data = FALSE)


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
  mod_RAG.L[[i]] <- add_criterion(mod_RAG.L[[i]], "loo")
  loo_imp <- loo(mod_RAG.L[[i]])
  LOO_base[i] <- loo_imp$estimates["elpd_loo", "Estimate"]
  
  # LOO for interaction model
  mod_RAG.L_int[[i]] <- add_criterion(mod_RAG.L_int[[i]], "loo")
  loo_imp_int <- loo(mod_RAG.L_int[[i]])
  LOO_int[i] <- loo_imp_int$estimates["elpd_loo", "Estimate"]
  
  # LOO diff and SE
  loo_comp <- loo_compare(loo_imp, loo_imp_int)
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


## G-computation (based on additive model)

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.L[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.L_0 <- rep(NA, nrow(sup0_all))
RAG.L_1 <- rep(NA, nrow(sup0_all))
diff_RAG.L <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.L_0[i] <- mean(sup0_all[i, ])
  RAG.L_1[i] <- mean(sup1_all[i, ])
  diff_RAG.L[i] <- RAG.L_1[i] - RAG.L_0[i]
}

plot(density(diff_RAG.L), main = "Local RAG (additive model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L)
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))

quantile(RAG.L_0, c(0.025, 0.5, 0.975))
quantile(RAG.L_1, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF

## Additive model
mod_RAG.S <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                            INGFIRST + EXUSED2,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 39897,
                          combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S, check_data = FALSE)


## Model with interaction between exposures
mod_RAG.S_int <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                                AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                INGFIRST + EXUSED2,
                              family = "binomial",
                              data = imp_all,
                              prior = c(prior(normal(0, 1), class = b),
                                        prior(normal(0, 2), class = Intercept)),
                              iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 77005,
                              combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S_int, check_data = FALSE)


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
  mod_RAG.S[[i]] <- add_criterion(mod_RAG.S[[i]], "loo")
  loo_imp <- loo(mod_RAG.S[[i]])
  LOO_base[i] <- loo_imp$estimates["elpd_loo", "Estimate"]
  
  # LOO for interaction model
  mod_RAG.S_int[[i]] <- add_criterion(mod_RAG.S_int[[i]], "loo")
  loo_imp_int <- loo(mod_RAG.S_int[[i]])
  LOO_int[i] <- loo_imp_int$estimates["elpd_loo", "Estimate"]
  
  # LOO diff and SE
  loo_comp <- loo_compare(loo_imp, loo_imp_int)
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


## G-computation (based on additive model)

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.S[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.S[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.S_0 <- rep(NA, nrow(sup0_all))
RAG.S_1 <- rep(NA, nrow(sup0_all))
diff_RAG.S <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.S_0[i] <- mean(sup0_all[i, ])
  RAG.S_1[i] <- mean(sup1_all[i, ])
  diff_RAG.S[i] <- RAG.S_1[i] - RAG.S_0[i]
}

plot(density(diff_RAG.S), main = "Self RAG (additive model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S)
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))

quantile(RAG.S_0, c(0.025, 0.5, 0.975))
quantile(RAG.S_1, c(0.025, 0.5, 0.975))


## Density plots of RAG results
pdf(file = "kichwa_RAG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.4), xlim = c(-5, 4),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.4), xlim = c(-5, 4),
     main = "SELF RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_RAG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


### DG - DISTANT vs LOCAL

## Additive model
mod_DG.L <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                           AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                           INGFIRST_DG,
                         family = cumulative("logit"),
                         data = imp_all,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0, 2), class = Intercept)),
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 98594,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L, check_data = FALSE)


## Model with interaction between exposures
mod_DG.L_int <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                               AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                               INGFIRST_DG,
                             family = cumulative("logit"),
                             data = imp_all,
                             prior = c(prior(normal(0, 1), class = b),
                                       prior(normal(0, 2), class = Intercept)),
                             iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 88713,
                             combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L_int, check_data = FALSE)


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
  mod_DG.L[[i]] <- add_criterion(mod_DG.L[[i]], "loo")
  loo_imp <- loo(mod_DG.L[[i]])
  LOO_base[i] <- loo_imp$estimates["elpd_loo", "Estimate"]
  
  # LOO for interaction model
  mod_DG.L_int[[i]] <- add_criterion(mod_DG.L_int[[i]], "loo")
  loo_imp_int <- loo(mod_DG.L_int[[i]])
  LOO_int[i] <- loo_imp_int$estimates["elpd_loo", "Estimate"]
  
  # LOO diff and SE
  loo_comp <- loo_compare(loo_imp, loo_imp_int)
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


## G-computation (based on additive model)

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.L[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.L[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.L_0 <- rep(NA, nrow(sup0_all))
DG.L_1 <- rep(NA, nrow(sup0_all))
diff_DG.L <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.L_0[i] <- mean(sup0_all[i, ])
  DG.L_1[i] <- mean(sup1_all[i, ])
  diff_DG.L[i] <- DG.L_1[i] - DG.L_0[i]
}

plot(density(diff_DG.L), main = "Local DG (additive model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L)
quantile(diff_DG.L, c(0.025, 0.5, 0.975))

quantile(DG.L_0, c(0.025, 0.5, 0.975))
quantile(DG.L_1, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF

## Additive model
mod_DG.S <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                           AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                           INGFIRST_DG,
                         family = cumulative("logit"),
                         data = imp_all,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0, 2), class = Intercept)),
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 56733,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S, check_data = FALSE)


## Model with interaction between exposures
mod_DG.S_int <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                               AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                               INGFIRST_DG,
                             family = cumulative("logit"),
                             data = imp_all,
                             prior = c(prior(normal(0, 1), class = b),
                                       prior(normal(0, 2), class = Intercept)),
                             iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 82537,
                             combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S_int, check_data = FALSE)


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
  mod_DG.S[[i]] <- add_criterion(mod_DG.S[[i]], "loo")
  loo_imp <- loo(mod_DG.S[[i]])
  LOO_base[i] <- loo_imp$estimates["elpd_loo", "Estimate"]
  
  # LOO for interaction model
  mod_DG.S_int[[i]] <- add_criterion(mod_DG.S_int[[i]], "loo")
  loo_imp_int <- loo(mod_DG.S_int[[i]])
  LOO_int[i] <- loo_imp_int$estimates["elpd_loo", "Estimate"]
  
  # LOO diff and SE
  loo_comp <- loo_compare(loo_imp, loo_imp_int)
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


## G-computation (based on additive model)

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.S[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.S[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.S_0 <- rep(NA, nrow(sup0_all))
DG.S_1 <- rep(NA, nrow(sup0_all))
diff_DG.S <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.S_0[i] <- mean(sup0_all[i, ])
  DG.S_1[i] <- mean(sup1_all[i, ])
  diff_DG.S[i] <- DG.S_1[i] - DG.S_0[i]
}

plot(density(diff_DG.S), main = "Self DG (additive model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S)
quantile(diff_DG.S, c(0.025, 0.5, 0.975))

quantile(DG.S_0, c(0.025, 0.5, 0.975))
quantile(DG.S_1, c(0.025, 0.5, 0.975))


## Density plots of DG results
pdf(file = "kichwa_DG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 0.65), xlim = c(-3, 3),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 0.65), xlim = c(-3, 3),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()



#################################################################
#### Sensitivity analysis 1: Inclusion of moralistic god (Catholic god) belief and practice terms in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here

### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                  INGFIRST + EXUSED1,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 21814,
                                combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L_sens1, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L_sens1[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.L_sens1[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.L_0_sens1 <- rep(NA, nrow(sup0_all))
RAG.L_1_sens1 <- rep(NA, nrow(sup0_all))
diff_RAG.L_sens1 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.L_0_sens1[i] <- mean(sup0_all[i, ])
  RAG.L_1_sens1[i] <- mean(sup1_all[i, ])
  diff_RAG.L_sens1[i] <- RAG.L_1_sens1[i] - RAG.L_0_sens1[i]
}

plot(density(diff_RAG.L_sens1), main = "Local RAG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens1)
quantile(diff_RAG.L_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                  INGFIRST + EXUSED2,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 95049,
                                combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S_sens1, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.S_sens1[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.S_sens1[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.S_0_sens1 <- rep(NA, nrow(sup0_all))
RAG.S_1_sens1 <- rep(NA, nrow(sup0_all))
diff_RAG.S_sens1 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.S_0_sens1[i] <- mean(sup0_all[i, ])
  RAG.S_1_sens1[i] <- mean(sup1_all[i, ])
  diff_RAG.S_sens1[i] <- RAG.S_1_sens1[i] - RAG.S_0_sens1[i]
}

plot(density(diff_RAG.S_sens1), main = "Self RAG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens1)
quantile(diff_RAG.S_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens1 <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 23059,
                               combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L_sens1, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.L_sens1[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.L_sens1[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.L_0_sens1 <- rep(NA, nrow(sup0_all))
DG.L_1_sens1 <- rep(NA, nrow(sup0_all))
diff_DG.L_sens1 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.L_0_sens1[i] <- mean(sup0_all[i, ])
  DG.L_1_sens1[i] <- mean(sup1_all[i, ])
  diff_DG.L_sens1[i] <- DG.L_1_sens1[i] - DG.L_0_sens1[i]
}

plot(density(diff_DG.L_sens1), main = "Local DG (Catholic)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens1)
quantile(diff_DG.L_sens1, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens1 <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 BGPUN_AVE + BGOMNI_AVE + BGPERFHO +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 25288,
                               combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S_sens1, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.S_sens1[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.S_sens1[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.S_0_sens1 <- rep(NA, nrow(sup0_all))
DG.S_1_sens1 <- rep(NA, nrow(sup0_all))
diff_DG.S_sens1 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.S_0_sens1[i] <- mean(sup0_all[i, ])
  DG.S_1_sens1[i] <- mean(sup1_all[i, ])
  diff_DG.S_sens1[i] <- DG.S_1_sens1[i] - DG.S_0_sens1[i]
}

plot(density(diff_DG.S_sens1), main = "Self DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens1)
quantile(diff_DG.S_sens1, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.S, c(0.025, 0.5, 0.975))


#################################################################
#### Sensitivity analysis 2: Inclusion of Pachamama ritual practice term in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here

### RAG - DISTANT vs LOCAL
mod_RAG.L_sens2 <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  LGPERFHO +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                  INGFIRST + EXUSED1,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 68753,
                                combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L_sens2, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L_sens2[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.L_sens2[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.L_0_sens2 <- rep(NA, nrow(sup0_all))
RAG.L_1_sens2 <- rep(NA, nrow(sup0_all))
diff_RAG.L_sens2 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.L_0_sens2[i] <- mean(sup0_all[i, ])
  RAG.L_1_sens2[i] <- mean(sup1_all[i, ])
  diff_RAG.L_sens2[i] <- RAG.L_1_sens2[i] - RAG.L_0_sens2[i]
}

plot(density(diff_RAG.L_sens2), main = "Local RAG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens2)
quantile(diff_RAG.L_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens2 <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  LGPERFHO +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                  INGFIRST + EXUSED2,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 4693,
                                combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S_sens2, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.S_sens2[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.S_sens2[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.S_0_sens2 <- rep(NA, nrow(sup0_all))
RAG.S_1_sens2 <- rep(NA, nrow(sup0_all))
diff_RAG.S_sens2 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.S_0_sens2[i] <- mean(sup0_all[i, ])
  RAG.S_1_sens2[i] <- mean(sup1_all[i, ])
  diff_RAG.S_sens2[i] <- RAG.S_1_sens2[i] - RAG.S_0_sens2[i]
}

plot(density(diff_RAG.S_sens2), main = "Self RAG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens2)
quantile(diff_RAG.S_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens2 <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 LGPERFHO +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 85492,
                               combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L_sens2, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.L_sens2[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.L_sens2[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.L_0_sens2 <- rep(NA, nrow(sup0_all))
DG.L_1_sens2 <- rep(NA, nrow(sup0_all))
diff_DG.L_sens2 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.L_0_sens2[i] <- mean(sup0_all[i, ])
  DG.L_1_sens2[i] <- mean(sup1_all[i, ])
  diff_DG.L_sens2[i] <- DG.L_1_sens2[i] - DG.L_0_sens2[i]
}

plot(density(diff_DG.L_sens2), main = "Local DG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens2)
quantile(diff_DG.L_sens2, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens2 <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 LGPERFHO +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 4897,
                               combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S_sens2, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.S_sens2[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.S_sens2[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.S_0_sens2 <- rep(NA, nrow(sup0_all))
DG.S_1_sens2 <- rep(NA, nrow(sup0_all))
diff_DG.S_sens2 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.S_0_sens2[i] <- mean(sup0_all[i, ])
  DG.S_1_sens2[i] <- mean(sup1_all[i, ])
  diff_DG.S_sens2[i] <- DG.S_1_sens2[i] - DG.S_0_sens2[i]
}

plot(density(diff_DG.S_sens2), main = "Self DG (Pachamama rituals)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens2)
quantile(diff_DG.S_sens2, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.S, c(0.025, 0.5, 0.975))



############################################################################
#### Sensitivity analysis 3: Inclusion of moralistic god (Catholic god) belief and practice terms and Pachamama rituals in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here

### RAG - DISTANT vs LOCAL
mod_RAG.L_sens3 <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                  INGFIRST + EXUSED1,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 6359,
                                combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L_sens3, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L_sens3[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.L_sens3[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.L_0_sens3 <- rep(NA, nrow(sup0_all))
RAG.L_1_sens3 <- rep(NA, nrow(sup0_all))
diff_RAG.L_sens3 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.L_0_sens3[i] <- mean(sup0_all[i, ])
  RAG.L_1_sens3[i] <- mean(sup1_all[i, ])
  diff_RAG.L_sens3[i] <- RAG.L_1_sens3[i] - RAG.L_0_sens3[i]
}

plot(density(diff_RAG.L_sens3), main = "Local RAG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens3)
quantile(diff_RAG.L_sens3, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens3, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens3, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens3 <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                  INGFIRST + EXUSED2,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 40478,
                                combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S_sens3, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.S_sens3[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.S_sens3[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.S_0_sens3 <- rep(NA, nrow(sup0_all))
RAG.S_1_sens3 <- rep(NA, nrow(sup0_all))
diff_RAG.S_sens3 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.S_0_sens3[i] <- mean(sup0_all[i, ])
  RAG.S_1_sens3[i] <- mean(sup1_all[i, ])
  diff_RAG.S_sens3[i] <- RAG.S_1_sens3[i] - RAG.S_0_sens3[i]
}

plot(density(diff_RAG.S_sens3), main = "Self RAG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens3)
quantile(diff_RAG.S_sens3, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens3, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens3, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens3 <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 78278,
                               combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L_sens3, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.L_sens3[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.L_sens3[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.L_0_sens3 <- rep(NA, nrow(sup0_all))
DG.L_1_sens3 <- rep(NA, nrow(sup0_all))
diff_DG.L_sens3 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.L_0_sens3[i] <- mean(sup0_all[i, ])
  DG.L_1_sens3[i] <- mean(sup1_all[i, ])
  diff_DG.L_sens3[i] <- DG.L_1_sens3[i] - DG.L_0_sens3[i]
}

plot(density(diff_DG.L_sens3), main = "Local DG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens3)
quantile(diff_DG.L_sens3, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens3, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens3, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens3 <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 BGPUN_AVE + BGOMNI_AVE + BGPERFHO + LGPERFHO +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN + MARKET1 + MARKET2 +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 64932,
                               combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S_sens3, check_data = FALSE)


## G-computation

### Will have to loop over each imputed dataset and store contrasts, then combine together afterwards

# Set up lists to store results in
sup0 <- list()
sup1 <- list()

# Predict outcome where all supernatural belief variables are '0' and '1' for each imputed dataset
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith dataset
  temp <- imp_all[[i]]
  
  ## Exposures at 0
  temp$LGPUN_AVE <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_DG.S_sens3[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUN_AVE <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_DG.S_sens3[[i]], newdata = temp, summary = FALSE)
  
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

# Difference between counter-factual states from each of the posterior samples
DG.S_0_sens3 <- rep(NA, nrow(sup0_all))
DG.S_1_sens3 <- rep(NA, nrow(sup0_all))
diff_DG.S_sens3 <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  DG.S_0_sens3[i] <- mean(sup0_all[i, ])
  DG.S_1_sens3[i] <- mean(sup1_all[i, ])
  diff_DG.S_sens3[i] <- DG.S_1_sens3[i] - DG.S_0_sens3[i]
}

plot(density(diff_DG.S_sens3), main = "Self DG (Cath and Pach)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens3)
quantile(diff_DG.S_sens3, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens3, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens3, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.S, c(0.025, 0.5, 0.975))


