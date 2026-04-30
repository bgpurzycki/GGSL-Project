### Objective 1 Analysis Code - Sihanaka
### Created 2/3/2026 by Dan Major-Smith
### R 4.4.1

####################################################################
#### Clear workspace, set working directory and install/load packages
rm(list=ls())
Sys.setenv(LANG = "en")

setwd("")


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

#install.packages("posterior")
library(posterior)

#install.packages("devtools")
#library("devtools")
#install_github('alastair-JL/AnthroTools')
library(AnthroTools)

#install.packages("ordbetareg")
library(ordbetareg)

#install.packages("ggdist")
library(ggdist)


#####################################################################
### Read in and process Sihanaka data (note that this step processes the raw data which is not openly-available. To follow these analyses, read in the data on line 280)

## Demographics first
sihanaka_demo <- read_excel("./DEMO Results_v2_DMS.xlsx",
                          sheet = "Demographics")

head(sihanaka_demo)
glimpse(sihanaka_demo)

# Remove some empty variables, unused IDs and empty rows
sihanaka_demo <- sihanaka_demo %>%
  filter(!is.na(PARTID)) %>%
  select(-NAME, -DATE, -TIME, -SEXRA, -SESSION)

# Replace any 'EXO' with 'EX0' (from letter 'o' to zero)
sihanaka_demo <- sihanaka_demo %>%
  mutate(PARTID = gsub("EXO", "EX0", PARTID))

# Check data against ERM and make sure named and coded the same
sihanaka_demo <- sihanaka_demo %>%
  rename(CERCID = PARTID, SITE_SPEC = SITE) %>%
  mutate(SITE = "Sihanaka") %>%
  mutate(RESEARCHER = "Norge") %>%
  relocate(CERCID, RESEARCHER, SITE, SITE_SPEC, .before = LOCATION)

# Go through variables, check sensible, and convert to numeric
table(sihanaka_demo$SEX)
table(sihanaka_demo$AGE)
table(sihanaka_demo$FAMILY)
table(sihanaka_demo$CHILDREN)
table(sihanaka_demo$FORMALED)
table(sihanaka_demo$MAT1)
table(sihanaka_demo$MAT1C)
table(sihanaka_demo$MAT2)
table(sihanaka_demo$MAT2C)
table(sihanaka_demo$MAT3)
table(sihanaka_demo$MAT3C)
table(sihanaka_demo$MAT4)
table(sihanaka_demo$MAT4C)

sihanaka_demo <- sihanaka_demo %>%
  mutate(SEX = as.numeric(SEX)) %>%
  mutate(AGE = as.numeric(AGE)) %>%
  mutate(AGE = ifelse(is.na(AGE) | AGE < 17, NA, AGE)) %>%
  mutate(FAMILY = ifelse(FAMILY == "1 and 5", "5", FAMILY)) %>%
  mutate(FAMILY = as.numeric(FAMILY)) %>%
  mutate(CHILDREN = ifelse(str_length(CHILDREN) > 3, str_sub(CHILDREN, 1, 1), CHILDREN)) %>%
  mutate(CHILDREN = as.numeric(CHILDREN)) %>%
  mutate(FORMALED = as.numeric(FORMALED)) %>%
  mutate(MAT1 = as.numeric(MAT1)) %>%
  mutate(MAT1C = as.numeric(MAT1C)) %>%
  mutate(MAT2 = as.numeric(MAT2)) %>%
  mutate(MAT2C = as.numeric(MAT2C)) %>%
  mutate(MAT3 = as.numeric(MAT3)) %>%
  mutate(MAT3 = ifelse(is.na(MAT3) | MAT3 < 0, NA, MAT3)) %>%
  mutate(MAT3C = as.numeric(MAT3C)) %>%
  mutate(MAT4 = as.numeric(MAT4)) %>%
  mutate(MAT4 = ifelse(is.na(MAT4) | MAT4 < 0, NA, MAT4)) %>%
  mutate(MAT4C = as.numeric(MAT4C))


# Make the derived material security variables
sihanaka_demo <- sihanaka_demo %>%
  mutate(MMAT = rowMeans(select(., MAT1, MAT2, MAT3, MAT4), na.rm = TRUE)) %>%
  mutate(MMATc = rowMeans(select(., MAT1C, MAT2C, MAT3C, MAT4C), na.rm = TRUE))


## Also make a binary 'fisher' variable, of whether participant had 'fisher' as occupation or not
table(sihanaka_demo$OCCUP)

sihanaka_demo <- sihanaka_demo %>%
  mutate(FISHER = ifelse(str_detect(OCCUP, "fish"), 1, 0)) %>%
  mutate(FISHER = ifelse(OCCUP == "NA", NA, FISHER))

table(sihanaka_demo$FISHER)


## Read in the game data
sihanaka_game <- read_excel("./EXPERIMENTS Results_v2_DMS.xlsx",
                          sheet = "Data",
                          skip = 1)

head(sihanaka_game)
glimpse(sihanaka_game)

# Remove some empty variables, post-game text data, and unused IDs
sihanaka_game <- sihanaka_game %>%
  select(-c(SITE, DATE, TIME, LOCATION, SESSNUM, GA1, GA2)) %>%
  rename(CERCID = ID, INTERVIEWER_GAMES = INTERVIEWER) %>%
  relocate(CERCID, .before = INTERVIEWER_GAMES)

# Replace any 'EXO' with 'EX0' (from letter 'o' to zero)
sihanaka_game <- sihanaka_game %>%
  mutate(CERCID = gsub("EXO", "EX0", CERCID))

# Check the data and edit to match ERM names and coding
sihanaka_game <- sihanaka_game %>%
  rename(ORDER = ORDERRAG) %>%
  mutate(INGFIRST = ifelse(ORDER == 1, 1, 0)) %>%
  relocate(INGFIRST, .after = ORDER) %>%
  rename(PRE1_1 = '1P1', PRE1_2 = '1P2', PRE1_3 = '1P3', PRE1_4 = '1P4', PRE1_5 = '1P5',
         PRE2_1 = '2P1', PRE2_2 = '2P2', PRE2_3 = '2P3', PRE2_4 = '2P4', PRE2_5 = '2P5') %>%
  rename(COREL.L = DISTANT1, INGROUP = INGROUP...17, COREL.S = DISTANT2, SELF = SELF...26) %>%
  mutate(SUM1 = COREL.L + INGROUP) %>%
  mutate(SUM2 = COREL.S + SELF) %>%
  relocate(COREL.L, INGROUP, SUM1, .after = EXUSED1) %>%
  relocate(COREL.S, SELF, SUM2, .after = EXUSED2) %>%
  mutate(INGFIRST_DG = ifelse(ORDERDG == 1, 1, 0)) %>% 
  relocate(INGFIRST_DG, .after = ORDERDG) %>%
  rename(COREL.L_DG = DISTANT...30, INGROUP_DG = INGROUP...29, COREL.S_DG = DISTANT...32, SELF_DG = SELF...31) %>%
  mutate(SUM1_DG = COREL.L_DG + INGROUP_DG) %>%
  mutate(SUM2_DG = COREL.S_DG + SELF_DG) %>%
  relocate(COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG, .after = INGFIRST_DG)


# Check if any RAG totals do not sum to 30 - All good!
sihanaka_game %>%
  filter(SUM1 != 30 | SUM2 != 30) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L, INGROUP, SUM1, COREL.S, SELF, SUM2)

# Check if any DG totals do not sum to 10 - All good!
sihanaka_game %>%
  filter(SUM1_DG != 10 | SUM2_DG != 10) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG)



### Now for the religiosity interview data
sihanaka_relig <- read_excel("./RELIGIOSITY Results_v2_DMS.xlsx",
                           sheet = "RELIGIOSITY", skip = 1)

head(sihanaka_relig)
glimpse(sihanaka_relig)


# Remove some unnecessary, text, and/or empty variables
sihanaka_relig <- sihanaka_relig %>%
  select(-c(DATE, TIME, LOCATION, NAME, BGPUND, BGDIED, LGPUNISHD, LGDIE...52, CORELD, OUTGD)) %>%
  rename(CERCID = ID, INTERVIEWER_RELIG = INTERVIEWER, LGDIE = LGDIE...51)

# Replace any 'EXO' with 'EX0' (from letter 'o' to zero)
sihanaka_relig <- sihanaka_relig %>%
  mutate(CERCID = gsub("EXO", "EX0", CERCID))

# Check the data and edit to match ERM names and coding (focus on the relevant variables here, for now)
table(sihanaka_relig$BGPUNISH)
table(sihanaka_relig$BGDIE)
table(sihanaka_relig$BGFEEL)
table(sihanaka_relig$BGSEE)
table(sihanaka_relig$LGPUNISH)
table(sihanaka_relig$LGDIE)
table(sihanaka_relig$LGFEEL)
table(sihanaka_relig$LGSEE)

sihanaka_relig <- sihanaka_relig %>%
  mutate(BGPUNISH = as.numeric(BGPUNISH)) %>%
  mutate(BGDIE = as.numeric(BGDIE)) %>%
  mutate(BGFEEL = as.numeric(BGFEEL)) %>%
  mutate(BGSEE = as.numeric(BGSEE)) %>%
  mutate(LGPUNISH = as.numeric(LGPUNISH)) %>%
  mutate(LGDIE = as.numeric(LGDIE)) %>%
  mutate(LGFEEL = as.numeric(LGFEEL)) %>%
  mutate(LGSEE = as.numeric(LGSEE))


## Is some missing data on the MG and LG variables. Will explore this.

# For MG -  No obvious pattern to missingness
temp <- sihanaka_relig %>%
  filter(is.na(BGPUNISH) | is.na(BGDIE) | is.na(BGFEEL) | is.na(BGSEE)) %>%
  select(CERCID, BGBLV, BGPUNISH, BGFEEL, BGSEE, BGDIE)
print(temp, n = 30L)

# For LG -  Some relationship between NAs and not believing in LG, but not super-obvious
temp <- sihanaka_relig %>%
  filter(is.na(LGPUNISH) | is.na(LGDIE) | is.na(LGFEEL) | is.na(LGSEE)) %>%
  select(CERCID, LGBLV, LGPUNISH, LGFEEL, LGSEE, LGDIE)
print(temp, n = 40L)


## Prepare some auxilary variables which may predict missinginess in these variables (believe in deity, freq of performances towards deity, and freq thinks about deities)
table(sihanaka_relig$BGPERFHO)
table(sihanaka_relig$BGBLV)
table(sihanaka_relig$BGTHINK)
table(sihanaka_relig$LGPERFHO)
table(sihanaka_relig$LGBLV)
table(sihanaka_relig$LGTHINK)

sihanaka_relig <- sihanaka_relig %>%
  mutate(BGPERFHO = ifelse(is.na(BGPERFHO), "NA",
                           ifelse(BGPERFHO == "2 and 4" | BGPERFHO == "2.2999999999999998", 
                                  2, BGPERFHO))) %>%
  mutate(BGPERFHO = as.numeric(BGPERFHO)) %>%
  mutate(BGBLV = as.numeric(BGBLV)) %>%
  mutate(BGTHINK = ifelse(is.na(BGTHINK), "NA", 
                          ifelse(BGTHINK == "1(every two years)", 1, BGTHINK))) %>%
  mutate(BGTHINK = as.numeric(BGTHINK)) %>%
  mutate(LGPERFHO = ifelse(is.na(LGPERFHO), "NA",
                             ifelse(LGPERFHO == "2 and 4", 2, LGPERFHO))) %>%
  mutate(LGPERFHO = as.numeric(LGPERFHO)) %>%
  mutate(LGBLV = ifelse(is.na(LGBLV), "NA",
                             ifelse(LGBLV == "2", 1, LGBLV))) %>%
  mutate(LGBLV = as.numeric(LGBLV)) %>%
  mutate(LGTHINK = as.numeric(LGTHINK))


### Combine sihanaka demographics, game and religiosity data together
names(sihanaka_demo)
names(sihanaka_game)
names(sihanaka_relig)

sihanaka_main <- full_join(sihanaka_demo, sihanaka_game, by = "CERCID")
sihanaka_main <- full_join(sihanaka_main, sihanaka_relig, by = "CERCID")


## Keep just variables needed for analyses
sihanaka_main <- sihanaka_main %>%
  select(c(CERCID, SEX, AGE, CHILDREN, FORMALED, MMAT, FISHER,
           INGFIRST, EXUSED1, COREL.L, EXUSED2, COREL.S,
           INGFIRST_DG, COREL.L_DG, COREL.S_DG,
           LGPUNISH, LGDIE, LGFEEL, LGSEE,
           BGPUNISH, BGDIE, BGFEEL, BGSEE,
           LGPERFHO, LGBLV, LGTHINK,
           BGPERFHO, BGBLV, BGTHINK))


## Save this dataset (in both RData and CSV formats)
save(sihanaka_main, file = "Sihanaka_data.RData")
write_csv(sihanaka_main, file = "Sihanaka_data.csv")


################################################################################
#### Data analysis

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("Sihanaka_data.RData")

### Descriptive stats

## Summary of missing data
cbind(n_miss = sapply(sihanaka_main, function(x) sum(is.na(x))),
      per_miss = sapply(sihanaka_main, function(x) round(sum(is.na(x)) / nrow(sihanaka_main) * 100, 2)))

# 28 cases with complete data (35.4%)
sum(complete.cases(sihanaka_main))
round(sum(complete.cases(sihanaka_main)) / nrow(sihanaka_main) * 100, 1)


# 56 cases (70.9%) with complete data if exclude LGDIE, MG, auxiliary vars
temp <- sihanaka_main %>%
  select(-c(LGDIE, BGPUNISH, BGDIE, BGFEEL, BGSEE, LGPERFHO, LGBLV, LGTHINK, BGPERFHO, BGBLV, BGTHINK))

sum(complete.cases(temp))
round(sum(complete.cases(temp)) / nrow(temp) * 100, 1)


## Demographics

# Age (years)
summary(sihanaka_main$AGE)
sd(sihanaka_main$AGE, na.rm = TRUE)

plot(density(sihanaka_main$AGE, na.rm = TRUE), main = "", xlab = "Age")
hist(sihanaka_main$AGE, main = "", xlab = "Age")

# Sex (1 = male)
addmargins(table(sihanaka_main$SEX))
round(prop.table(table(sihanaka_main$SEX)) * 100, 1)

# Children
addmargins(table(sihanaka_main$CHILDREN))
round(prop.table(table(sihanaka_main$CHILDREN)) * 100, 1)

summary(sihanaka_main$CHILDREN)
sd(sihanaka_main$CHILDREN, na.rm = TRUE)

plot(density(sihanaka_main$CHILDREN, na.rm = TRUE), main = "", xlab = "Number of children")
hist(sihanaka_main$CHILDREN, main = "", xlab = "Number of children")

# Education
addmargins(table(sihanaka_main$FORMALED))
round(prop.table(table(sihanaka_main$FORMALED)) * 100, 1)

summary(sihanaka_main$FORMALED)
sd(sihanaka_main$FORMALED, na.rm = TRUE)

plot(density(sihanaka_main$FORMALED, na.rm = TRUE), main = "", xlab = "Number of years education")
hist(sihanaka_main$FORMALED, main = "", xlab = "Number of years education")

# Material insecurity
addmargins(table(sihanaka_main$MMAT))
round(prop.table(table(sihanaka_main$MMAT)) * 100, 1)

summary(sihanaka_main$MMAT)
sd(sihanaka_main$MMAT, na.rm = TRUE)

plot(density(sihanaka_main$MMAT, na.rm = TRUE), main = "", xlab = "Food insecurity")
hist(sihanaka_main$MMAT, main = "", xlab = "Food insecurity")

# Fisher (1 = fisher)
addmargins(table(sihanaka_main$FISHER))
round(prop.table(table(sihanaka_main$FISHER)) * 100, 1)


## Religiosity

# MG punish
addmargins(table(sihanaka_main$BGPUNISH))
round(prop.table(table(sihanaka_main$BGPUNISH)) * 100, 1)

# LG punish
addmargins(table(sihanaka_main$LGPUNISH))
round(prop.table(table(sihanaka_main$LGPUNISH)) * 100, 1)

# Tetrachoric correlation between MG and LG PUNISH
addmargins(table(sihanaka_main$LGPUNISH, sihanaka_main$BGPUNISH))
round(prop.table(table(sihanaka_main$LGPUNISH, sihanaka_main$BGPUNISH), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$LGPUNISH, sihanaka_main$BGPUNISH))


# MG afterlife
addmargins(table(sihanaka_main$BGDIE))
round(prop.table(table(sihanaka_main$BGDIE)) * 100, 1)

# LG afterlife
addmargins(table(sihanaka_main$LGDIE))
round(prop.table(table(sihanaka_main$LGDIE)) * 100, 1)

# Tetrachoric correlation between MG and LG DIE
addmargins(table(sihanaka_main$LGDIE, sihanaka_main$BGDIE))
round(prop.table(table(sihanaka_main$LGDIE, sihanaka_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$LGDIE, sihanaka_main$BGDIE))


# Tetrachoric correlation between PUNISH and DIE for both MG and LG
addmargins(table(sihanaka_main$LGPUNISH, sihanaka_main$LGDIE))
round(prop.table(table(sihanaka_main$LGPUNISH, sihanaka_main$LGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$LGPUNISH, sihanaka_main$LGDIE))

addmargins(table(sihanaka_main$BGPUNISH, sihanaka_main$BGDIE))
round(prop.table(table(sihanaka_main$BGPUNISH, sihanaka_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$BGPUNISH, sihanaka_main$BGDIE))


# MG and LG mini-punishment scales
sihanaka_main <- sihanaka_main %>%
  mutate(BGPUN_AVE = rowMeans(select(., BGPUNISH, BGDIE), na.rm = FALSE)) %>%
  mutate(LGPUN_AVE = rowMeans(select(., LGPUNISH, LGDIE), na.rm = FALSE))

# MG
addmargins(table(sihanaka_main$BGPUN_AVE))
round(prop.table(table(sihanaka_main$BGPUN_AVE)) * 100, 1)

summary(sihanaka_main$BGPUN_AVE)

# LG
addmargins(table(sihanaka_main$LGPUN_AVE))
round(prop.table(table(sihanaka_main$LGPUN_AVE)) * 100, 1)

summary(sihanaka_main$LGPUN_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(sihanaka_main$BGPUN_AVE)), main = "MG Punish")
barplot(prop.table(table(sihanaka_main$LGPUN_AVE)), main = "LG Punish")
dev.off()

# Correlation between punishment mini-scales
addmargins(table(sihanaka_main$LGPUN_AVE, sihanaka_main$BGPUN_AVE))
round(prop.table(table(sihanaka_main$LGPUN_AVE, sihanaka_main$BGPUN_AVE), margin = 1) * 100, 1)

cor.test(sihanaka_main$LGPUN_AVE, sihanaka_main$BGPUN_AVE)
polychoric(cbind(as.factor(sihanaka_main$LGPUN_AVE), as.factor(sihanaka_main$BGPUN_AVE)))


# MG feel
addmargins(table(sihanaka_main$BGFEEL))
round(prop.table(table(sihanaka_main$BGFEEL)) * 100, 1)

# LG feel
addmargins(table(sihanaka_main$LGFEEL))
round(prop.table(table(sihanaka_main$LGFEEL)) * 100, 1)

# Tetrachoric correlation between MG and LG FEEL
addmargins(table(sihanaka_main$LGFEEL, sihanaka_main$BGFEEL))
round(prop.table(table(sihanaka_main$LGFEEL, sihanaka_main$BGFEEL), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$LGFEEL, sihanaka_main$BGFEEL))


# MG see
addmargins(table(sihanaka_main$BGSEE))
round(prop.table(table(sihanaka_main$BGSEE)) * 100, 1)

# LG see
addmargins(table(sihanaka_main$LGSEE))
round(prop.table(table(sihanaka_main$LGSEE)) * 100, 1)

# Tetrachoric correlation between MG and LG SEE
addmargins(table(sihanaka_main$LGSEE, sihanaka_main$BGSEE))
round(prop.table(table(sihanaka_main$LGSEE, sihanaka_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$LGSEE, sihanaka_main$BGSEE))


# Tetrachoric correlation between FEEL and SEE for both MG and LG
addmargins(table(sihanaka_main$LGFEEL, sihanaka_main$LGSEE))
round(prop.table(table(sihanaka_main$LGFEEL, sihanaka_main$LGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$LGFEEL, sihanaka_main$LGSEE))

addmargins(table(sihanaka_main$BGFEEL, sihanaka_main$BGSEE))
round(prop.table(table(sihanaka_main$BGFEEL, sihanaka_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(sihanaka_main$BGFEEL, sihanaka_main$BGSEE))


# MG and LG mini-omniscience scales
sihanaka_main <- sihanaka_main %>%
  mutate(BGOMNI_AVE = rowMeans(select(., BGFEEL, BGSEE), na.rm = FALSE)) %>%
  mutate(LGOMNI_AVE = rowMeans(select(., LGFEEL, LGSEE), na.rm = FALSE))

# MG
addmargins(table(sihanaka_main$BGOMNI_AVE))
round(prop.table(table(sihanaka_main$BGOMNI_AVE)) * 100, 1)

summary(sihanaka_main$BGOMNI_AVE)

# LG
addmargins(table(sihanaka_main$LGOMNI_AVE))
round(prop.table(table(sihanaka_main$LGOMNI_AVE)) * 100, 1)

summary(sihanaka_main$LGOMNI_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(sihanaka_main$BGOMNI_AVE)), main = "MG omniscience")
barplot(prop.table(table(sihanaka_main$LGOMNI_AVE)), main = "LG omniscience")
dev.off()

# Correlation between omniscience mini-scales
addmargins(table(sihanaka_main$LGOMNI_AVE, sihanaka_main$BGOMNI_AVE))
round(prop.table(table(sihanaka_main$LGOMNI_AVE, sihanaka_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(sihanaka_main$LGOMNI_AVE, sihanaka_main$BGOMNI_AVE)
polychoric(cbind(as.factor(sihanaka_main$LGOMNI_AVE), as.factor(sihanaka_main$BGOMNI_AVE)))


# Correlation between punishment and omniscience mini-scales for both MG and LG
addmargins(table(sihanaka_main$LGPUN_AVE, sihanaka_main$LGOMNI_AVE))
round(prop.table(table(sihanaka_main$LGPUN_AVE, sihanaka_main$LGOMNI_AVE), margin = 1) * 100, 1)

cor.test(sihanaka_main$LGPUN_AVE, sihanaka_main$LGOMNI_AVE)
polychoric(cbind(as.factor(sihanaka_main$LGPUN_AVE), as.factor(sihanaka_main$LGOMNI_AVE)))

addmargins(table(sihanaka_main$BGPUN_AVE, sihanaka_main$BGOMNI_AVE))
round(prop.table(table(sihanaka_main$BGPUN_AVE, sihanaka_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(sihanaka_main$BGPUN_AVE, sihanaka_main$BGOMNI_AVE)
polychoric(cbind(as.factor(sihanaka_main$BGPUN_AVE), as.factor(sihanaka_main$BGOMNI_AVE)))


## Descriptives of auxiliary religiosity variables for imputation

# Believes in LG
addmargins(table(sihanaka_main$LGBLV))
round(prop.table(table(sihanaka_main$LGBLV)) * 100, 1)

# Freq thinks about LG
addmargins(table(sihanaka_main$LGTHINK))
round(prop.table(table(sihanaka_main$LGTHINK)) * 100, 1)

# Freq performs rituals towards LG
addmargins(table(sihanaka_main$LGPERFHO))
round(prop.table(table(sihanaka_main$LGPERFHO)) * 100, 1)

# Believes in MG
addmargins(table(sihanaka_main$BGBLV))
round(prop.table(table(sihanaka_main$BGBLV)) * 100, 1)

# Freq thinks about MG
addmargins(table(sihanaka_main$BGTHINK))
round(prop.table(table(sihanaka_main$BGTHINK)) * 100, 1)

# Freq performs rituals towards MG
addmargins(table(sihanaka_main$BGPERFHO))
round(prop.table(table(sihanaka_main$BGPERFHO)) * 100, 1)


### Religious Landscape Interview data 

## Cultural salience of deities
d1 <- read.delim("Madagascargods.txt")

# Gods list
d.s <- CalculateSalience(d1, Order = "ORDERG", CODE = "GODSPIRIT.C",
                         Subj = "ID", Salience = "godsal") 
godstab <- FreeListTable(d.s, Subj = "ID", Order = "ORDERG", 
                         CODE = "GODSPIRIT.C", Salience = "godsal", tableType = "MAX_SALIENCE")
model <- SalienceByCode(d.s, Subj = "ID", CODE = "GODSPIRIT.C", Salience = "godsal")
godsord <- SalienceOrdBeta(godstab, var_sel = "TOP", top = 8, seed = 182, IDs_first = TRUE)
res <- SalienceEstimateSummary(godsord, quantiles = c(0.025, 0.5, 0.975))
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = model, S_uncert = res, label = "gods", 
                    lower_int = "2.5%", upper_int = "97.5%")
SalienceEstimatePlot(godsord, order = "high-low")

# Save this plot
pdf(file = "GodsSalience.pdf", height = 10, width = 8)
SalienceEstimatePlot(godsord, order = "high-low")
dev.off()

## Gods' traits
d2 <- data.frame(read_excel("Sihanaka_RLI.xlsx",
                             sheet = "Ark1"))
d2 <- d2[,1:12] # gods data only

ndria <- d2[d2$GODSPIRIT == "Ndrianampanjaka (immortal king)",][,8:12]
ndria <- ndria %>% mutate_if(is.character, as.numeric)
colSums(ndria)
colSums(ndria)/nrow(ndria)

zazav <- d2[d2$GODSPIRIT == "Zazavavindrano (naiads, including the individual Volanoro)",][,8:12]
zazav <- zazav %>% mutate_if(is.character, as.numeric)
zazav <- zazav[-9, ]
colSums(zazav)
colSums(zazav)/nrow(zazav)


### Free-list data of what gods like/dislike (i.e., cultural salience of gods' concerns)
sihanaka_fl <- read_excel("Sihanaka_FreeList.xlsx")

head(sihanaka_fl)

## MG likes
table(sihanaka_fl$BGCARE_GEN_FINAL, useNA = "ifany")

# Keep just relevant variables and remove missing data
sihanaka_fl_mgl <- sihanaka_fl %>%
  select(c(PARTID, ORDER, BGCARE_GEN_FINAL)) %>%
  filter(!is.na(BGCARE_GEN_FINAL))

sihanaka_fl_mgl <- as.data.frame(sihanaka_fl_mgl)

# Calculate item salience
sihanaka_fl_mgl.s <- CalculateSalience(sihanaka_fl_mgl, Subj = "PARTID", Order = "ORDER",
                                       CODE = "BGCARE_GEN_FINAL", Salience = "BGCARE.S")
head(sihanaka_fl_mgl.s)

# Calculate Smith's S for each code
mgl.s <- SalienceByCode(sihanaka_fl_mgl.s, Subj = "PARTID", CODE = "BGCARE_GEN_FINAL", 
                        Salience = "BGCARE.S", dealWithDoubles = "MAX")

# For analyses below, need to make a table of these data with '0' if participants did not list said code
mgl.tab <- FreeListTable(sihanaka_fl_mgl.s, Subj = "PARTID", Order = "ORDER", CODE = "BGCARE_GEN_FINAL", 
                         Salience = "BGCARE.S", tableType = "MAX_SALIENCE")
head(mgl.tab)

# Ordered beta model for top 8 items, calculating Smith's S and uncertainty intervals for each
mgl.final <- SalienceOrdBeta(mgl.tab, var_sel = "TOP", top = 8, seed = 18458, IDs_first = TRUE)

# Summary of items
mgl.summary <- SalienceEstimateSummary(mgl.final, quantiles = c(0.025, 0.5, 0.975))

# Flower-plot of results
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = mgl.s, S_uncert = mgl.summary, label = "MG LIKES")

# Full distribution plot of results
mgl.dist <- SalienceEstimatePlot(mgl.final, order = "high-low")

# Drop the last items, for consistency across items
mgl.final2 <- mgl.final[c(-7, -8)]
mgl.dist2 <- SalienceEstimatePlot(mgl.final2, order = "high-low")

# Add and edit title
mgl.dist2 <- mgl.dist2 +
  labs(title = "Ndrianampanjaka likes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))


## MG dislikes
table(sihanaka_fl$BGDIS_GEN_FINAL, useNA = "ifany")

# Keep just relevant variables and remove missing data
sihanaka_fl_mgd <- sihanaka_fl %>%
  select(c(PARTID, ORDER, BGDIS_GEN_FINAL)) %>%
  filter(!is.na(BGDIS_GEN_FINAL))

sihanaka_fl_mgd <- as.data.frame(sihanaka_fl_mgd)

# Calculate item salience
sihanaka_fl_mgd.s <- CalculateSalience(sihanaka_fl_mgd, Subj = "PARTID", Order = "ORDER",
                                       CODE = "BGDIS_GEN_FINAL", Salience = "BGDIS.S")
head(sihanaka_fl_mgd.s)

# Calculate Smith's S for each code
mgd.s <- SalienceByCode(sihanaka_fl_mgd.s, Subj = "PARTID", CODE = "BGDIS_GEN_FINAL", 
                        Salience = "BGDIS.S", dealWithDoubles = "MAX")

# For analyses below, need to make a table of these data with '0' if participants did not list said code
mgd.tab <- FreeListTable(sihanaka_fl_mgd.s, Subj = "PARTID", Order = "ORDER", CODE = "BGDIS_GEN_FINAL", 
                         Salience = "BGDIS.S", tableType = "MAX_SALIENCE")
head(mgd.tab)

# Ordered beta model for top 8 items, calculating Smith's S and uncertainty intervals for each
mgd.final <- SalienceOrdBeta(mgd.tab, var_sel = "TOP", top = 8, seed = 654723, IDs_first = TRUE)

# Summary of items
mgd.summary <- SalienceEstimateSummary(mgd.final, quantiles = c(0.025, 0.5, 0.975))

# Flower-plot of results
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = mgd.s, S_uncert = mgd.summary, label = "MG DISLIKES")

# Full distribution plot of results
mgd.dist <- SalienceEstimatePlot(mgd.final, order = "high-low")

# Drop the last items, as high density at low values, so wacky scales
mgd.final2 <- mgd.final[c(-7, -8)]
mgd.dist2 <- SalienceEstimatePlot(mgd.final2, order = "high-low")

# Add and edit title
mgd.dist2 <- mgd.dist2 +
  labs(title = "Ndrianampanjaka dislikes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))


## LG likes
table(sihanaka_fl$LGCARE_GEN_FINAL, useNA = "ifany")

# Keep just relevant variables and remove missing data
sihanaka_fl_lgl <- sihanaka_fl %>%
  select(c(PARTID, ORDER, LGCARE_GEN_FINAL)) %>%
  filter(!is.na(LGCARE_GEN_FINAL))

sihanaka_fl_lgl <- as.data.frame(sihanaka_fl_lgl)

# Calculate item salience
sihanaka_fl_lgl.s <- CalculateSalience(sihanaka_fl_lgl, Subj = "PARTID", Order = "ORDER",
                                       CODE = "LGCARE_GEN_FINAL", Salience = "LGCARE.S")
head(sihanaka_fl_lgl.s)

# Calculate Smith's S for each code
lgl.s <- SalienceByCode(sihanaka_fl_lgl.s, Subj = "PARTID", CODE = "LGCARE_GEN_FINAL", 
                        Salience = "LGCARE.S", dealWithDoubles = "MAX")

# For analyses below, need to make a table of these data with '0' if participants did not list said code
lgl.tab <- FreeListTable(sihanaka_fl_lgl.s, Subj = "PARTID", Order = "ORDER", CODE = "LGCARE_GEN_FINAL", 
                         Salience = "LGCARE.S", tableType = "MAX_SALIENCE")
head(lgl.tab)

# Ordered beta model for top 8 items, calculating Smith's S and uncertainty intervals for each
lgl.final <- SalienceOrdBeta(lgl.tab, var_sel = "TOP", top = 8, seed = 13463, IDs_first = TRUE)

# Summary of items
lgl.summary <- SalienceEstimateSummary(lgl.final, quantiles = c(0.025, 0.5, 0.975))

# Flower-plot of results
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = lgl.s, S_uncert = lgl.summary, label = "LG LIKES")

# Full distribution plot of results
lgl.dist <- SalienceEstimatePlot(lgl.final, order = "high-low")

# Drop the last items, for consistency across items
lgl.final2 <- lgl.final[c(-7, -8)]
lgl.dist2 <- SalienceEstimatePlot(lgl.final2, order = "high-low")

# Add and edit title
lgl.dist2 <- lgl.dist2 +
  labs(title = "Zazavavindrano likes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))


## LG dislikes
table(sihanaka_fl$LGDIS_GEN_FINAL, useNA = "ifany")

# Keep just relevant variables and remove missing data
sihanaka_fl_lgd <- sihanaka_fl %>%
  select(c(PARTID, ORDER, LGDIS_GEN_FINAL)) %>%
  filter(!is.na(LGDIS_GEN_FINAL))

sihanaka_fl_lgd <- as.data.frame(sihanaka_fl_lgd)

# Calculate item salience
sihanaka_fl_lgd.s <- CalculateSalience(sihanaka_fl_lgd, Subj = "PARTID", Order = "ORDER",
                                       CODE = "LGDIS_GEN_FINAL", Salience = "LGDIS.S")
head(sihanaka_fl_lgd.s)

# Calculate Smith's S for each code
lgd.s <- SalienceByCode(sihanaka_fl_lgd.s, Subj = "PARTID", CODE = "LGDIS_GEN_FINAL", 
                        Salience = "LGDIS.S", dealWithDoubles = "MAX")

# For analyses below, need to make a table of these data with '0' if participants did not list said code
lgd.tab <- FreeListTable(sihanaka_fl_lgd.s, Subj = "PARTID", Order = "ORDER", CODE = "LGDIS_GEN_FINAL", 
                         Salience = "LGDIS.S", tableType = "MAX_SALIENCE")
head(lgd.tab)

# Ordered beta model for top 8 items, calculating Smith's S and uncertainty intervals for each
lgd.final <- SalienceOrdBeta(lgd.tab, var_sel = "TOP", top = 8, seed = 87547, IDs_first = TRUE)

# Summary of items
lgd.summary <- SalienceEstimateSummary(lgd.final, quantiles = c(0.025, 0.5, 0.975))

# Flower-plot of results
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = lgd.s, S_uncert = lgd.summary, label = "LG DISLIKES")

# Full distribution plot of results
lgd.dist <- SalienceEstimatePlot(lgd.final, order = "high-low")

# Drop the last items, as high density at low values, so wacky scales
lgd.final2 <- lgd.final[c(-7, -8)]
lgd.dist2 <- SalienceEstimatePlot(lgd.final2, order = "high-low")

# Add and edit title
lgd.dist2 <- lgd.dist2 +
  labs(title = "Zazavavindrano dislikes") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))


## Combine plots together, and save
pdf(file = "GodsConcerns_Flower.pdf", height = 11, width = 11)
par(mar = c(0, 0, 0, 0), mfrow = c(2, 2))
FlowerPlotIntervals(SmithsS = mgl.s, S_uncert = mgl.summary, label = "Ndrianampanjaka\nlikes")
FlowerPlotIntervals(SmithsS = mgd.s, S_uncert = mgd.summary, label = "Ndrianampanjaka\ndislikes")
FlowerPlotIntervals(SmithsS = lgl.s, S_uncert = lgl.summary, label = "Zazavavindrano\nlikes")
FlowerPlotIntervals(SmithsS = lgd.s, S_uncert = lgd.summary, label = "Zazavavindrano\ndislikes")
dev.off()

pdf(file = "GodsConcerns_dist.pdf", height = 12, width = 16)
gridExtra::grid.arrange(mgl.dist2, mgd.dist2, lgl.dist2, lgd.dist2)
dev.off()


### Cooperation

## RAGs

# Order (1 = local game first)
addmargins(table(sihanaka_main$INGFIRST))
round(prop.table(table(sihanaka_main$INGFIRST)) * 100, 1)


## Local RAG (RAG 1)

# Comprehension checks - All only needed two checks, so no need to include in models below
addmargins(table(sihanaka_main$EXUSED1))
round(prop.table(table(sihanaka_main$EXUSED1)) * 100, 1)

sihanaka_main <- sihanaka_main %>%
  select(-EXUSED1)

# Tokens to distant co-religionist
summary(sihanaka_main$COREL.L)
sd(sihanaka_main$COREL.L, na.rm = TRUE)


## Self RAG (RAG 2)

# Comprehension checks - All only needed two checks, so no need to include in models below
addmargins(table(sihanaka_main$EXUSED2))
round(prop.table(table(sihanaka_main$EXUSED2)) * 100, 1)

sihanaka_main <- sihanaka_main %>%
  select(-EXUSED2)

# Tokens to distant co-religionist
summary(sihanaka_main$COREL.S)
sd(sihanaka_main$COREL.S, na.rm = TRUE)


## Plot of RAG results to theoretical binomial distribution
pdf(file = "Sihanaka_RAGtoBinom.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

# Local RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "LOCAL RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.15), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(sihanaka_main$COREL.L, na.rm = TRUE))
polygon(density(sihanaka_main$COREL.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(sihanaka_main$COREL.L, na.rm = TRUE), lty = "dashed", col = "red")

# Self RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "SELF RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.15), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(sihanaka_main$COREL.S, na.rm = TRUE))
polygon(density(sihanaka_main$COREL.S, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(sihanaka_main$COREL.S, na.rm = TRUE), lty = "dashed", col = "red")

dev.off()


### Test whether RAG results differ from unbiased binomial 50/50 distribution

# LOCAL RAG
mod_RAG.L_base <- brm(COREL.L | trials(30) ~ 1,
                      family = "binomial",
                      data = sihanaka_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 16965)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_base)

# 95% CIs of log-odds is essentially 0/odds essentially 1, suggesting no bias towards local recipients
exp(fixef(mod_RAG.L_base))


# SELF RAG
mod_RAG.S_base <- brm(COREL.S | trials(30) ~ 1,
                      family = "binomial",
                      data = sihanaka_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 36343)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_base)

# 95% CIs of log-odds exclude 0/odds exclude 1, suggesting bias towards self rather than distant
exp(fixef(mod_RAG.S_base))


## Formal test of difference between local and self RAG donations
df_temp <- as.data.frame(cbind(RAG = c(sihanaka_main$COREL.L, sihanaka_main$COREL.S), 
                               Cond = c(rep("Local", nrow(sihanaka_main)), rep("Self", nrow(sihanaka_main))),
                               CERCID = c(sihanaka_main$CERCID, sihanaka_main$CERCID)))

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
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 68800)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_RAG.LvsS, variables = "Cond", type = "prediction")


## DGs

# Order (1 = local game first)
addmargins(table(sihanaka_main$INGFIRST_DG))
round(prop.table(table(sihanaka_main$INGFIRST_DG)) * 100, 1)


## Local DG (DG 1)

# Tokens to distant co-religionist
summary(sihanaka_main$COREL.L_DG)
sd(sihanaka_main$COREL.L_DG, na.rm = TRUE)

addmargins(table(sihanaka_main$COREL.L_DG))
round(prop.table(table(sihanaka_main$COREL.L_DG)) * 100, 1)


## Self DG (RAG 2)

# Tokens to distant co-religionist
summary(sihanaka_main$COREL.S_DG)
sd(sihanaka_main$COREL.S_DG, na.rm = TRUE)

addmargins(table(sihanaka_main$COREL.S_DG))
round(prop.table(table(sihanaka_main$COREL.S_DG)) * 100, 1)


## Plot of DG results
pdf(file = "Sihanaka_DGDescriptives.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(sihanaka_main$COREL.L_DG, na.rm = TRUE), 
     main = "LOCAL DG", xlab = "Donations to Distant",
     ylim = c(0, 0.3), xlim = c(-2, 11), axes = FALSE)
axis(side = 1, at = c(0, 2, 4, 6, 8, 10))
axis(side = 2)
box()
polygon(density(sihanaka_main$COREL.L_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(sihanaka_main$COREL.L_DG, na.rm = TRUE), lty = "dashed")

plot(density(sihanaka_main$COREL.S_DG, na.rm = TRUE), 
     main = "SELF DG", xlab = "Donations to Distant",
     ylim = c(0, 0.3), xlim = c(-2, 11), axes = FALSE)
axis(side = 1, at = c(0, 2, 4, 6, 8, 10))
axis(side = 2)
box()
polygon(density(sihanaka_main$COREL.S_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(sihanaka_main$COREL.S_DG, na.rm = TRUE), lty = "dashed")

dev.off()


## Formal test of difference between local and self DG donations
df_temp <- as.data.frame(cbind(DG = c(sihanaka_main$COREL.L_DG, sihanaka_main$COREL.S_DG), 
                               Cond = c(rep("Local", nrow(sihanaka_main)), rep("Self", nrow(sihanaka_main))),
                               CERCID = c(sihanaka_main$CERCID, sihanaka_main$CERCID)))

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
                   iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 21471)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_DG.LvsS, variables = "Cond", type = "prediction")


###########################################################################################
### Complete-case analyses: Main regression analyses using causal model (with age, sex, education, number of children, material security and fisher as confounders, plus game order as covariates [no RAG comprehension checks as no variability])

## Remove missing data
dat_cc <- sihanaka_main[complete.cases(sihanaka_main$COREL.L, sihanaka_main$COREL.S,
                                       sihanaka_main$COREL.L_DG, sihanaka_main$COREL.S_DG,
                                       sihanaka_main$LGOMNI_AVE, sihanaka_main$LGPUNISH,
                                       sihanaka_main$AGE, sihanaka_main$SEX, sihanaka_main$FORMALED,
                                       sihanaka_main$MMAT, sihanaka_main$CHILDREN, sihanaka_main$FISHER,
                                       sihanaka_main$INGFIRST, sihanaka_main$INGFIRST_DG), ]

### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm(COREL.L | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                   INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 81704)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L)

# Add model fit, so can compare models
mod_RAG.L <- add_criterion(mod_RAG.L, "loo")
loo_RAG.L <- loo(mod_RAG.L)
loo_RAG.L


## Interaction model
mod_RAG.L_int <- brm(COREL.L | trials(30) ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                       INGFIRST,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 50371)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_int)

# Add model fit, so can compare models
mod_RAG.L_int <- add_criterion(mod_RAG.L_int, "loo")
loo_RAG.L_int <- loo(mod_RAG.L_int)
loo_RAG.L_int

# Compare to base model without interaction term - Slight improvement in model fit with interaction, but not massive and quite uncertain
loo_compare(loo_RAG.L, loo_RAG.L_int)


## G-computation (based on additive model first)

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
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
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_int, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
sup1 <- predict(mod_RAG.L_int, newdata = df_sup1, summary = FALSE)

## Difference between counter-factual states from each of the posterior samples
RAG.L_0_int <- rep(NA, nrow(sup0))
RAG.L_1_int <- rep(NA, nrow(sup0))
diff_RAG.L_int <- rep(NA, nrow(sup0))
for (i in 1:nrow(sup0)) {
  RAG.L_0_int[i] <- mean(sup0[i, ])
  RAG.L_1_int[i] <- mean(sup1[i, ])
  diff_RAG.L_int[i] <- RAG.L_1_int[i] - RAG.L_0_int[i]
}

plot(density(diff_RAG.L_int), main = "Local RAG (interaction model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_int)
quantile(diff_RAG.L_int, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_int, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_int, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF

## Additive model
mod_RAG.S <- brm(COREL.S | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                   INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 32352)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S)

# Add model fit, so can compare models
mod_RAG.S <- add_criterion(mod_RAG.S, "loo")
loo_RAG.S <- loo(mod_RAG.S)
loo_RAG.S


## Interaction model
mod_RAG.S_int <- brm(COREL.S | trials(30) ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                       INGFIRST,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 88365)

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
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
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

plot(density(diff_RAG.S), main = "Self RAG (additive model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S)
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))

quantile(RAG.S_0, c(0.025, 0.5, 0.975))
quantile(RAG.S_1, c(0.025, 0.5, 0.975))


## Density plots of RAG results
pdf(file = "Sihanaka_RAG_gcomp_CCA.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.4), xlim = c(-5, 6),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.4), xlim = c(-5, 6),
     main = "SELF RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_RAG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


### DG - DISTANT vs LOCAL

# Make sure outcome is an ordered factor
dat_cc$COREL.L_DG <- factor(dat_cc$COREL.L_DG, ordered = TRUE)

## Additive model
mod_DG.L <- brm(COREL.L_DG ~ LGPUNISH + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = dat_cc,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 77923)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L)

# Add model fit, so can compare models
mod_DG.L <- add_criterion(mod_DG.L, "loo")
loo_DG.L <- loo(mod_DG.L)
loo_DG.L


## Interaction model
mod_DG.L_int <- brm(COREL.L_DG ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = dat_cc,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 31270)

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
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
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
mod_DG.S <- brm(COREL.S_DG ~ LGPUNISH + LGOMNI_AVE +
                  AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = dat_cc,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 38903)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S)

# Add model fit, so can compare models
mod_DG.S <- add_criterion(mod_DG.S, "loo")
loo_DG.S <- loo(mod_DG.S)
loo_DG.S


## Interaction model
mod_DG.S_int <- brm(COREL.S_DG ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                      AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                      INGFIRST_DG,
                    family = cumulative("logit"),
                    data = dat_cc,
                    prior = c(prior(normal(0, 1), class = b),
                              prior(normal(0, 2), class = Intercept)),
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 43613)

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
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN, FISHER = dat_cc$FISHER,
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
pdf(file = "Sihanaka_DG_gcomp_CCA.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 0.7), xlim = c(-3, 4),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 0.7), xlim = c(-3, 4),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()


### As are some quite large differences between MI and CCA results for the local DG, will compare key variables between cases included vs excluded from the CCA
sihanaka_main$CCA <- ifelse(complete.cases(sihanaka_main$COREL.L, sihanaka_main$COREL.S,
                                       sihanaka_main$COREL.L_DG, sihanaka_main$COREL.S_DG,
                                       sihanaka_main$LGOMNI_AVE, sihanaka_main$LGPUNISH,
                                       sihanaka_main$AGE, sihanaka_main$SEX, sihanaka_main$FORMALED,
                                       sihanaka_main$MMAT, sihanaka_main$CHILDREN, sihanaka_main$FISHER,
                                       sihanaka_main$INGFIRST, sihanaka_main$INGFIRST_DG) == 1, 1, 0)
table(sihanaka_main$CCA)

by(sihanaka_main$COREL.L, sihanaka_main$CCA, summary); summary(sihanaka_main$COREL.L)
by(sihanaka_main$COREL.S, sihanaka_main$CCA, summary); summary(sihanaka_main$COREL.S)
by(sihanaka_main$COREL.L_DG, sihanaka_main$CCA, summary); summary(sihanaka_main$COREL.L_DG)
by(sihanaka_main$COREL.S_DG, sihanaka_main$CCA, summary); summary(sihanaka_main$COREL.S_DG)
by(sihanaka_main$LGPUNISH, sihanaka_main$CCA, summary)
by(sihanaka_main$LGOMNI_AVE, sihanaka_main$CCA, summary)
by(sihanaka_main$AGE, sihanaka_main$CCA, summary)
by(sihanaka_main$SEX, sihanaka_main$CCA, summary)
by(sihanaka_main$FORMALED, sihanaka_main$CCA, summary)
by(sihanaka_main$CHILDREN, sihanaka_main$CCA, summary)
by(sihanaka_main$MMAT, sihanaka_main$CCA, summary)
by(sihanaka_main$FISHER, sihanaka_main$CCA, summary)


#################################################################
#### Sensitivity analysis 1: Removal of all confounders other than 'fisher'

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm(COREL.L | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                         FISHER +
                         INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 25460)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
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

# No real difference in interpretation
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm(COREL.S | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                         FISHER +
                         INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 37790)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               FISHER = dat_cc$FISHER,
                               INGFIRST = dat_cc$INGFIRST))
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

# No real difference in interpretation
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens1 <- brm(COREL.L_DG ~ LGPUNISH + LGOMNI_AVE +
                        FISHER +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 58796)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               FISHER = dat_cc$FISHER,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.L_sens1, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               FISHER = dat_cc$FISHER,
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

plot(density(diff_DG.L_sens1), main = "Local DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens1)
quantile(diff_DG.L_sens1, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens1, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens1 <- brm(COREL.S_DG ~ LGPUNISH + LGOMNI_AVE +
                        FISHER +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 91396)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGOMNI_AVE = 0,
                               FISHER = dat_cc$FISHER,
                               INGFIRST_DG = dat_cc$INGFIRST_DG))
sup0 <- predict(mod_DG.S_sens1, newdata = df_sup0, summary = FALSE)

# Convert these back from factors to numbers of tokens
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGOMNI_AVE = 1,
                               FISHER = dat_cc$FISHER, 
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

plot(density(diff_DG.S_sens1), main = "Self DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens1)
quantile(diff_DG.S_sens1, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens1, c(0.025, 0.5, 0.975))

# Relationship is more strongly positive here, but still broadly consistent with before
quantile(diff_DG.S, c(0.025, 0.5, 0.975))


###########################################################################################
### Multiple imputation analyses

str(sihanaka_main)
summary(sihanaka_main)

## Summary of missing data
cbind(n_miss = sapply(sihanaka_main, function(x) sum(is.na(x))),
      per_miss = sapply(sihanaka_main, function(x) round(sum(is.na(x)) / nrow(sihanaka_main) * 100, 2)))

# Drop the LG and MG DIE variables, and the punishment mini-scales
sihanaka_main_forMI <- sihanaka_main %>%
  select(-c(LGDIE, BGDIE, LGPUN_AVE, BGPUN_AVE))

# Code binary variables as factors (for MICE to work with logistic regression)
sihanaka_main_forMI$SEX <- as.factor(sihanaka_main_forMI$SEX)
sihanaka_main_forMI$FISHER <- as.factor(sihanaka_main_forMI$FISHER)
sihanaka_main_forMI$INGFIRST <- as.factor(sihanaka_main_forMI$INGFIRST)
sihanaka_main_forMI$INGFIRST_DG <- as.factor(sihanaka_main_forMI$INGFIRST_DG)
sihanaka_main_forMI$LGPUNISH <- as.factor(sihanaka_main_forMI$LGPUNISH)
#sihanaka_main_forMI$LGDIE <- as.factor(sihanaka_main_forMI$LGDIE)
sihanaka_main_forMI$LGFEEL <- as.factor(sihanaka_main_forMI$LGFEEL)
sihanaka_main_forMI$LGSEE <- as.factor(sihanaka_main_forMI$LGSEE)
sihanaka_main_forMI$BGPUNISH <- as.factor(sihanaka_main_forMI$BGPUNISH)
#sihanaka_main_forMI$BGDIE <- as.factor(sihanaka_main_forMI$BGDIE)
sihanaka_main_forMI$BGFEEL <- as.factor(sihanaka_main_forMI$BGFEEL)
sihanaka_main_forMI$BGSEE <- as.factor(sihanaka_main_forMI$BGSEE)
sihanaka_main_forMI$LGPERFHO <- as.factor(sihanaka_main_forMI$LGPERFHO)
sihanaka_main_forMI$LGBLV <- as.factor(sihanaka_main_forMI$LGBLV)
sihanaka_main_forMI$LGTHINK <- as.factor(sihanaka_main_forMI$LGTHINK)
sihanaka_main_forMI$BGPERFHO <- as.factor(sihanaka_main_forMI$BGPERFHO)
sihanaka_main_forMI$BGBLV <- as.factor(sihanaka_main_forMI$BGBLV)
sihanaka_main_forMI$BGTHINK <- as.factor(sihanaka_main_forMI$BGTHINK)

str(sihanaka_main_forMI)
summary(sihanaka_main_forMI)


## Set up imputations - Change mini-scales from PMM to passively impute (as is just based on means of two prior variables), and change ordered factors from 'polyreg' (mulitnomial model) to 'polr' (ordinal model)
meth <- make.method(sihanaka_main_forMI)
#meth["LGPUN_AVE"] <- "~ I(((as.numeric(LGPUNISH) - 1) + (as.numeric(LGDIE) - 1)) / 2)"
meth["LGOMNI_AVE"] <- "~ I(((as.numeric(LGFEEL) - 1) + (as.numeric(LGSEE) - 1)) / 2)"
#meth["BGPUN_AVE"] <- "~ I(((as.numeric(BGPUNISH) - 1) + (as.numeric(BGDIE) - 1)) / 2)"
meth["BGOMNI_AVE"] <- "~ I(((as.numeric(BGFEEL) - 1) + (as.numeric(BGSEE) - 1)) / 2)"
meth["LGPERFHO"] <- "polr"
meth["BGPERFHO"] <- "polr"
meth["LGTHINK"] <- "polr"
meth["BGTHINK"] <- "polr"
meth

# Predictor matrix - Exclude omniscience scores as predictors, and ID
pred <- make.predictorMatrix(sihanaka_main_forMI)
#pred[, "LGPUN_AVE"] <- 0
pred[, "LGOMNI_AVE"] <- 0
#pred[, "BGPUN_AVE"] <- 0
pred[, "BGOMNI_AVE"] <- 0
pred[, "CERCID"] <- 0
pred["CERCID", ] <- 0
pred

# Visit sequence - Make sure passively-imputed scales are after the individual variables (they are)
visit <- make.visitSequence(sihanaka_main_forMI)
visit

# Test imputation to check formulas and that no obvious errors
test <- mice(sihanaka_main_forMI, m = 5, maxit = 0, method = meth, predictorMatrix = pred, visitSequence = visit)
test$formulas

## Run imputations - 20 imputations as approx. 20% cases with missing data (and to avoid having too many datasets, which slows down computation when running Bayesian models on these data)
imp <- mice(sihanaka_main_forMI, m = 20, maxit = 10, 
            method = meth, predictorMatrix = pred, visitSequence = visit,
            seed = 29811, print = TRUE)

## Check imputations worked correctly
imp1 <- complete(imp, 1)

head(sihanaka_main_forMI, n = 20L)
head(imp1, n = 20L)


## Save these imputed datasets, to save time if need them later
save(imp, file = "Sihanaka_imp.RData")
#load("Sihanaka_imp.RData")


#### Descriptive statistics for variables with missing data
temp_imp <- complete(imp, "long")

## Age
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(AGE), iqr25 = quantile(AGE, 0.25), iqr75 = quantile(AGE, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## Sex (male)
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(SEX == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## Education
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(FORMALED), iqr25 = quantile(FORMALED, 0.25), iqr75 = quantile(FORMALED, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## Number of children
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(CHILDREN), iqr25 = quantile(CHILDREN, 0.25), iqr75 = quantile(CHILDREN, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## Material security
temp_imp %>%
  group_by(.imp) %>%
  summarise(median = median(MMAT), iqr25 = quantile(MMAT, 0.25), iqr75 = quantile(MMAT, 0.75)) %>%
  ungroup() %>%
  summarise(mean_med = mean(median), mean_iqr25 = mean(iqr25), mean_iqr75 = mean(iqr75))

## Fisher
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(FISHER == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGPUNISH
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGPUNISH == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGFEEL
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGFEEL == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGSEE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGSEE == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGPUNISH
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGPUNISH == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGFEEL
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGFEEL == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGSEE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGSEE == "1"), per = N / nrow(sihanaka_main_forMI) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LG omniscience mini-scale
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(LGOMNI_AVE)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean))

## MG omniscience mini-scale
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(BGOMNI_AVE)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean))


## Correlations between religiosity variables
# Extract imputed datasets
imp_all <- complete(imp, action = "all")

# Set up lists to store results in
MGLG_pun <- rep(NA, length(imp_all))
#MGLG_die <- rep(NA, length(imp_all))
#LG_pundie <- rep(NA, length(imp_all))
#MG_pundie <- rep(NA, length(imp_all))
#MGLG_pun_scale <- rep(NA, length(imp_all))
MGLG_feel <- rep(NA, length(imp_all))
MGLG_see <- rep(NA, length(imp_all))
LG_feelsee <- rep(NA, length(imp_all))
MG_feelsee <- rep(NA, length(imp_all))
MGLG_omni_scale <- rep(NA, length(imp_all))
#LG_punomni_scale <- rep(NA, length(imp_all))
#MG_punomni_scale <- rep(NA, length(imp_all))

# Loop over each imputed dataset and run correlations
for (i in 1:length(imp_all)) {
  print(paste0("On imputed dataset: ", i))
  
  # Extract ith imputed dataset
  df_temp <- imp_all[[i]]
  
  # Run correlations on ith imputed dataset adn store results
  MGLG_pun[i] <- tetrachoric(cbind(df_temp$LGPUNISH, df_temp$BGPUNISH))$rho[1, 2]
  #MGLG_die[i] <- tetrachoric(cbind(df_temp$LGDIE, df_temp$BGDIE))$rho[1, 2]
  #LG_pundie[i] <- tetrachoric(cbind(df_temp$LGPUNISH, df_temp$LGDIE))$rho[1, 2]
  #MG_pundie[i] <- tetrachoric(cbind(df_temp$BGPUNISH, df_temp$BGDIE))$rho[1, 2]
  #MGLG_pun_scale[i] <- polychoric(cbind(as.factor(df_temp$LGPUN_AVE), as.factor(df_temp$BGPUN_AVE)))$rho[1, 2]
  MGLG_feel[i] <- tetrachoric(cbind(df_temp$LGFEEL, df_temp$BGFEEL))$rho[1, 2]
  MGLG_see[i] <- tetrachoric(cbind(df_temp$LGSEE, df_temp$BGSEE))$rho[1, 2]
  LG_feelsee[i] <- tetrachoric(cbind(df_temp$LGFEEL, df_temp$LGSEE))$rho[1, 2]
  MG_feelsee[i] <- tetrachoric(cbind(df_temp$BGFEEL, df_temp$BGSEE))$rho[1, 2]
  MGLG_omni_scale[i] <- polychoric(cbind(as.factor(df_temp$LGOMNI_AVE), as.factor(df_temp$BGOMNI_AVE)))$rho[1, 2]
  #LG_punomni_scale[i] <- polychoric(cbind(as.factor(df_temp$LGPUN_AVE), as.factor(df_temp$LGOMNI_AVE)))$rho[1, 2]
  #MG_punomni_scale[i] <- polychoric(cbind(as.factor(df_temp$BGPUN_AVE), as.factor(df_temp$BGOMNI_AVE)))$rho[1, 2]
  
}


## Get mean of correlations
mean(MGLG_pun)
#mean(MGLG_die)
#mean(LG_pundie)
#mean(MG_pundie)
#mean(MGLG_pun_scale)
mean(MGLG_feel)
mean(MGLG_see)
mean(LG_feelsee)
mean(MG_feelsee)
mean(MGLG_omni_scale)
#mean(LG_punomni_scale)
#mean(MG_punomni_scale)


#### Main regression analyses using causal model (with age, sex, education, number of children, material security and fishing occupation as confounders, plus game order as covariates [no RAG comprehension checks as no variability]) #####

## Using 'brm_multiple' to perform analyses on each imputed dataset and then pool results together (see https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html)

## Extract each imputed dataset and convert binary variables back to integer from factors
imp_all <- complete(imp, action = "all")

for (i in 1:length(imp_all)) {
  temp <- imp_all[[i]]
  temp$SEX <- as.integer(temp$SEX) - 1
  temp$INGFIRST <- as.integer(temp$INGFIRST) - 1
  temp$INGFIRST_DG <- as.integer(temp$INGFIRST_DG) - 1
  temp$LGPUNISH <- as.integer(temp$LGPUNISH) - 1
  temp$FISHER <- as.integer(temp$FISHER) - 1
  
  # Replace old imputed data with new one
  imp_all[[i]] <- temp
}

str(imp_all)


### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm_multiple(COREL.L | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                            INGFIRST,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 79874,
                          combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L, check_data = FALSE)

## To note: r-hat and effective sample size values may indicate poor fitting and convergence issues, but this is because different imputed datasets have different values, so may not give exactly the same results - This is completely normal (and to be expected with MI!), so is likely a false positive. Still, worth checking that each individual chain converged, just to make sure
summary(mod_RAG.L[[1]])
summary(mod_RAG.L[[2]])


## Model with interaction between exposures
mod_RAG.L_int <- brm_multiple(COREL.L | trials(30) ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                                AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                                INGFIRST,
                              family = "binomial",
                              data = imp_all,
                              prior = c(prior(normal(0, 1), class = b),
                                        prior(normal(0, 2), class = Intercept)),
                              iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 79509,
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
  temp$LGPUNISH <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
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


## G-computation (based on interaction model)

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
  temp$LGPUNISH <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L_int[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
  temp$LGOMNI_AVE <- 1
  sup1_temp <- predict(mod_RAG.L_int[[i]], newdata = temp, summary = FALSE)
  
  sup1[[i]] <- sup1_temp
}


## Combine posterior samples together
sup0_all <- do.call(rbind, sup0)
sup1_all <- do.call(rbind, sup1)

# Difference between counter-factual states from each of the posterior samples
RAG.L_0_int <- rep(NA, nrow(sup0_all))
RAG.L_1_int <- rep(NA, nrow(sup0_all))
diff_RAG.L_int <- rep(NA, nrow(sup0_all))
for (i in 1:nrow(sup0_all)) {
  RAG.L_0_int[i] <- mean(sup0_all[i, ])
  RAG.L_1_int[i] <- mean(sup1_all[i, ])
  diff_RAG.L_int[i] <- RAG.L_1_int[i] - RAG.L_0_int[i]
}

plot(density(diff_RAG.L_int), main = "Local RAG (interaction model)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_int)
quantile(diff_RAG.L_int, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_int, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_int, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF

## Additive model
mod_RAG.S <- brm_multiple(COREL.S | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                            INGFIRST,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 84676,
                          combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S, check_data = FALSE)


## Model with interaction between exposures
mod_RAG.S_int <- brm_multiple(COREL.S | trials(30) ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                                AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                                INGFIRST,
                              family = "binomial",
                              data = imp_all,
                              prior = c(prior(normal(0, 1), class = b),
                                        prior(normal(0, 2), class = Intercept)),
                              iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 90262,
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
  temp$LGPUNISH <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.S[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
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
pdf(file = "Sihanaka_RAG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.42), xlim = c(-5, 5),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.42), xlim = c(-5, 5),
     main = "SELF RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_RAG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()



### DG - DISTANT vs LOCAL

## Additive model (Make sure outcome is an ordered factor)
mod_DG.L <- brm_multiple(factor(COREL.L_DG, ordered = TRUE) ~ LGPUNISH + LGOMNI_AVE +
                           AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                           INGFIRST_DG,
                         family = cumulative("logit"),
                         data = imp_all,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0, 2), class = Intercept)),
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 36887,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L, check_data = FALSE)


## Model with interaction between exposures (Make sure outcome is an ordered factor)
mod_DG.L_int <- brm_multiple(factor(COREL.L_DG, ordered = TRUE) ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                               AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                               INGFIRST_DG,
                             family = cumulative("logit"),
                             data = imp,
                             prior = c(prior(normal(0, 1), class = b),
                                       prior(normal(0, 2), class = Intercept)),
                             iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 48831,
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
  temp$LGPUNISH <- 0
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
  temp$LGPUNISH <- 1
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

## Additive model (Make sure outcome is an ordered factor)
mod_DG.S <- brm_multiple(factor(COREL.S_DG, ordered = TRUE) ~ LGPUNISH + LGOMNI_AVE +
                           AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                           INGFIRST_DG,
                         family = cumulative("logit"),
                         data = imp_all,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0, 2), class = Intercept)),
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 59135,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S, check_data = FALSE)


## Model with interaction between exposures (Make sure outcome is an ordered factor)
mod_DG.S_int <- brm_multiple(factor(COREL.S_DG, ordered = TRUE) ~ LGPUNISH + LGOMNI_AVE + LGPUNISH:LGOMNI_AVE +
                               AGE + SEX + FORMALED + MMAT + CHILDREN + FISHER +
                               INGFIRST_DG,
                             family = cumulative("logit"),
                             data = imp_all,
                             prior = c(prior(normal(0, 1), class = b),
                                       prior(normal(0, 2), class = Intercept)),
                             iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 15446,
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
  temp$LGPUNISH <- 0
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
  temp$LGPUNISH <- 1
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
pdf(file = "Sihanaka_DG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 0.85), xlim = c(-3, 2),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 0.85), xlim = c(-3, 2),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()



#################################################################
#### Sensitivity analysis 1: Only including 'fisher' as confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here

### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm_multiple(COREL.L | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                            FISHER +
                            INGFIRST,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 71097,
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
  temp$LGPUNISH <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.L_sens1[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
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

plot(density(diff_RAG.L_sens1), main = "Local RAG (fisher only)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens1)
quantile(diff_RAG.L_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm_multiple(COREL.S | trials(30) ~ LGPUNISH + LGOMNI_AVE +
                                  FISHER +
                                  INGFIRST,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 2553,
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
  temp$LGPUNISH <- 0
  temp$LGOMNI_AVE <- 0
  sup0_temp <- predict(mod_RAG.S_sens1[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
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

plot(density(diff_RAG.S_sens1), main = "Self RAG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens1)
quantile(diff_RAG.S_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens1 <- brm_multiple(factor(COREL.L_DG, ordered = TRUE) ~ LGPUNISH + LGOMNI_AVE +
                           FISHER +
                           INGFIRST_DG,
                         family = cumulative("logit"),
                         data = imp_all,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0, 2), class = Intercept)),
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 23812,
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
  temp$LGPUNISH <- 0
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
  temp$LGPUNISH <- 1
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

plot(density(diff_DG.L_sens1), main = "Local DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens1)
quantile(diff_DG.L_sens1, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens1 <- brm_multiple(factor(COREL.S_DG, ordered = TRUE) ~ LGPUNISH + LGOMNI_AVE +
                                 FISHER +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 99603,
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
  temp$LGPUNISH <- 0
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
  temp$LGPUNISH <- 1
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
