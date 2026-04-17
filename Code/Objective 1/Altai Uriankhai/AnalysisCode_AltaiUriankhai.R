### Objective 1 Analysis Code -  Altai Uriankhai
### Created 24/2/2026 by Dan Major-Smith
### R 4.4.1

####################################################################
#### Clear workspace, set working directory and install/load packages
rm(list=ls())
Sys.setenv(LANG = "en")

setwd("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/Projects/Attila_AU/Objective 1 - Altai Uriankhai")
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

#install.packages("posterior")
library(posterior)


#####################################################################
### Read in and process Altai Uriankhai data (note that this step processes the raw data which is not yet openly-available. To follow these analyses, read in the data on line 980)


## Demographics first
altai_demo <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Mateffy/Data/GGSL-Demographics-AUD001-AUD099.xlsx",
                         sheet = "Demographics")

head(altai_demo)
glimpse(altai_demo)

# Remove some empty variables and unused IDs
altai_demo <- altai_demo %>%
  filter(!grepl("It doesn't exist", PARTID)) %>%
  select(-NAME, -DATE, -TIME, -SESSION, -...30)

# Check data against ERM and make sure named and coded the same
altai_demo <- altai_demo %>%
  rename(CERCID = PARTID, SITE_SPEC = SITE) %>%
  mutate(SITE = "ALTAI URIANKHAI") %>%
  mutate(RESEARCHER = "MATEFFY") %>%
  relocate(CERCID, RESEARCHER, SITE, SITE_SPEC, .before = LOCATION)

# Tidy some of the data
altai_demo <- altai_demo %>%
  mutate(FAMILY = ifelse(FAMILY == "1 [?] 4 [?]", "4", FAMILY)) %>%
  mutate(FAMILY = as.numeric(FAMILY)) %>%
  mutate(HOWLONG = ifelse(HOWLONG == "[45]", "45", HOWLONG)) %>%
  mutate(HOWLONG = as.numeric(HOWLONG)) %>%
  mutate(GROWUP = ifelse(GROWUP == "[0] NA", "0", GROWUP)) %>%
  mutate(GROWUP = ifelse(GROWUP == "1;0", "2", GROWUP)) %>%
  mutate(GROWUP = as.numeric(GROWUP)) %>%
  mutate(CITYYRS = ifelse(CITYYRS == "[?]", "NA", 
                          ifelse(CITYYRS == "[0]" | CITYYRS == "0 [?]", "0", 
                                 ifelse(CITYYRS == "4 [?]", "4", CITYYRS)))) %>%
  mutate(CITYYRS = as.numeric(CITYYRS)) %>%
  mutate(CHILDREN = ifelse(CHILDREN == "2 + 4", "6", CHILDREN)) %>%
  mutate(CHILDREN = as.numeric(CHILDREN)) %>%
  mutate(FORMALED = as.numeric(FORMALED)) %>%
  mutate(HOUSEHOLD = as.numeric(HOUSEHOLD)) %>%
  mutate(NATLANG = as.numeric(NATLANG)) %>%
  mutate(MAT1 = as.numeric(MAT1)) %>%
  mutate(MAT1C = as.numeric(MAT1C)) %>%
  mutate(MAT2 = as.numeric(MAT2)) %>%
  mutate(MAT2C = as.numeric(MAT2C)) %>%
  mutate(MAT3 = as.numeric(MAT3)) %>%
  mutate(MAT3C = as.numeric(MAT3C)) %>%
  mutate(MAT4 = as.numeric(MAT4)) %>%
  mutate(MAT4C = as.numeric(MAT4C)) 

# Make the derived material security variables
altai_demo <- altai_demo %>%
  mutate(MMAT = rowMeans(select(., MAT1, MAT2, MAT3, MAT4), na.rm = TRUE)) %>%
  mutate(MMATc = rowMeans(select(., MAT1C, MAT2C, MAT3C, MAT4C), na.rm = TRUE))


## Read in the game data
altai_game <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Mateffy/Data/GGSL-Experimental-Activities-AUD001-AUD099.xlsx",
                         sheet = "Data",
                         skip = 1)

head(altai_game)
glimpse(altai_game)

# Remove some empty variables, post-game text data, and unused IDs
altai_game <- altai_game %>%
  filter(!grepl("nobody drew it", ID)) %>%
  select(-SITE, -DATE, -TIME, -LOCATION, -SESSNUM, -GA1, -GA2, -`Post-EXP/GA1`, -`Post-EXP/GA2`, -...37, -...38) %>%
  rename(CERCID = ID, INTERVIEWER_GAMES = INTERVIEWER)

# Check the data and edit to match ERM names and coding
altai_game <- altai_game %>%
  rename(ORDER = ORDERRAG) %>%
  mutate(ORDER = ifelse(ORDER == "[LS]", "LS", 
                        ifelse(ORDER == "[SL]", "SL", ORDER))) %>%
  mutate(INGFIRST = ifelse(ORDER == "LS", 1, 0)) %>%
  relocate(INGFIRST, .after = ORDER) %>%
  mutate(PASSED1 = ifelse(PASSED1 == "YES", "1", "0")) %>%
  mutate(PASSED1 = as.numeric(PASSED1)) %>%
  rename(PRE1_1 = '1P1', PRE1_2 = '1P2', PRE1_3 = '1P3', PRE1_4 = '1P4', PRE1_5 = '1P5',
         PRE2_1 = '2P1', PRE2_2 = '2P2', PRE2_3 = '2P3', PRE2_4 = '2P4', PRE2_5 = '2P5') %>%
  mutate(PRE1_1 = ifelse(PRE1_1 == "[1]", "1", PRE1_1)) %>% # '[1]' means 'inferred yes'
  mutate(PRE1_1 = as.numeric(PRE1_1)) %>%
  mutate(PRE1_2 = as.numeric(PRE1_2)) %>%
  mutate(PRE1_3 = as.numeric(PRE1_3)) %>%
  mutate(PRE1_4 = as.numeric(PRE1_4)) %>%
  mutate(PRE1_5 = ifelse(PRE1_5 == "[1]", "1", PRE1_5)) %>% # '[1]' means 'inferred yes'
  mutate(PRE1_5 = as.numeric(PRE1_5)) %>%
  mutate(EXUSED1 = ifelse(EXUSED1 == "[1]", "1", EXUSED1)) %>% # '[1]' means 'inferred yes'
  mutate(EXUSED1 = as.numeric(EXUSED1)) %>%
  mutate(PASSED2 = ifelse(PASSED2 == "YES", "1", "0")) %>%
  mutate(PASSED2 = as.numeric(PASSED2)) %>%
  mutate(PRE2_1 = ifelse(PRE2_1 == "[1]", "1", PRE2_1)) %>% # '[1]' means 'inferred yes'
  mutate(PRE2_1 = as.numeric(PRE2_1)) %>%
  mutate(PRE2_2 = ifelse(PRE2_2 == "[1]", "1", PRE2_2)) %>% # '[1]' means 'inferred yes'
  mutate(PRE2_2 = as.numeric(PRE2_2)) %>%
  mutate(PRE2_3 = ifelse(PRE2_3 == "[1]", "1", PRE2_3)) %>% # '[1]' means 'inferred yes'
  mutate(PRE2_3 = as.numeric(PRE2_3)) %>%
  mutate(PRE2_4 = ifelse(PRE2_4 == "[1]", "1", PRE2_4)) %>% # '[1]' means 'inferred yes'
  mutate(PRE2_4 = as.numeric(PRE2_4)) %>%
  mutate(PRE2_5 = ifelse(PRE2_5 == "[1]", "1", PRE2_5)) %>% # '[1]' means 'inferred yes'
  mutate(PRE2_5 = as.numeric(PRE1_5)) %>%
  mutate(EXUSED2 = as.numeric(EXUSED2)) %>%
  rename(COREL.L = DISTANT...18, INGROUP = LOCAL...17, COREL.S = DISTANT...27, SELF = SELF...26) %>%
  mutate(SUM1 = COREL.L + INGROUP) %>%
  mutate(SUM2 = COREL.S + SELF) %>%
  relocate(COREL.L, INGROUP, SUM1, .after = EXUSED1) %>%
  relocate(COREL.S, SELF, SUM2, .after = EXUSED2) %>%
  mutate(ORDERDG = ifelse(ORDERDG == "[SL]", "SL", ORDERDG)) %>%
  mutate(INGFIRST_DG = ifelse(ORDERDG == "LS", 1, 0)) %>%
  relocate(INGFIRST_DG, .after = ORDERDG) %>%
  rename(COREL.L_DG = DISTANT...30, INGROUP_DG = LOCAL...29, COREL.S_DG = DISTANT...32, SELF_DG = SELF...31) %>%
  mutate(SUM1_DG = COREL.L_DG + INGROUP_DG) %>%
  mutate(SUM2_DG = COREL.S_DG + SELF_DG) %>%
  relocate(COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG, .after = INGFIRST_DG)


# Check if any RAG totals do not sum to 30
altai_game %>%
  filter(SUM1 != 30 | SUM2 != 30) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L, INGROUP, SUM1, COREL.S, SELF, SUM2)

# Two people are '0' for both games, both of whom did not pass the tests so presumably did not play the games. Will code as NA
altai_game <- altai_game %>%
  mutate(COREL.L = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, COREL.L)) %>%
  mutate(INGROUP = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, INGROUP)) %>%
  mutate(SUM1 = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, SUM1)) %>%
  mutate(COREL.S = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, COREL.S)) %>%
  mutate(SELF = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, SELF)) %>%
  mutate(SUM2 = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, SUM2))

# Check if any DG totals do not sum to 10
altai_game %>%
  filter(SUM1_DG != 10 | SUM2_DG != 10) %>%
  select(CERCID, PASSED1, PASSED2, COREL.L_DG, INGROUP_DG, SUM1_DG, COREL.S_DG, SELF_DG, SUM2_DG)

# Just the two folks who did not play the RAG either. Will code as NA
altai_game <- altai_game %>%
  mutate(COREL.L_DG = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, COREL.L_DG)) %>%
  mutate(INGROUP_DG = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, INGROUP_DG)) %>%
  mutate(SUM1_DG = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, SUM1_DG)) %>%
  mutate(COREL.S_DG = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, COREL.S_DG)) %>%
  mutate(SELF_DG = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, SELF_DG)) %>%
  mutate(SUM2_DG = ifelse(CERCID == "AUD003" | CERCID == "AUD049", NA, SUM2_DG))


### Now for the religiosity interview data
altai_relig <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Mongolia - Mateffy/Data/GGSL-Religiosity-Interview-AUD001-AUD099_revised_DMS.xlsx",
                          sheet = "RELIGIOSITY")

head(altai_relig)
glimpse(altai_relig)


# Remove some empty variables and unused IDs
altai_relig <- altai_relig %>%
  filter(!grepl("It doesn't exist", PARTID)) %>%
  filter(PARTID != "AUD049") %>%
  select(-c(DATE, TIME, LOCATION, NAME, PARTCR1:TIMEDR, BGM1:POM4)) %>%
  rename(CERCID = PARTID, INTERVIEWER_RELIG = INTERVIEWER)

# Check the data and edit to match ERM names and coding - As lots of recoding, will go variable by variable

# BGTHINK
altai_relig <- altai_relig %>%
  mutate(BGLG1ST = as.numeric(BGLG1ST)) %>%
  mutate(BGTHINK_INFERRED = BGTHINK) %>%
  mutate(BGTHINK = ifelse(BGTHINK == "[0]" | BGTHINK == "[1]" | BGTHINK == "[2]" | BGTHINK == "[3]" | BGTHINK == "[4]", "NA", BGTHINK)) %>%
  mutate(BGTHINK_INFERRED = ifelse(BGTHINK_INFERRED == "[0]", "0",
                                   ifelse(BGTHINK_INFERRED == "[1]", "1",
                                          ifelse(BGTHINK_INFERRED == "[2]", "2",
                                                 ifelse(BGTHINK_INFERRED == "[3]", "3",
                                                        ifelse(BGTHINK_INFERRED == "[4]", "4", BGTHINK_INFERRED)))))) %>%
  mutate(BGTHINK = as.numeric(BGTHINK)) %>%
  mutate(BGTHINK_INFERRED = as.numeric(BGTHINK_INFERRED)) %>%
  mutate(BGTHINK_BINARY = ifelse(is.na(BGTHINK_INFERRED), NA,
                                 ifelse(BGTHINK_INFERRED == 0, 0, 1))) %>%
  relocate(BGTHINK_INFERRED, BGTHINK_BINARY, .after = BGTHINK)

# BGPERF
altai_relig <- altai_relig %>%
  mutate(BGPERF_INFERRED = BGPERF) %>%
  mutate(BGPERF = ifelse(BGPERF == "[1]", "NA", BGPERF)) %>%
  mutate(BGPERF_INFERRED = ifelse(BGPERF_INFERRED == "[1]", "1", BGPERF_INFERRED)) %>%
  mutate(BGPERF = as.numeric(BGPERF)) %>%
  mutate(BGPERF_INFERRED = as.numeric(BGPERF_INFERRED)) %>%
  relocate(BGPERF_INFERRED, .after = BGPERF)

# BGPERFHO
altai_relig <- altai_relig %>%
  mutate(BGPERFHO_INFERRED = BGPERFHO) %>%
  mutate(BGPERFHO = ifelse(BGPERFHO == "[1]" | BGPERFHO == "[2]" | BGPERFHO == "[4]" | BGPERFHO == "[No]", "NA", BGPERFHO)) %>%
  mutate(BGPERFHO_INFERRED = ifelse(BGPERFHO_INFERRED == "[No]", "NA",
                                    ifelse(BGPERFHO_INFERRED == "[1]", "1",
                                           ifelse(BGPERFHO_INFERRED == "[2]", "2",
                                                  ifelse(BGPERFHO_INFERRED == "[4]", "4", BGPERFHO_INFERRED))))) %>%
  mutate(BGPERFHO = as.numeric(BGPERFHO)) %>%
  mutate(BGPERFHO_INFERRED = as.numeric(BGPERFHO_INFERRED)) %>%
  mutate(BGPERFHO_BINARY = ifelse(is.na(BGPERFHO_INFERRED), NA,
                                  ifelse(BGPERFHO_INFERRED == 0, 0, 1))) %>%
  relocate(BGPERFHO_INFERRED, BGPERFHO_BINARY, .after = BGPERFHO)

table(altai_relig$BGPERF, altai_relig$BGPERFHO, useNA = "ifany")
table(altai_relig$BGPERF, altai_relig$BGPERFHO_INFERRED, useNA = "ifany")

# BGFREQW
altai_relig <- altai_relig %>%
  mutate(BGFREQW_INFERRED = BGFREQW) %>%
  mutate(BGFREQW = ifelse(BGFREQW == "[0]" | BGFREQW == "[1]" | BGFREQW == "[2]" | BGFREQW == "[3]" | BGFREQW == "[4]", "NA", BGFREQW)) %>%
  mutate(BGFREQW_INFERRED = ifelse(BGFREQW_INFERRED == "[0]", "0",
                                   ifelse(BGFREQW_INFERRED == "[1]", "1",
                                          ifelse(BGFREQW_INFERRED == "[2]", "2",
                                                 ifelse(BGFREQW_INFERRED == "[3]", "3",
                                                        ifelse(BGFREQW_INFERRED == "[4]", "4", BGFREQW_INFERRED)))))) %>%
  mutate(BGFREQW = as.numeric(BGFREQW)) %>%
  mutate(BGFREQW_INFERRED = as.numeric(BGFREQW_INFERRED)) %>%
  mutate(BGFREQW_BINARY = ifelse(is.na(BGFREQW_INFERRED), NA,
                                 ifelse(BGFREQW_INFERRED == 0, 0, 1))) %>%
  relocate(BGFREQW_INFERRED, BGFREQW_BINARY, .after = BGFREQW)

# BGPUNISH
altai_relig <- altai_relig %>%
  mutate(BGPUNISH_INFERRED = BGPUNISH) %>%
  mutate(BGPUNISH = ifelse(BGPUNISH == "[1]" | BGPUNISH == "[0]", "NA", BGPUNISH)) %>%
  mutate(BGPUNISH_INFERRED = ifelse(BGPUNISH_INFERRED == "[1]", "1", 
                                    ifelse(BGPUNISH == "[0]", "0", BGPUNISH_INFERRED))) %>%
  mutate(BGPUNISH = as.numeric(BGPUNISH)) %>%
  mutate(BGPUNISH_INFERRED = as.numeric(BGPUNISH_INFERRED)) %>%
  relocate(BGPUNISH_INFERRED, .after = BGPUNISH)

# BGSTEAL
altai_relig <- altai_relig %>%
  mutate(BGSTEAL_INFERRED = BGSTEAL) %>%
  mutate(BGSTEAL = ifelse(BGSTEAL == "[0]" | BGSTEAL == "[1]" | BGSTEAL == "[2]" | BGSTEAL == "[3]" | BGSTEAL == "[4]", "NA", BGSTEAL)) %>%
  mutate(BGSTEAL_INFERRED = ifelse(BGSTEAL_INFERRED == "[0]", "0",
                                   ifelse(BGSTEAL_INFERRED == "[1]", "1",
                                          ifelse(BGSTEAL_INFERRED == "[2]", "2",
                                                 ifelse(BGSTEAL_INFERRED == "[3]", "3",
                                                        ifelse(BGSTEAL_INFERRED == "[4]", "4", BGSTEAL_INFERRED)))))) %>%
  mutate(BGSTEAL = as.numeric(BGSTEAL)) %>%
  mutate(BGSTEAL_INFERRED = as.numeric(BGSTEAL_INFERRED)) %>%
  mutate(BGSTEAL_BINARY = ifelse(is.na(BGSTEAL_INFERRED), NA,
                                 ifelse(BGSTEAL_INFERRED == 0, 0, 1))) %>%
  relocate(BGSTEAL_INFERRED, BGSTEAL_BINARY, .after = BGSTEAL)

# BGSTLIMP
altai_relig <- altai_relig %>%
  mutate(BGSTLIMP_INFERRED = BGSTLIMP) %>%
  mutate(BGSTLIMP = ifelse(BGSTLIMP == "[0]" | BGSTLIMP == "[1]" | BGSTLIMP == "[2]", "NA", BGSTLIMP)) %>%
  mutate(BGSTLIMP_INFERRED = ifelse(BGSTLIMP_INFERRED == "[0]", "0",
                                    ifelse(BGSTLIMP_INFERRED == "[1]", "1",
                                           ifelse(BGSTLIMP_INFERRED == "[2]", "2", BGSTLIMP_INFERRED)))) %>%
  mutate(BGSTLIMP = as.numeric(BGSTLIMP)) %>%
  mutate(BGSTLIMP_INFERRED = as.numeric(BGSTLIMP_INFERRED)) %>%
  mutate(BGSTLIMP_BINARY = ifelse(is.na(BGSTLIMP_INFERRED), NA,
                                  ifelse(BGSTLIMP_INFERRED == 0, 0, 1))) %>%
  relocate(BGSTLIMP_INFERRED, BGSTLIMP_BINARY, .after = BGSTLIMP)

# BGLYING
altai_relig <- altai_relig %>%
  mutate(BGLYING_INFERRED = BGLYING) %>%
  mutate(BGLYING = ifelse(BGLYING == "[0]" | BGLYING == "[1]" | BGLYING == "[2]" | BGLYING == "[3]" | BGLYING == "[4]", "NA", BGLYING)) %>%
  mutate(BGLYING_INFERRED = ifelse(BGLYING_INFERRED == "[0]", "0",
                                   ifelse(BGLYING_INFERRED == "[1]", "1",
                                          ifelse(BGLYING_INFERRED == "[2]", "2",
                                                 ifelse(BGLYING_INFERRED == "[3]", "3",
                                                        ifelse(BGLYING_INFERRED == "[4]", "4", BGLYING_INFERRED)))))) %>%
  mutate(BGLYING = as.numeric(BGLYING)) %>%
  mutate(BGLYING_INFERRED = as.numeric(BGLYING_INFERRED)) %>%
  mutate(BGLYING_BINARY = ifelse(is.na(BGLYING_INFERRED), NA,
                                 ifelse(BGLYING_INFERRED == 0, 0, 1))) %>%
  relocate(BGLYING_INFERRED, BGLYING_BINARY, .after = BGLYING)

# BGLIEIMP
altai_relig <- altai_relig %>%
  mutate(BGLIEIMP_INFERRED = BGLIEIMP) %>%
  mutate(BGLIEIMP = ifelse(BGLIEIMP == "[0]" | BGLIEIMP == "[1]" | BGLIEIMP == "[2]", "NA", BGLIEIMP)) %>%
  mutate(BGLIEIMP_INFERRED = ifelse(BGLIEIMP_INFERRED == "[0]", "0",
                                    ifelse(BGLIEIMP_INFERRED == "[1]", "1",
                                           ifelse(BGLIEIMP_INFERRED == "[2]", "2", BGLIEIMP_INFERRED)))) %>%
  mutate(BGLIEIMP = as.numeric(BGLIEIMP)) %>%
  mutate(BGLIEIMP_INFERRED = as.numeric(BGLIEIMP_INFERRED)) %>%
  mutate(BGLIEIMP_BINARY = ifelse(is.na(BGLIEIMP_INFERRED), NA,
                                  ifelse(BGLIEIMP_INFERRED == 0, 0, 1))) %>%
  relocate(BGLIEIMP_INFERRED, BGLIEIMP_BINARY, .after = BGLIEIMP)

# BGMURDER
altai_relig <- altai_relig %>%
  mutate(BGMURDER_INFERRED = BGMURDER) %>%
  mutate(BGMURDER = ifelse(BGMURDER == "[0]" | BGMURDER == "[1]" | BGMURDER == "[2]" | BGMURDER == "[3]" | BGMURDER == "[4]", "NA", BGMURDER)) %>%
  mutate(BGMURDER_INFERRED = ifelse(BGMURDER_INFERRED == "[0]", "0",
                                    ifelse(BGMURDER_INFERRED == "[1]", "1",
                                           ifelse(BGMURDER_INFERRED == "[2]", "2",
                                                  ifelse(BGMURDER_INFERRED == "[3]", "3",
                                                         ifelse(BGMURDER_INFERRED == "[4]", "4", BGMURDER_INFERRED)))))) %>%
  mutate(BGMURDER = as.numeric(BGMURDER)) %>%
  mutate(BGMURDER_INFERRED = as.numeric(BGMURDER_INFERRED)) %>%
  mutate(BGMURDER_BINARY = ifelse(is.na(BGMURDER_INFERRED), NA,
                                  ifelse(BGMURDER_INFERRED == 0, 0, 1))) %>%
  relocate(BGMURDER_INFERRED, BGMURDER_BINARY, .after = BGMURDER)

# BGMURDIMP
altai_relig <- altai_relig %>%
  mutate(BGMURDIMP_INFERRED = BGMURDIMP) %>%
  mutate(BGMURDIMP = ifelse(BGMURDIMP == "[0]" | BGMURDIMP == "[1]" | BGMURDIMP == "[2]" | BGMURDIMP == "[3]" | BGMURDIMP == "[4]", "NA", BGMURDIMP)) %>%
  mutate(BGMURDIMP_INFERRED = ifelse(BGMURDIMP_INFERRED == "[0]", "0",
                                     ifelse(BGMURDIMP_INFERRED == "[1]", "1",
                                            ifelse(BGMURDIMP_INFERRED == "[2]", "2",
                                                   ifelse(BGMURDIMP_INFERRED == "[3]", "3",
                                                          ifelse(BGMURDIMP_INFERRED == "[4]", "4", BGMURDIMP_INFERRED)))))) %>%
  mutate(BGMURDIMP = as.numeric(BGMURDIMP)) %>%
  mutate(BGMURDIMP_INFERRED = as.numeric(BGMURDIMP_INFERRED)) %>%
  mutate(BGMURDIMP_BINARY = ifelse(is.na(BGMURDIMP_INFERRED), NA,
                                   ifelse(BGMURDIMP_INFERRED == 0, 0, 1))) %>%
  relocate(BGMURDIMP_INFERRED, BGMURDIMP_BINARY, .after = BGMURDIMP)

# BGFEEL
altai_relig <- altai_relig %>%
  mutate(BGFEEL_INFERRED = BGFEEL) %>%
  mutate(BGFEEL = ifelse(BGFEEL == "[1]", "NA", BGFEEL)) %>%
  mutate(BGFEEL_INFERRED = ifelse(BGFEEL_INFERRED == "[1]", "1", BGFEEL_INFERRED)) %>%
  mutate(BGFEEL = as.numeric(BGFEEL)) %>%
  mutate(BGFEEL_INFERRED = as.numeric(BGFEEL_INFERRED)) %>%
  relocate(BGFEEL_INFERRED, .after = BGFEEL)

# BGSEE
altai_relig <- altai_relig %>%
  mutate(BGSEE = as.numeric(BGSEE))

# BGREWARD
altai_relig <- altai_relig %>%
  mutate(BGREWARD_INFERRED = BGREWARD) %>%
  mutate(BGREWARD = ifelse(BGREWARD == "[0]" | BGREWARD == "[1]" | BGREWARD == "[2]" | BGREWARD == "[3]" | BGREWARD == "[4]", "NA", BGREWARD)) %>%
  mutate(BGREWARD_INFERRED = ifelse(BGREWARD_INFERRED == "[0]", "0",
                                    ifelse(BGREWARD_INFERRED == "[1]", "1",
                                           ifelse(BGREWARD_INFERRED == "[2]", "2",
                                                  ifelse(BGREWARD_INFERRED == "[3]", "3",
                                                         ifelse(BGREWARD_INFERRED == "[4]", "4", BGREWARD_INFERRED)))))) %>%
  mutate(BGREWARD = as.numeric(BGREWARD)) %>%
  mutate(BGREWARD_INFERRED = as.numeric(BGREWARD_INFERRED)) %>%
  mutate(BGREWARD_BINARY = ifelse(is.na(BGREWARD_INFERRED), NA,
                                  ifelse(BGREWARD_INFERRED == 0, 0, 1))) %>%
  relocate(BGREWARD_INFERRED, BGREWARD_BINARY, .after = BGREWARD)

# BGDIE
altai_relig <- altai_relig %>%
  mutate(BGDIE_INFERRED = BGDIE) %>%
  mutate(BGDIE = ifelse(BGDIE == "[1]", "NA", BGDIE)) %>%
  mutate(BGDIE_INFERRED = ifelse(BGDIE_INFERRED == "[1]", "1", BGDIE_INFERRED)) %>%
  mutate(BGDIE = as.numeric(BGDIE)) %>%
  mutate(BGDIE_INFERRED = as.numeric(BGDIE_INFERRED)) %>%
  relocate(BGDIE_INFERRED, .after = BGDIE)

# BGSTRANGER
altai_relig <- altai_relig %>%
  mutate(BGSTRANGER_INFERRED = BGSTRANGER) %>%
  mutate(BGSTRANGER = ifelse(BGSTRANGER == "[1]", "NA", BGSTRANGER)) %>%
  mutate(BGSTRANGER_INFERRED = ifelse(BGSTRANGER_INFERRED == "[1]", "1", BGSTRANGER_INFERRED)) %>%
  mutate(BGSTRANGER = as.numeric(BGSTRANGER)) %>%
  mutate(BGSTRANGER_INFERRED = as.numeric(BGSTRANGER_INFERRED)) %>%
  relocate(BGSTRANGER_INFERRED, .after = BGSTRANGER)

# BGOTHERRIT
altai_relig <- altai_relig %>%
  mutate(BGOTHERRIT_INFERRED = BGOTHERRIT) %>%
  mutate(BGOTHERRIT = ifelse(BGOTHERRIT == "[1]", "NA", BGOTHERRIT)) %>%
  mutate(BGOTHERRIT_INFERRED = ifelse(BGOTHERRIT_INFERRED == "[1]", "1", BGOTHERRIT_INFERRED)) %>%
  mutate(BGOTHERRIT = as.numeric(BGOTHERRIT)) %>%
  mutate(BGOTHERRIT_INFERRED = as.numeric(BGOTHERRIT_INFERRED)) %>%
  relocate(BGOTHERRIT_INFERRED, .after = BGOTHERRIT)

# BGPERFC
altai_relig <- altai_relig %>%
  mutate(BGPERFC_INFERRED = BGPERFC) %>%
  mutate(BGPERFC = ifelse(BGPERFC == "[1]", "NA", BGPERFC)) %>%
  mutate(BGPERFC_INFERRED = ifelse(BGPERFC_INFERRED == "[1]", "1", BGPERFC_INFERRED)) %>%
  mutate(BGPERFC = as.numeric(BGPERFC)) %>%
  mutate(BGPERFC_INFERRED = as.numeric(BGPERFC_INFERRED)) %>%
  relocate(BGPERFC_INFERRED, .after = BGPERFC)


# LGTHINK
altai_relig <- altai_relig %>%
  mutate(LGTHINK_INFERRED = LGTHINK) %>%
  mutate(LGTHINK = ifelse(LGTHINK == "[0]" | LGTHINK == "[1]" | LGTHINK == "[2]" | LGTHINK == "[3]" | LGTHINK == "[4]", "NA", LGTHINK)) %>%
  mutate(LGTHINK_INFERRED = ifelse(LGTHINK_INFERRED == "[0]", "0",
                                   ifelse(LGTHINK_INFERRED == "[1]", "1",
                                          ifelse(LGTHINK_INFERRED == "[2]", "2",
                                                 ifelse(LGTHINK_INFERRED == "[3]", "3",
                                                        ifelse(LGTHINK_INFERRED == "[4]", "4", LGTHINK_INFERRED)))))) %>%
  mutate(LGTHINK = as.numeric(LGTHINK)) %>%
  mutate(LGTHINK_INFERRED = as.numeric(LGTHINK_INFERRED)) %>%
  mutate(LGTHINK_BINARY = ifelse(is.na(LGTHINK_INFERRED), NA,
                                 ifelse(LGTHINK_INFERRED == 0, 0, 1))) %>%
  relocate(LGTHINK_INFERRED, LGTHINK_BINARY, .after = LGTHINK)

# LGPERF
altai_relig <- altai_relig %>%
  mutate(LGPERF = as.numeric(LGPERF))

# LGPERFHO
altai_relig <- altai_relig %>%
  mutate(LGPERFHO_INFERRED = LGPERFHO) %>%
  mutate(LGPERFHO = ifelse(LGPERFHO == "[1]" | LGPERFHO == "[No]", "NA", LGPERFHO)) %>%
  mutate(LGPERFHO_INFERRED = ifelse(LGPERFHO_INFERRED == "[No]", "NA",
                                    ifelse(LGPERFHO_INFERRED == "[1]", "1", LGPERFHO_INFERRED))) %>%
  mutate(LGPERFHO = as.numeric(LGPERFHO)) %>%
  mutate(LGPERFHO_INFERRED = as.numeric(LGPERFHO_INFERRED)) %>%
  mutate(LGPERFHO_BINARY = ifelse(is.na(LGPERFHO_INFERRED), NA,
                                  ifelse(LGPERFHO_INFERRED == 0, 0, 1))) %>%
  relocate(LGPERFHO_INFERRED, LGPERFHO_BINARY, .after = LGPERFHO)

table(altai_relig$LGPERF, altai_relig$LGPERFHO, useNA = "ifany")
table(altai_relig$LGPERF, altai_relig$LGPERFHO_INFERRED, useNA = "ifany")

# LGFREQW
altai_relig <- altai_relig %>%
  mutate(LGFREQW_INFERRED = LGFREQW) %>%
  mutate(LGFREQW = ifelse(LGFREQW == "[0]" | LGFREQW == "[1]" | LGFREQW == "[2]" | LGFREQW == "[3]" | LGFREQW == "[4]", "NA", LGFREQW)) %>%
  mutate(LGFREQW_INFERRED = ifelse(LGFREQW_INFERRED == "[0]", "0",
                                   ifelse(LGFREQW_INFERRED == "[1]", "1",
                                          ifelse(LGFREQW_INFERRED == "[2]", "2",
                                                 ifelse(LGFREQW_INFERRED == "[3]", "3",
                                                        ifelse(LGFREQW_INFERRED == "[4]", "4", LGFREQW_INFERRED)))))) %>%
  mutate(LGFREQW = as.numeric(LGFREQW)) %>%
  mutate(LGFREQW_INFERRED = as.numeric(LGFREQW_INFERRED)) %>%
  mutate(LGFREQW_BINARY = ifelse(is.na(LGFREQW_INFERRED), NA,
                                 ifelse(LGFREQW_INFERRED == 0, 0, 1))) %>%
  relocate(LGFREQW_INFERRED, LGFREQW_BINARY, .after = LGFREQW)

# LGPUNISH
altai_relig <- altai_relig %>%
  mutate(LGPUNISH = as.numeric(LGPUNISH))

# LGSTEAL
altai_relig <- altai_relig %>%
  mutate(LGSTEAL_INFERRED = LGSTEAL) %>%
  mutate(LGSTEAL = ifelse(LGSTEAL == "[0]" | LGSTEAL == "[1]" | LGSTEAL == "[2]" | LGSTEAL == "[3]" | LGSTEAL == "[4]", "NA", LGSTEAL)) %>%
  mutate(LGSTEAL_INFERRED = ifelse(LGSTEAL_INFERRED == "[0]", "0",
                                   ifelse(LGSTEAL_INFERRED == "[1]", "1",
                                          ifelse(LGSTEAL_INFERRED == "[2]", "2",
                                                 ifelse(LGSTEAL_INFERRED == "[3]", "3",
                                                        ifelse(LGSTEAL_INFERRED == "[4]", "4", LGSTEAL_INFERRED)))))) %>%
  mutate(LGSTEAL = as.numeric(LGSTEAL)) %>%
  mutate(LGSTEAL_INFERRED = as.numeric(LGSTEAL_INFERRED)) %>%
  mutate(LGSTEAL_BINARY = ifelse(is.na(LGSTEAL_INFERRED), NA,
                                 ifelse(LGSTEAL_INFERRED == 0, 0, 1))) %>%
  relocate(LGSTEAL_INFERRED, LGSTEAL_BINARY, .after = LGSTEAL)

# LGSTEALIMP
altai_relig <- altai_relig %>%
  mutate(LGSTEALIMP_INFERRED = LGSTEALIMP) %>%
  mutate(LGSTEALIMP = ifelse(LGSTEALIMP == "[0]" | LGSTEALIMP == "[1]" | LGSTEALIMP == "[2]" | LGSTEALIMP == "[3]" | LGSTEALIMP == "[4]", 
                             "NA", LGSTEALIMP)) %>%
  mutate(LGSTEALIMP_INFERRED = ifelse(LGSTEALIMP_INFERRED == "[0]", "0",
                                      ifelse(LGSTEALIMP_INFERRED == "[1]", "1",
                                             ifelse(LGSTEALIMP_INFERRED == "[2]", "2",
                                                    ifelse(LGSTEALIMP_INFERRED == "[3]", "3",
                                                           ifelse(LGSTEALIMP_INFERRED == "[4]", "4", LGSTEALIMP_INFERRED)))))) %>%
  mutate(LGSTEALIMP = as.numeric(LGSTEALIMP)) %>%
  mutate(LGSTEALIMP_INFERRED = as.numeric(LGSTEALIMP_INFERRED)) %>%
  mutate(LGSTEALIMP_BINARY = ifelse(is.na(LGSTEALIMP_INFERRED), NA,
                                    ifelse(LGSTEALIMP_INFERRED == 0, 0, 1))) %>%
  relocate(LGSTEALIMP_INFERRED, LGSTEALIMP_BINARY, .after = LGSTEALIMP)

# LGLYING
altai_relig <- altai_relig %>%
  mutate(LGLYING_INFERRED = LGLYING) %>%
  mutate(LGLYING = ifelse(LGLYING == "[0]" | LGLYING == "[1]" | LGLYING == "[2]" | LGLYING == "[3]" | LGLYING == "[4]", "NA", LGLYING)) %>%
  mutate(LGLYING_INFERRED = ifelse(LGLYING_INFERRED == "[0]", "0",
                                   ifelse(LGLYING_INFERRED == "[1]", "1",
                                          ifelse(LGLYING_INFERRED == "[2]", "2",
                                                 ifelse(LGLYING_INFERRED == "[3]", "3",
                                                        ifelse(LGLYING_INFERRED == "[4]", "4", LGLYING_INFERRED)))))) %>%
  mutate(LGLYING = as.numeric(LGLYING)) %>%
  mutate(LGLYING_INFERRED = as.numeric(LGLYING_INFERRED)) %>%
  mutate(LGLYING_BINARY = ifelse(is.na(LGLYING_INFERRED), NA,
                                 ifelse(LGLYING_INFERRED == 0, 0, 1))) %>%
  relocate(LGLYING_INFERRED, LGLYING_BINARY, .after = LGLYING)

# LGLIEIMP
altai_relig <- altai_relig %>%
  mutate(LGLIEIMP_INFERRED = LGLIEIMP) %>%
  mutate(LGLIEIMP = ifelse(LGLIEIMP == "[0]" | LGLIEIMP == "[1]" | LGLIEIMP == "[2]" | LGLIEIMP == "[3]", "NA", LGLIEIMP)) %>%
  mutate(LGLIEIMP_INFERRED = ifelse(LGLIEIMP_INFERRED == "[0]", "0",
                                    ifelse(LGLIEIMP_INFERRED == "[1]", "1",
                                           ifelse(LGLIEIMP_INFERRED == "[2]", "2", 
                                                  ifelse(LGLIEIMP_INFERRED == "[3]", "3", LGLIEIMP_INFERRED))))) %>%
  mutate(LGLIEIMP = as.numeric(LGLIEIMP)) %>%
  mutate(LGLIEIMP_INFERRED = as.numeric(LGLIEIMP_INFERRED)) %>%
  mutate(LGLIEIMP_BINARY = ifelse(is.na(LGLIEIMP_INFERRED), NA,
                                  ifelse(LGLIEIMP_INFERRED == 0, 0, 1))) %>%
  relocate(LGLIEIMP_INFERRED, LGLIEIMP_BINARY, .after = LGLIEIMP)

# LGMURDER
altai_relig <- altai_relig %>%
  mutate(LGMURDER_INFERRED = LGMURDER) %>%
  mutate(LGMURDER = ifelse(LGMURDER == "[0]" | LGMURDER == "[1]" | LGMURDER == "[2]" | LGMURDER == "[3]" | LGMURDER == "[4]", "NA", LGMURDER)) %>%
  mutate(LGMURDER_INFERRED = ifelse(LGMURDER_INFERRED == "[0]", "0",
                                    ifelse(LGMURDER_INFERRED == "[1]", "1",
                                           ifelse(LGMURDER_INFERRED == "[2]", "2",
                                                  ifelse(LGMURDER_INFERRED == "[3]", "3",
                                                         ifelse(LGMURDER_INFERRED == "[4]", "4", LGMURDER_INFERRED)))))) %>%
  mutate(LGMURDER = as.numeric(LGMURDER)) %>%
  mutate(LGMURDER_INFERRED = as.numeric(LGMURDER_INFERRED)) %>%
  mutate(LGMURDER_BINARY = ifelse(is.na(LGMURDER_INFERRED), NA,
                                  ifelse(LGMURDER_INFERRED == 0, 0, 1))) %>%
  relocate(LGMURDER_INFERRED, LGMURDER_BINARY, .after = LGMURDER)

# LGMURDIMP
altai_relig <- altai_relig %>%
  mutate(LGMURDIMP_INFERRED = LGMURDIMP) %>%
  mutate(LGMURDIMP = ifelse(LGMURDIMP == "[2]" | LGMURDIMP == "[3]" | LGMURDIMP == "[4]", "NA", LGMURDIMP)) %>%
  mutate(LGMURDIMP_INFERRED = ifelse(LGMURDIMP_INFERRED == "[2]", "2",
                                     ifelse(LGMURDIMP_INFERRED == "[3]", "3",
                                            ifelse(LGMURDIMP_INFERRED == "[4]", "4", LGMURDIMP_INFERRED)))) %>%
  mutate(LGMURDIMP = as.numeric(LGMURDIMP)) %>%
  mutate(LGMURDIMP_INFERRED = as.numeric(LGMURDIMP_INFERRED)) %>%
  mutate(LGMURDIMP_BINARY = ifelse(is.na(LGMURDIMP_INFERRED), NA,
                                   ifelse(LGMURDIMP_INFERRED == 0, 0, 1))) %>%
  relocate(LGMURDIMP_INFERRED, LGMURDIMP_BINARY, .after = LGMURDIMP)

# LGFEEL
altai_relig <- altai_relig %>%
  mutate(LGFEEL_INFERRED = LGFEEL) %>%
  mutate(LGFEEL = ifelse(LGFEEL == "[1]", "NA", LGFEEL)) %>%
  mutate(LGFEEL_INFERRED = ifelse(LGFEEL_INFERRED == "[1]", "1", LGFEEL_INFERRED)) %>%
  mutate(LGFEEL = as.numeric(LGFEEL)) %>%
  mutate(LGFEEL_INFERRED = as.numeric(LGFEEL_INFERRED)) %>%
  relocate(LGFEEL_INFERRED, .after = LGFEEL)

# LGSEE
altai_relig <- altai_relig %>%
  mutate(LGSEE_INFERRED = LGSEE) %>%
  mutate(LGSEE = ifelse(LGSEE == "[1]", "NA", LGSEE)) %>%
  mutate(LGSEE_INFERRED = ifelse(LGSEE_INFERRED == "[1]", "1", LGSEE_INFERRED)) %>%
  mutate(LGSEE = as.numeric(LGSEE)) %>%
  mutate(LGSEE_INFERRED = as.numeric(LGSEE_INFERRED)) %>%
  relocate(LGSEE_INFERRED, .after = LGSEE)

# LGREWARD
altai_relig <- altai_relig %>%
  mutate(LGREWARD_INFERRED = LGREWARD) %>%
  mutate(LGREWARD = ifelse(LGREWARD == "[0]" | LGREWARD == "[1]" | LGREWARD == "[2]" | LGREWARD == "[3]" | LGREWARD == "[4]", "NA", LGREWARD)) %>%
  mutate(LGREWARD_INFERRED = ifelse(LGREWARD_INFERRED == "[0]", "0",
                                    ifelse(LGREWARD_INFERRED == "[1]", "1",
                                           ifelse(LGREWARD_INFERRED == "[2]", "2",
                                                  ifelse(LGREWARD_INFERRED == "[3]", "3",
                                                         ifelse(LGREWARD_INFERRED == "[4]", "4", LGREWARD_INFERRED)))))) %>%
  mutate(LGREWARD = as.numeric(LGREWARD)) %>%
  mutate(LGREWARD_INFERRED = as.numeric(LGREWARD_INFERRED)) %>%
  mutate(LGREWARD_BINARY = ifelse(is.na(LGREWARD_INFERRED), NA,
                                  ifelse(LGREWARD_INFERRED == 0, 0, 1))) %>%
  relocate(LGREWARD_INFERRED, LGREWARD_BINARY, .after = LGREWARD)

# LGDIE
altai_relig <- altai_relig %>%
  mutate(LGDIE_INFERRED = LGDIE) %>%
  mutate(LGDIE = ifelse(LGDIE == "[1]", "NA", LGDIE)) %>%
  mutate(LGDIE_INFERRED = ifelse(LGDIE_INFERRED == "[1]", "1", LGDIE_INFERRED)) %>%
  mutate(LGDIE = as.numeric(LGDIE)) %>%
  mutate(LGDIE_INFERRED = as.numeric(LGDIE_INFERRED)) %>%
  relocate(LGDIE_INFERRED, .after = LGDIE)

# LGSTRANGER
altai_relig <- altai_relig %>%
  mutate(LGSTRANGER_INFERRED = LGSTRANGER) %>%
  mutate(LGSTRANGER = ifelse(LGSTRANGER == "[1]", "NA", LGSTRANGER)) %>%
  mutate(LGSTRANGER_INFERRED = ifelse(LGSTRANGER_INFERRED == "[1]", "1", LGSTRANGER_INFERRED)) %>%
  mutate(LGSTRANGER = as.numeric(LGSTRANGER)) %>%
  mutate(LGSTRANGER_INFERRED = as.numeric(LGSTRANGER_INFERRED)) %>%
  relocate(LGSTRANGER_INFERRED, .after = LGSTRANGER)

# LGOTHERRIT
altai_relig <- altai_relig %>%
  mutate(LGOTHERRIT_INFERRED = LGOTHERRIT) %>%
  mutate(LGOTHERRIT = ifelse(LGOTHERRIT == "[1]", "NA", LGOTHERRIT)) %>%
  mutate(LGOTHERRIT_INFERRED = ifelse(LGOTHERRIT_INFERRED == "[1]", "1", LGOTHERRIT_INFERRED)) %>%
  mutate(LGOTHERRIT = as.numeric(LGOTHERRIT)) %>%
  mutate(LGOTHERRIT_INFERRED = as.numeric(LGOTHERRIT_INFERRED)) %>%
  relocate(LGOTHERRIT_INFERRED, .after = LGOTHERRIT)

# LGPERFC
altai_relig <- altai_relig %>%
  mutate(LGPERFC = as.numeric(LGPERFC)) 


# POLEVAL
altai_relig <- altai_relig %>%
  mutate(POLEVAL_INFERRED = POLEVAL) %>%
  mutate(POLEVAL = ifelse(POLEVAL == "[0]" | POLEVAL == "[1]" | POLEVAL == "[2]" | POLEVAL == "[-1]" | POLEVAL == "[-2]", "NA", POLEVAL)) %>%
  mutate(POLEVAL_INFERRED = ifelse(POLEVAL_INFERRED == "[0]", "0",
                                   ifelse(POLEVAL_INFERRED == "[1]", "1",
                                          ifelse(POLEVAL_INFERRED == "[2]", "2",
                                                 ifelse(POLEVAL_INFERRED == "[-1]", "-1",
                                                        ifelse(POLEVAL_INFERRED == "[-2]", "-2", POLEVAL_INFERRED)))))) %>%
  mutate(POLEVAL = as.numeric(POLEVAL)) %>%
  mutate(POLEVAL_INFERRED = as.numeric(POLEVAL_INFERRED)) %>%
  relocate(POLEVAL_INFERRED, .after = POLEVAL)

# CORELEMO
altai_relig <- altai_relig %>%
  mutate(CORELEMO_INFERRED = CORELEMO) %>%
  mutate(CORELEMO = ifelse(CORELEMO == "[1]" | CORELEMO == "[2]" | CORELEMO == "[3]" | CORELEMO == "[4]" | CORELEMO == "[5]", "NA", CORELEMO)) %>%
  mutate(CORELEMO_INFERRED = ifelse(CORELEMO_INFERRED == "[1]", "1",
                                    ifelse(CORELEMO_INFERRED == "[2]", "2",
                                           ifelse(CORELEMO_INFERRED == "[3]", "3",
                                                  ifelse(CORELEMO_INFERRED == "[4]", "4",
                                                         ifelse(CORELEMO_INFERRED == "[5]", "5", CORELEMO_INFERRED)))))) %>%
  mutate(CORELEMO = as.numeric(CORELEMO)) %>%
  mutate(CORELEMO_INFERRED = as.numeric(CORELEMO_INFERRED)) %>%
  relocate(CORELEMO_INFERRED, .after = CORELEMO)

# INGREMO
altai_relig <- altai_relig %>%
  mutate(INGREMO_INFERRED = INGREMO) %>%
  mutate(INGREMO = ifelse(INGREMO == "[1]" | INGREMO == "[2]" | INGREMO == "[3]" | INGREMO == "[4]" | INGREMO == "[5]", "NA", INGREMO)) %>%
  mutate(INGREMO_INFERRED = ifelse(INGREMO_INFERRED == "[1]", "1",
                                   ifelse(INGREMO_INFERRED == "[2]", "2",
                                          ifelse(INGREMO_INFERRED == "[3]", "3",
                                                 ifelse(INGREMO_INFERRED == "[4]", "4",
                                                        ifelse(INGREMO_INFERRED == "[5]", "5", INGREMO_INFERRED)))))) %>%
  mutate(INGREMO = as.numeric(INGREMO)) %>%
  mutate(INGREMO_INFERRED = as.numeric(INGREMO_INFERRED)) %>%
  relocate(INGREMO_INFERRED, .after = INGREMO)

# OUTGREMO
altai_relig <- altai_relig %>%
  mutate(OUTGREMO_INFERRED = OUTGREMO) %>%
  mutate(OUTGREMO = ifelse(OUTGREMO == "[1]" | OUTGREMO == "[2]" | OUTGREMO == "[3]" | OUTGREMO == "[4]" | OUTGREMO == "[5]", "NA", OUTGREMO)) %>%
  mutate(OUTGREMO_INFERRED = ifelse(OUTGREMO_INFERRED == "[1]", "1",
                                    ifelse(OUTGREMO_INFERRED == "[2]", "2",
                                           ifelse(OUTGREMO_INFERRED == "[3]", "3",
                                                  ifelse(OUTGREMO_INFERRED == "[4]", "4",
                                                         ifelse(OUTGREMO_INFERRED == "[5]", "5", OUTGREMO_INFERRED)))))) %>%
  mutate(OUTGREMO = as.numeric(OUTGREMO)) %>%
  mutate(OUTGREMO_INFERRED = as.numeric(OUTGREMO_INFERRED)) %>%
  relocate(OUTGREMO_INFERRED, .after = OUTGREMO)

# CORELSIM - Are two versions of this variable. both seem broadly similar, although the second version has more codings of '0', which mean 'don't know' (rather than any actual measure of closeness...). Will process the second one, but all seems rather dodgy.
temp <- altai_relig %>%
  select(CERCID, CORELSIM...57, CORELSIM...89, CORELSIM_NOTE)

altai_relig <- altai_relig %>%
  rename(CORELSIM = CORELSIM...89) %>%
  select(-CORELSIM...57, -CORELSIM_NOTE) %>%
  mutate(CORELSIM_INFERRED = CORELSIM) %>%
  mutate(CORELSIM = ifelse(CORELSIM == "[-1]" | CORELSIM == "[0/-2]" | CORELSIM == "[1]", "NA", CORELSIM)) %>%
  mutate(CORELSIM_INFERRED = ifelse(CORELSIM_INFERRED == "[1]", "1",
                                    ifelse(CORELSIM_INFERRED == "[0/-2]", "NA",
                                           ifelse(CORELSIM_INFERRED == "[-1]", "-1", CORELSIM_INFERRED)))) %>%
  mutate(CORELSIM = as.numeric(CORELSIM)) %>%
  mutate(CORELSIM_INFERRED = as.numeric(CORELSIM_INFERRED)) %>%
  relocate(CORELSIM, CORELSIM_INFERRED, .after = OUTGREMO_INFERRED)


# PRAY
altai_relig <- altai_relig %>%
  mutate(PRAY_INFERRED = PRAY) %>%
  mutate(PRAY = ifelse(PRAY == "[0]" | PRAY == "[1]" | PRAY == "[2]" | PRAY == "[3]" | PRAY == "[4]", "NA", PRAY)) %>%
  mutate(PRAY_INFERRED = ifelse(PRAY_INFERRED == "[0]", "0",
                                ifelse(PRAY_INFERRED == "[1]", "1",
                                       ifelse(PRAY_INFERRED == "[2]", "2",
                                              ifelse(PRAY_INFERRED == "[3]", "3",
                                                     ifelse(PRAY_INFERRED == "[4]", "4", PRAY_INFERRED)))))) %>%
  mutate(PRAY = as.numeric(PRAY)) %>%
  mutate(PRAY_INFERRED = as.numeric(PRAY_INFERRED)) %>%
  mutate(PRAY_BINARY = ifelse(is.na(PRAY_INFERRED), NA,
                              ifelse(PRAY_INFERRED == 0, 0, 1))) %>%
  relocate(PRAY_INFERRED, PRAY_BINARY, .after = PRAY)

# BGRIT
altai_relig <- altai_relig %>%
  select(-BGRIT_NOTE) %>%
  mutate(BGRIT_INFERRED = BGRIT) %>%
  mutate(BGRIT = ifelse(BGRIT == "[0]" | BGRIT == "[1]", "NA", BGRIT)) %>%
  mutate(BGRIT_INFERRED = ifelse(BGRIT_INFERRED == "[0]", "0",
                                 ifelse(BGRIT_INFERRED == "[1]", "1", BGRIT_INFERRED))) %>%
  mutate(BGRIT = as.numeric(BGRIT)) %>%
  mutate(BGRIT_INFERRED = as.numeric(BGRIT_INFERRED)) %>%
  mutate(BGRIT_BINARY = ifelse(is.na(BGRIT_INFERRED), NA,
                               ifelse(BGRIT_INFERRED == 0, 0, 1))) %>%
  relocate(BGRIT_INFERRED, BGRIT_BINARY, .after = BGRIT)

# LGRIT
altai_relig <- altai_relig %>%
  select(-LGRIT_NOTE) %>%
  mutate(LGRIT_INFERRED = LGRIT) %>%
  mutate(LGRIT = ifelse(LGRIT == "[0]" | LGRIT == "[1]", "NA", LGRIT)) %>%
  mutate(LGRIT_INFERRED = ifelse(LGRIT_INFERRED == "[0]", "0",
                                 ifelse(LGRIT_INFERRED == "[1]", "1", LGRIT_INFERRED))) %>%
  mutate(LGRIT = as.numeric(LGRIT)) %>%
  mutate(LGRIT_INFERRED = as.numeric(LGRIT_INFERRED)) %>%
  mutate(LGRIT_BINARY = ifelse(is.na(LGRIT_INFERRED), NA,
                               ifelse(LGRIT_INFERRED == 0, 0, 1))) %>%
  relocate(LGRIT_INFERRED, LGRIT_BINARY, .after = LGRIT)

# BGBLV, LGBLV, BGBP (1 = rituals more important; 2 = belief more important; 3 = neither are important) and LGBP (will drop here, as LG accidentally asked about MG twice...)
altai_relig <- altai_relig %>%
  mutate(BGBLV = as.numeric(BGBLV)) %>%
  mutate(LGBLV = as.numeric(LGBLV)) %>%
  mutate(BGBP = as.numeric(BGBP)) %>%
  rename(BGPICK = BGBP) %>%
  select(-LGBP)

# BGMON
altai_relig <- altai_relig %>%
  mutate(BGMON_INFERRED = BGMON) %>%
  mutate(BGMON = ifelse(BGMON == "[0]" | BGMON == "[1]", "NA", BGMON)) %>%
  mutate(BGMON_INFERRED = ifelse(BGMON_INFERRED == "[1]", "1",
                                 ifelse(BGMON_INFERRED == "[0]", "0", BGMON_INFERRED))) %>%
  mutate(BGMON = as.numeric(BGMON)) %>%
  mutate(BGMON_INFERRED = as.numeric(BGMON_INFERRED)) %>%
  relocate(BGMON, BGMON_INFERRED, .after = BGOTHERRIT_INFERRED)

# LGMON
altai_relig <- altai_relig %>%
  mutate(LGMON_INFERRED = LGMON) %>%
  mutate(LGMON = ifelse(LGMON == "[0]" | LGMON == "[1]", "NA", LGMON)) %>%
  mutate(LGMON_INFERRED = ifelse(LGMON_INFERRED == "[1]", "1",
                                 ifelse(LGMON_INFERRED == "[0]", "0", LGMON_INFERRED))) %>%
  mutate(LGMON = as.numeric(LGMON)) %>%
  mutate(LGMON_INFERRED = as.numeric(LGMON_INFERRED)) %>%
  relocate(LGMON, LGMON_INFERRED, .after = LGOTHERRIT_INFERRED)


# RELCOR, RELOUT, TRADOUT, OUTSIM, INNUM, CONUM and OUTNUM - Drop all, as data super messy and not relevant for project here
altai_relig <- altai_relig %>%
  select(-c(RELCOR:OUTNUM_NOTE))


## Is quite a lot of missing data on some MG variables, some of which may be due to answering 'no' to believing in god (and hence not answering quetion). Will explore this
temp <- altai_relig %>%
  filter(is.na(BGPUNISH) | is.na(BGDIE) | is.na(BGFEEL) | is.na(BGSEE)) %>%
  select(CERCID, BGBLV, BGPUNISH, BGPUNISH_INFERRED, BGFEEL, BGFEEL_INFERRED, BGSEE, BGDIE, BGDIE_INFERRED)
print(temp, n = 30L)

# Lots of the missing data is in cases like this, so will refer back to Attila's original data and update if appropriate
altai_relig <- altai_relig %>%
  mutate(BGPUNISH = ifelse(CERCID == "AUD006", 0, BGPUNISH)) %>%
  mutate(BGPUNISH_INFERRED = ifelse(CERCID == "AUD006", 0, BGPUNISH_INFERRED)) %>%
  mutate(BGPUNISH = ifelse(CERCID == "AUD042", 1, BGPUNISH)) %>%
  mutate(BGPUNISH_INFERRED = ifelse(CERCID == "AUD042", 1, BGPUNISH_INFERRED))

# AUD010, AUD016, AUD027, AUD035, AUD050, AUD052, AUD058, AUD061, AUD066, AUD072, AUD073, AUD080, AUD085: Not believe in MG, and answered 'don't know' to all/most MG questions. Currently coded as 'NA' - Will leave as missing for now and impute missing data later on
# altai_relig <- altai_relig %>%
#   mutate(BGTHINK = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                         "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                         "AUD073", "AUD080", "AUD085")
#                           & is.na(BGTHINK), 0, BGTHINK)) %>%
#   mutate(BGTHINK_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                  "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                  "AUD073", "AUD080", "AUD085") 
#                                    & is.na(BGTHINK_INFERRED), 0, BGTHINK_INFERRED)) %>%
#   mutate(BGTHINK_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                "AUD073", "AUD080", "AUD085") 
#                                  & is.na(BGTHINK_BINARY), 0, BGTHINK_BINARY)) %>%
#   mutate(BGFREQW = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                         "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                         "AUD073", "AUD080", "AUD085") 
#                           & is.na(BGFREQW), 0, BGFREQW)) %>%
#   mutate(BGFREQW_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                  "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                  "AUD073", "AUD080", "AUD085")
#                                    & is.na(BGFREQW_INFERRED), 0, BGFREQW_INFERRED)) %>%
#   mutate(BGFREQW_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                "AUD073", "AUD080", "AUD085") 
#                                  & is.na(BGFREQW_BINARY), 0, BGFREQW_BINARY)) %>%
#   mutate(BGPUNISH = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                          "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                          "AUD073", "AUD080", "AUD085") 
#                            & is.na(BGPUNISH), 0, BGPUNISH)) %>%
#   mutate(BGPUNISH_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                   "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                   "AUD073", "AUD080", "AUD085")
#                                     & is.na(BGPUNISH_INFERRED), 0, BGPUNISH_INFERRED)) %>%
#   mutate(BGSTEAL = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                         "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                         "AUD073", "AUD080", "AUD085")
#                           & is.na(BGSTEAL), 0, BGSTEAL)) %>%
#   mutate(BGSTEAL_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                  "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                  "AUD073", "AUD080", "AUD085")
#                                    & is.na(BGSTEAL_INFERRED), 0, BGSTEAL_INFERRED)) %>%
#   mutate(BGSTEAL_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                "AUD073", "AUD080", "AUD085")
#                                  & is.na(BGSTEAL_BINARY), 0, BGSTEAL_BINARY)) %>%
#   mutate(BGSTLIMP = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                          "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                          "AUD073", "AUD080", "AUD085")
#                            & is.na(BGSTLIMP), 0, BGSTLIMP)) %>%
#   mutate(BGSTLIMP_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                   "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                   "AUD073", "AUD080", "AUD085")
#                                     & is.na(BGSTLIMP_INFERRED), 0, BGSTLIMP_INFERRED)) %>%
#   mutate(BGSTLIMP_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                 "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                 "AUD073", "AUD080", "AUD085")
#                                   & is.na(BGSTLIMP_BINARY), 0, BGSTLIMP_BINARY)) %>%
#   mutate(BGLYING = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                         "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                         "AUD073", "AUD080", "AUD085")
#                           & is.na(BGLYING), 0, BGLYING)) %>%
#   mutate(BGLYING_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                  "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                  "AUD073", "AUD080", "AUD085")
#                                    & is.na(BGLYING_INFERRED), 0, BGLYING_INFERRED)) %>%
#   mutate(BGLYING_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                "AUD073", "AUD080", "AUD085") 
#                                  & is.na(BGLYING_BINARY), 0, BGLYING_BINARY)) %>%
#   mutate(BGLIEIMP = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                          "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                          "AUD073", "AUD080", "AUD085")
#                            & is.na(BGLIEIMP), 0, BGLIEIMP)) %>%
#   mutate(BGLIEIMP_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                   "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                   "AUD073", "AUD080", "AUD085")
#                                     & is.na(BGLIEIMP_INFERRED), 0, BGLIEIMP_INFERRED)) %>%
#   mutate(BGLIEIMP_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                 "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                 "AUD073", "AUD080", "AUD085")
#                                   & is.na(BGLIEIMP_BINARY), 0, BGLIEIMP_BINARY)) %>%
#   mutate(BGMURDER = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                          "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                          "AUD073", "AUD080", "AUD085")
#                            & is.na(BGMURDER), 0, BGMURDER)) %>%
#   mutate(BGMURDER_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                   "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                   "AUD073", "AUD080", "AUD085")
#                                     & is.na(BGMURDER_INFERRED), 0, BGMURDER_INFERRED)) %>%
#   mutate(BGMURDER_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                 "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                 "AUD073", "AUD080", "AUD085")
#                                   & is.na(BGMURDER_BINARY), 0, BGMURDER_BINARY)) %>%
#   mutate(BGMURDIMP = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                           "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                           "AUD073", "AUD080", "AUD085")
#                             & is.na(BGMURDIMP), 0, BGMURDIMP)) %>%
#   mutate(BGMURDIMP_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                    "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                    "AUD073", "AUD080", "AUD085")
#                                      & is.na(BGMURDIMP_INFERRED), 0, BGMURDIMP_INFERRED)) %>%
#   mutate(BGMURDIMP_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                  "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                  "AUD073", "AUD080", "AUD085")
#                                    & is.na(BGMURDIMP_BINARY), 0, BGMURDIMP_BINARY)) %>%
#   mutate(BGFEEL = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                        "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                        "AUD073", "AUD080", "AUD085") 
#                          & is.na(BGFEEL), 0, BGFEEL)) %>%
#   mutate(BGFEEL_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                 "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                 "AUD073", "AUD080", "AUD085")
#                                   & is.na(BGFEEL_INFERRED), 0, BGFEEL_INFERRED)) %>%
#   mutate(BGSEE = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                       "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                       "AUD073", "AUD080", "AUD085") 
#                         & is.na(BGSEE), 0, BGSEE)) %>%
#   mutate(BGREWARD = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                          "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                          "AUD073", "AUD080", "AUD085")
#                            & is.na(BGREWARD), 0, BGREWARD)) %>%
#   mutate(BGREWARD_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                   "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                   "AUD073", "AUD080", "AUD085")
#                                     & is.na(BGREWARD_INFERRED), 0, BGREWARD_INFERRED)) %>%
#   mutate(BGREWARD_BINARY = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                 "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                 "AUD073", "AUD080", "AUD085")
#                                   & is.na(BGREWARD_BINARY), 0, BGREWARD_BINARY)) %>%
#   mutate(BGDIE = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                       "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                       "AUD073", "AUD080", "AUD085") 
#                         & is.na(BGDIE), 0, BGDIE)) %>%
#   mutate(BGDIE_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                "AUD073", "AUD080", "AUD085")
#                                  & is.na(BGDIE_INFERRED), 0, BGDIE_INFERRED)) %>%
#   mutate(BGSTRANGER = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                            "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                            "AUD073", "AUD080", "AUD085")
#                              & is.na(BGSTRANGER), 0, BGSTRANGER)) %>%
#   mutate(BGSTRANGER_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                     "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                     "AUD073", "AUD080", "AUD085") 
#                                       & is.na(BGSTRANGER_INFERRED), 0, BGSTRANGER_INFERRED)) %>%
#   mutate(BGOTHERRIT = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                            "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                            "AUD073", "AUD080", "AUD085")
#                              & is.na(BGOTHERRIT), 0, BGOTHERRIT)) %>%
#   mutate(BGOTHERRIT_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                     "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                     "AUD073", "AUD080", "AUD085")
#                                       & is.na(BGOTHERRIT_INFERRED), 0, BGOTHERRIT_INFERRED)) %>%
#   mutate(BGMON = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                       "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                       "AUD073", "AUD080", "AUD085") 
#                         & is.na(BGMON), 0, BGMON)) %>%
#   mutate(BGMON_INFERRED = ifelse(CERCID %in% c("AUD010", "AUD016", "AUD027", "AUD035", "AUD050", 
#                                                "AUD052", "AUD058", "AUD061", "AUD066", "AUD072", 
#                                                "AUD073", "AUD080", "AUD085")
#                                  & is.na(BGMON_INFERRED), 0, BGMON_INFERRED))


### Combine Altai demographics, game and religiosity data together
names(altai_demo)
names(altai_game)
names(altai_relig)

altai_main <- full_join(altai_game, altai_demo, by = "CERCID")
altai_main <- full_join(altai_main, altai_relig, by = "CERCID")


## Keep just variables needed for analyses
altai_main <- altai_main %>%
  select(c(CERCID, SEX, AGE, CHILDREN, FORMALED, MMAT, 
           INGFIRST, EXUSED1, COREL.L, EXUSED2, COREL.S,
           INGFIRST_DG, COREL.L_DG, COREL.S_DG,
           LGPUNISH, LGDIE, LGFEEL, LGSEE,
           BGPUNISH, BGDIE, BGFEEL, BGSEE,
           LGPERF, LGBLV, LGTHINK_BINARY,
           BGPERF, BGBLV, BGTHINK_BINARY))

## Drop the two people who did not take part in the games
altai_main %>%
  filter(is.na(COREL.L)) %>%
  select(c(CERCID, COREL.L, COREL.S, COREL.L_DG, COREL.S_DG))

altai_main <- altai_main %>%
  filter(!is.na(COREL.L))

## Save this dataset (in both RData and CSV formats)
save(altai_main, file = "altai_data.RData")
write_csv(altai_main, file = "altai_data.csv")



################################################################################
#### Data analysis

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("altai_data.RData")


### Descriptive stats

## Summary of missing data
cbind(n_miss = sapply(altai_main, function(x) sum(is.na(x))),
      per_miss = sapply(altai_main, function(x) round(sum(is.na(x)) / nrow(altai_main) * 100, 2)))

# 47 cases with complete data (54.7%)
sum(complete.cases(altai_main))
round(sum(complete.cases(altai_main)) / nrow(altai_main) * 100, 1)


# 62 cases (72.1%) with complete data if exclude Buddha and auxiliary vars
temp <- altai_main %>%
  select(-c(BGPUNISH, BGDIE, BGFEEL, BGSEE, LGPERF, LGBLV, LGTHINK_BINARY, BGPERF, BGBLV, BGTHINK_BINARY))

sum(complete.cases(temp))
round(sum(complete.cases(temp)) / nrow(temp) * 100, 1)


# 48 cases (55.8%) with complete data if exclude just Buddha vars
temp <- altai_main %>%
  select(-c(LGPERF, LGBLV, LGTHINK_BINARY, BGPERF, BGBLV, BGTHINK_BINARY))

sum(complete.cases(temp))
round(sum(complete.cases(temp)) / nrow(temp) * 100, 1)


## Demographics

# Age (years)
summary(altai_main$AGE)
sd(altai_main$AGE)

plot(density(altai_main$AGE), main = "", xlab = "Age")
hist(altai_main$AGE, main = "", xlab = "Age")

# Sex (1 = male)
addmargins(table(altai_main$SEX))
round(prop.table(table(altai_main$SEX)) * 100, 1)

# Children
addmargins(table(altai_main$CHILDREN))
round(prop.table(table(altai_main$CHILDREN)) * 100, 1)

summary(altai_main$CHILDREN)
sd(altai_main$CHILDREN, na.rm = TRUE)

plot(density(altai_main$CHILDREN, na.rm = TRUE), main = "", xlab = "Number of children")
hist(altai_main$CHILDREN, main = "", xlab = "Number of children")

# Education
addmargins(table(altai_main$FORMALED))
round(prop.table(table(altai_main$FORMALED)) * 100, 1)

summary(altai_main$FORMALED)
sd(altai_main$FORMALED, na.rm = TRUE)

plot(density(altai_main$FORMALED, na.rm = TRUE), main = "", xlab = "Number of years education")
hist(altai_main$FORMALED, main = "", xlab = "Number of years education")

# Material insecurity
addmargins(table(altai_main$MMAT))
round(prop.table(table(altai_main$MMAT)) * 100, 1)

summary(altai_main$MMAT)
sd(altai_main$MMAT, na.rm = TRUE)

plot(density(altai_main$MMAT, na.rm = TRUE), main = "", xlab = "Food insecurity")
hist(altai_main$MMAT, main = "", xlab = "Food insecurity")


## Religiosity

# MG punish
addmargins(table(altai_main$BGPUNISH))
round(prop.table(table(altai_main$BGPUNISH)) * 100, 1)

# LG punish
addmargins(table(altai_main$LGPUNISH))
round(prop.table(table(altai_main$LGPUNISH)) * 100, 1)

# Tetrachoric correlation between MG and LG PUNISH
addmargins(table(altai_main$LGPUNISH, altai_main$BGPUNISH))
round(prop.table(table(altai_main$LGPUNISH, altai_main$BGPUNISH), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$LGPUNISH, altai_main$BGPUNISH))


# MG afterlife
addmargins(table(altai_main$BGDIE))
round(prop.table(table(altai_main$BGDIE)) * 100, 1)

# LG afterlife
addmargins(table(altai_main$LGDIE))
round(prop.table(table(altai_main$LGDIE)) * 100, 1)

# Tetrachoric correlation between MG and LG DIE
addmargins(table(altai_main$LGDIE, altai_main$BGDIE))
round(prop.table(table(altai_main$LGDIE, altai_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$LGDIE, altai_main$BGDIE))


# Tetrachoric correlation between PUNISH and DIE for both MG and LG
addmargins(table(altai_main$LGPUNISH, altai_main$LGDIE))
round(prop.table(table(altai_main$LGPUNISH, altai_main$LGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$LGPUNISH, altai_main$LGDIE))

addmargins(table(altai_main$BGPUNISH, altai_main$BGDIE))
round(prop.table(table(altai_main$BGPUNISH, altai_main$BGDIE), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$BGPUNISH, altai_main$BGDIE))


# MG and LG mini-punishment scales
altai_main <- altai_main %>%
  mutate(BGPUN_AVE = rowMeans(select(., BGPUNISH, BGDIE), na.rm = FALSE)) %>%
  mutate(LGPUN_AVE = rowMeans(select(., LGPUNISH, LGDIE), na.rm = FALSE))

# MG
addmargins(table(altai_main$BGPUN_AVE))
round(prop.table(table(altai_main$BGPUN_AVE)) * 100, 1)

summary(altai_main$BGPUN_AVE)

# LG
addmargins(table(altai_main$LGPUN_AVE))
round(prop.table(table(altai_main$LGPUN_AVE)) * 100, 1)

summary(altai_main$LGPUN_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(altai_main$BGPUN_AVE)), main = "MG Punish", ylim = c(0, 0.7))
barplot(prop.table(table(altai_main$LGPUN_AVE)), main = "LG Punish", ylim = c(0, 0.7))
dev.off()

# Correlation between punishment mini-scales
addmargins(table(altai_main$LGPUN_AVE, altai_main$BGPUN_AVE))
round(prop.table(table(altai_main$LGPUN_AVE, altai_main$BGPUN_AVE), margin = 1) * 100, 1)

cor.test(altai_main$LGPUN_AVE, altai_main$BGPUN_AVE)
polychoric(cbind(as.factor(altai_main$LGPUN_AVE), as.factor(altai_main$BGPUN_AVE)))


# MG feel
addmargins(table(altai_main$BGFEEL))
round(prop.table(table(altai_main$BGFEEL)) * 100, 1)

# LG feel
addmargins(table(altai_main$LGFEEL))
round(prop.table(table(altai_main$LGFEEL)) * 100, 1)

# Tetrachoric correlation between MG and LG FEEL
addmargins(table(altai_main$LGFEEL, altai_main$BGFEEL))
round(prop.table(table(altai_main$LGFEEL, altai_main$BGFEEL), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$LGFEEL, altai_main$BGFEEL))


# MG see
addmargins(table(altai_main$BGSEE))
round(prop.table(table(altai_main$BGSEE)) * 100, 1)

# LG see
addmargins(table(altai_main$LGSEE))
round(prop.table(table(altai_main$LGSEE)) * 100, 1)

# Tetrachoric correlation between MG and LG SEE
addmargins(table(altai_main$LGSEE, altai_main$BGSEE))
round(prop.table(table(altai_main$LGSEE, altai_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$LGSEE, altai_main$BGSEE))


# Tetrachoric correlation between FEEL and SEE for both MG and LG
addmargins(table(altai_main$LGFEEL, altai_main$LGSEE))
round(prop.table(table(altai_main$LGFEEL, altai_main$LGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$LGFEEL, altai_main$LGSEE))

addmargins(table(altai_main$BGFEEL, altai_main$BGSEE))
round(prop.table(table(altai_main$BGFEEL, altai_main$BGSEE), margin = 1) * 100, 1)

tetrachoric(cbind(altai_main$BGFEEL, altai_main$BGSEE))


# MG and LG mini-omniscience scales
altai_main <- altai_main %>%
  mutate(BGOMNI_AVE = rowMeans(select(., BGFEEL, BGSEE), na.rm = FALSE)) %>%
  mutate(LGOMNI_AVE = rowMeans(select(., LGFEEL, LGSEE), na.rm = FALSE))

# MG
addmargins(table(altai_main$BGOMNI_AVE))
round(prop.table(table(altai_main$BGOMNI_AVE)) * 100, 1)

summary(altai_main$BGOMNI_AVE)

# LG
addmargins(table(altai_main$LGOMNI_AVE))
round(prop.table(table(altai_main$LGOMNI_AVE)) * 100, 1)

summary(altai_main$LGOMNI_AVE)

par(mfrow = c(1, 2))
barplot(prop.table(table(altai_main$BGOMNI_AVE)), main = "MG omniscience", ylim = c(0, 0.8))
barplot(prop.table(table(altai_main$LGOMNI_AVE)), main = "LG omniscience", ylim = c(0, 0.8))
dev.off()

# Correlation between omniscience mini-scales
addmargins(table(altai_main$LGOMNI_AVE, altai_main$BGOMNI_AVE))
round(prop.table(table(altai_main$LGOMNI_AVE, altai_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(altai_main$LGOMNI_AVE, altai_main$BGOMNI_AVE)
polychoric(cbind(as.factor(altai_main$LGOMNI_AVE), as.factor(altai_main$BGOMNI_AVE)))


# Correlation between punishment and omniscience mini-scales for both MG and LG
addmargins(table(altai_main$LGPUN_AVE, altai_main$LGOMNI_AVE))
round(prop.table(table(altai_main$LGPUN_AVE, altai_main$LGOMNI_AVE), margin = 1) * 100, 1)

cor.test(altai_main$LGPUN_AVE, altai_main$LGOMNI_AVE)
polychoric(cbind(as.factor(altai_main$LGPUN_AVE), as.factor(altai_main$LGOMNI_AVE)))

addmargins(table(altai_main$BGPUN_AVE, altai_main$BGOMNI_AVE))
round(prop.table(table(altai_main$BGPUN_AVE, altai_main$BGOMNI_AVE), margin = 1) * 100, 1)

cor.test(altai_main$BGPUN_AVE, altai_main$BGOMNI_AVE)
polychoric(cbind(as.factor(altai_main$BGPUN_AVE), as.factor(altai_main$BGOMNI_AVE)))


## Descriptives of auxiliary religiosity variables for imputation

# Believes in LG
addmargins(table(altai_main$LGBLV))
round(prop.table(table(altai_main$LGBLV)) * 100, 1)

# Ever thinks about LG
addmargins(table(altai_main$LGTHINK_BINARY))
round(prop.table(table(altai_main$LGTHINK_BINARY)) * 100, 1)

# Performs rituals towards LG
addmargins(table(altai_main$LGPERF))
round(prop.table(table(altai_main$LGPERF)) * 100, 1)

# Believes in MG
addmargins(table(altai_main$BGBLV))
round(prop.table(table(altai_main$BGBLV)) * 100, 1)

# Ever thinks about MG
addmargins(table(altai_main$BGTHINK_BINARY))
round(prop.table(table(altai_main$BGTHINK_BINARY)) * 100, 1)

# Performs rituals towards MG
addmargins(table(altai_main$BGPERF))
round(prop.table(table(altai_main$BGPERF)) * 100, 1)


### Cooperation

## RAGs

# Order (1 = local game first)
addmargins(table(altai_main$INGFIRST))
round(prop.table(table(altai_main$INGFIRST)) * 100, 1)


## Local RAG (RAG 1)

# Comprehension checks - All only needed one check, so no need to include in models below
addmargins(table(altai_main$EXUSED1))
round(prop.table(table(altai_main$EXUSED1)) * 100, 1)

altai_main <- altai_main %>%
  select(-EXUSED1)

# Tokens to distant co-religionist
summary(altai_main$COREL.L)
sd(altai_main$COREL.L, na.rm = TRUE)


## Self RAG (RAG 2)

# Comprehension checks - All only needed one check, so no need to include in models below
addmargins(table(altai_main$EXUSED2))
round(prop.table(table(altai_main$EXUSED2)) * 100, 1)

altai_main <- altai_main %>%
  select(-EXUSED2)

# Tokens to distant co-religionist
summary(altai_main$COREL.S)
sd(altai_main$COREL.S, na.rm = TRUE)


## Plot of RAG results to theoretical binomial distribution
pdf(file = "Altai_RAGtoBinom.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

# Local RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "LOCAL RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.15), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(altai_main$COREL.L, na.rm = TRUE))
polygon(density(altai_main$COREL.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(altai_main$COREL.L, na.rm = TRUE), lty = "dashed", col = "red")

# Self RAG
plot(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col = "blue", type = "l", 
     main = "SELF RAG", xlab = "Donations to Distant", ylab = "Density",
     ylim = c(0, 0.15), xlim = c(-3, 31))
polygon(0:30, dbinom(x = 0:30, size = 30, prob = 0.5), col=rgb(0, 0, 1, 0.25))
abline(v = 15, lty = "dashed")

lines(density(altai_main$COREL.S, na.rm = TRUE))
polygon(density(altai_main$COREL.S, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(altai_main$COREL.S, na.rm = TRUE), lty = "dashed", col = "red")

dev.off()


### Test whether RAG results differ from unbiased binomial 50/50 distribution

# LOCAL RAG
mod_RAG.L_base <- brm(COREL.L | trials(30) ~ 1,
                      family = "binomial",
                      data = altai_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 4477)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_base)

# 95% CIs of log-odds includes 0/odds include 1, but only just, suggesting a little bias towards local recipients, although are also consistent with unbiased allocations
exp(fixef(mod_RAG.L_base))


# SELF RAG
mod_RAG.S_base <- brm(COREL.S | trials(30) ~ 1,
                      family = "binomial",
                      data = altai_main,
                      prior = c(prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 34196)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_base)

# 95% CIs of log-odds exclude 0/odds exclude 1, suggesting bias towards self rather than distant
exp(fixef(mod_RAG.S_base))


## Formal test of difference between local and self RAG donations
df_temp <- as.data.frame(cbind(RAG = c(altai_main$COREL.L, altai_main$COREL.S), 
                               Cond = c(rep("Local", nrow(altai_main)), rep("Self", nrow(altai_main))),
                               CERCID = c(altai_main$CERCID, altai_main$CERCID)))

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
                   iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 45741)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_RAG.LvsS, variables = "Cond", type = "prediction")


## DGs

# Order (1 = local game first)
addmargins(table(altai_main$INGFIRST_DG))
round(prop.table(table(altai_main$INGFIRST_DG)) * 100, 1)


## Local DG (DG 1)

# Tokens to distant co-religionist
summary(altai_main$COREL.L_DG)
sd(altai_main$COREL.L_DG, na.rm = TRUE)

addmargins(table(altai_main$COREL.L_DG))
round(prop.table(table(altai_main$COREL.L_DG)) * 100, 1)


## Self DG (RAG 2)

# Tokens to distant co-religionist
summary(altai_main$COREL.S_DG)
sd(altai_main$COREL.S_DG, na.rm = TRUE)

addmargins(table(altai_main$COREL.S_DG))
round(prop.table(table(altai_main$COREL.S_DG)) * 100, 1)


## Plot of DG results
pdf(file = "Altai_DGDescriptives.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(altai_main$COREL.L_DG, na.rm = TRUE), 
     main = "LOCAL DG", xlab = "Donations to Distant",
     ylim = c(0, 0.3), xlim = c(-1, 11))
polygon(density(altai_main$COREL.L_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(altai_main$COREL.L_DG, na.rm = TRUE), lty = "dashed")

plot(density(altai_main$COREL.S_DG, na.rm = TRUE), 
     main = "SELF DG", xlab = "Donations to Distant",
     ylim = c(0, 0.3), xlim = c(-1, 11))
polygon(density(altai_main$COREL.S_DG, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = mean(altai_main$COREL.S_DG, na.rm = TRUE), lty = "dashed")

dev.off()


## Formal test of difference between local and self DG donations
df_temp <- as.data.frame(cbind(DG = c(altai_main$COREL.L_DG, altai_main$COREL.S_DG), 
                               Cond = c(rep("Local", nrow(altai_main)), rep("Self", nrow(altai_main))),
                               CERCID = c(altai_main$CERCID, altai_main$CERCID)))

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
                   iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 70508)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_DG.LvsS)

## Difference between conditions on count scale, using 'marginaleffects' package
avg_comparisons(mod_DG.LvsS, variables = "Cond", type = "prediction")


###########################################################################################
### Complete-case analyses: Main regression analyses using causal model (with age, sex, education, number of children and material security as confounders, plus game order as covariates [no RAG comprehension checks as no variability])

## Remove missing data
dat_cc <- altai_main[complete.cases(altai_main$COREL.L, altai_main$COREL.S,
                                    altai_main$COREL.L_DG, altai_main$COREL.S_DG,
                                    altai_main$LGPUN_AVE, altai_main$LGOMNI_AVE,
                                    altai_main$LGPUNISH, altai_main$LGFEEL,
                                    altai_main$BGPUN_AVE, altai_main$BGOMNI_AVE,
                                    altai_main$BGPUNISH, altai_main$BGFEEL,
                                    altai_main$AGE, altai_main$SEX, altai_main$FORMALED,
                                    altai_main$MMAT, altai_main$CHILDREN,
                                    altai_main$INGFIRST, altai_main$INGFIRST_DG), ]

### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN +
                   INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 8143)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L)

# Add model fit, so can compare models
mod_RAG.L <- add_criterion(mod_RAG.L, "loo")
loo_RAG.L <- loo(mod_RAG.L)
loo_RAG.L


## Interaction model
mod_RAG.L_int <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN +
                       INGFIRST,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12355)

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
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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


### RAG - DISTANT vs SELF

## Additive model
mod_RAG.S <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                   AGE + SEX + FORMALED + MMAT + CHILDREN +
                   INGFIRST,
                 family = "binomial",
                 data = dat_cc,
                 prior = c(prior(normal(0, 1), class = b),
                           prior(normal(0, 2), class = Intercept)),
                 iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 96067)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S)

# Add model fit, so can compare models
mod_RAG.S <- add_criterion(mod_RAG.S, "loo")
loo_RAG.S <- loo(mod_RAG.S)
loo_RAG.S


## Interaction model
mod_RAG.S_int <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                       AGE + SEX + FORMALED + MMAT + CHILDREN +
                       INGFIRST,
                     family = "binomial",
                     data = dat_cc,
                     prior = c(prior(normal(0, 1), class = b),
                               prior(normal(0, 2), class = Intercept)),
                     iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 79732)

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
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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

plot(density(diff_RAG.S), main = "Self RAG", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S)
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))

quantile(RAG.S_0, c(0.025, 0.5, 0.975))
quantile(RAG.S_1, c(0.025, 0.5, 0.975))


## Density plots of RAG results
pdf(file = "Altai_RAG_gcomp_CCA.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.3), xlim = c(-8, 4),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.3), xlim = c(-8, 4),
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
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 18400)

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
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 65972)

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
                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                  INGFIRST_DG,
                family = cumulative("logit"),
                data = dat_cc,
                prior = c(prior(normal(0, 1), class = b),
                          prior(normal(0, 2), class = Intercept)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 37946)

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
                    iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 90183)

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
pdf(file = "Altai_DG_gcomp_CCA.pdf", height = 5, width = 10)

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
#### Sensitivity analysis 1: Inclusion of moralistic god (Buddha) terms in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 84709)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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

# Less negative than original model, but no real difference in interpretation (wider CIs though)
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                         BGPUN_AVE + BGOMNI_AVE +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 52273)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens1)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUN_AVE = 0, LGOMNI_AVE = 0,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S_sens1, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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

# Marginally less negative than original model, but no real difference in interpretation (slightly wider CIs though)
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
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 15087)

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
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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
mod_DG.S_sens1 <- brm(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                        BGPUN_AVE + BGOMNI_AVE +
                        AGE + SEX + FORMALED + MMAT + CHILDREN +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 27893)

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
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUN_AVE = 1, LGOMNI_AVE = 1,
                               BGPUN_AVE = dat_cc$BGPUN_AVE, BGOMNI_AVE = dat_cc$BGOMNI_AVE,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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

# No real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))


#################################################################
#### Sensitivity analysis 2: Use of single variables for punishment and omniscience, rather than mini-scales

## As little evidence of multiplicative effect between exposures, will just focus on additive models here


### RAG - DISTANT vs LOCAL
mod_RAG.L_sens2 <- brm(COREL.L | trials(30) ~ LGPUNISH + LGFEEL +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12352)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.L_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.L_sens2, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST = dat_cc$INGFIRST))
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

# More negative than original model, but no real difference in interpretation
quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens2 <- brm(COREL.S | trials(30) ~ LGPUNISH + LGFEEL +
                         AGE + SEX + FORMALED + MMAT + CHILDREN +
                         INGFIRST,
                       family = "binomial",
                       data = dat_cc,
                       prior = c(prior(normal(0, 1), class = b),
                                 prior(normal(0, 2), class = Intercept)),
                       iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 2619)

# Model fit statistics all look good (r-hat = 1, high ESS)
summary(mod_RAG.S_sens2)


## G-computation

# Predict outcome where all supernatural belief variables are '0'
df_sup0 <- as.data.frame(cbind(LGPUNISH = 0, LGFEEL = 0,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST = dat_cc$INGFIRST))
sup0 <- predict(mod_RAG.S_sens2, newdata = df_sup0, summary = FALSE)

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
                               INGFIRST = dat_cc$INGFIRST))
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

# Less negative than core model, but no real difference in interpretation
quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens2 <- brm(COREL.L_DG ~ LGPUNISH + LGFEEL +
                        AGE + SEX + FORMALED + MMAT + CHILDREN +
                        INGFIRST_DG,
                      family = cumulative("logit"),
                      data = dat_cc,
                      prior = c(prior(normal(0, 1), class = b),
                                prior(normal(0, 2), class = Intercept)),
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 56469)

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
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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
                      iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 79804)

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
sup0_temp <- sup0
for(i in 1:length(attr(sup0, "levels"))) {
  sup0_temp[sup0 == i] <- as.numeric(attr(sup0, "levels")[i])
}
sup0 <- sup0_temp

# Predict outcome where all supernatural belief variables are '1'
df_sup1 <- as.data.frame(cbind(LGPUNISH = 1, LGFEEL = 1,
                               AGE = dat_cc$AGE, SEX = dat_cc$SEX, 
                               FORMALED = dat_cc$FORMALED, MMAT = dat_cc$MMAT, 
                               CHILDREN = dat_cc$CHILDREN,
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

plot(density(diff_DG.S_sens2), main = "Self DG (Single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens2)
quantile(diff_DG.S_sens2, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens2, c(0.025, 0.5, 0.975))

# No real difference in interpretation
quantile(diff_DG.S, c(0.025, 0.5, 0.975))



###########################################################################################
### Multiple imputation analyses

str(altai_main)
summary(altai_main)

## Summary of missing data
cbind(n_miss = sapply(altai_main, function(x) sum(is.na(x))),
      per_miss = sapply(altai_main, function(x) round(sum(is.na(x)) / nrow(altai_main) * 100, 2)))

# Code binary variables as factors (for MICE to work with logistic regression)
altai_main$SEX <- as.factor(altai_main$SEX)
altai_main$INGFIRST <- as.factor(altai_main$INGFIRST)
altai_main$INGFIRST_DG <- as.factor(altai_main$INGFIRST_DG)
altai_main$LGPUNISH <- as.factor(altai_main$LGPUNISH)
altai_main$LGDIE <- as.factor(altai_main$LGDIE)
altai_main$LGFEEL <- as.factor(altai_main$LGFEEL)
altai_main$LGSEE <- as.factor(altai_main$LGSEE)
altai_main$BGPUNISH <- as.factor(altai_main$BGPUNISH)
altai_main$BGDIE <- as.factor(altai_main$BGDIE)
altai_main$BGFEEL <- as.factor(altai_main$BGFEEL)
altai_main$BGSEE <- as.factor(altai_main$BGSEE)
altai_main$LGPERF <- as.factor(altai_main$LGPERF)
altai_main$LGBLV <- as.factor(altai_main$LGBLV)
altai_main$LGTHINK_BINARY <- as.factor(altai_main$LGTHINK_BINARY)
altai_main$BGPERF <- as.factor(altai_main$BGPERF)
altai_main$BGBLV <- as.factor(altai_main$BGBLV)
altai_main$BGTHINK_BINARY <- as.factor(altai_main$BGTHINK_BINARY)

str(altai_main)
summary(altai_main)


## Set up imputations - Change mini-scales from PMM to passively impute (as is just based on means of two prior variables)
meth <- make.method(altai_main)
meth["LGPUN_AVE"] <- "~ I(((as.numeric(LGPUNISH) - 1) + (as.numeric(LGDIE) - 1)) / 2)"
meth["LGOMNI_AVE"] <- "~ I(((as.numeric(LGFEEL) - 1) + (as.numeric(LGSEE) - 1)) / 2)"
meth["BGPUN_AVE"] <- "~ I(((as.numeric(BGPUNISH) - 1) + (as.numeric(BGDIE) - 1)) / 2)"
meth["BGOMNI_AVE"] <- "~ I(((as.numeric(BGFEEL) - 1) + (as.numeric(BGSEE) - 1)) / 2)"
meth

# Predictor matrix - Exclude punishment and omniscience scores as predictors, and ID
pred <- make.predictorMatrix(altai_main)
pred[, "LGPUN_AVE"] <- 0
pred[, "LGOMNI_AVE"] <- 0
pred[, "BGPUN_AVE"] <- 0
pred[, "BGOMNI_AVE"] <- 0
pred[, "CERCID"] <- 0
pred["CERCID", ] <- 0
pred

# Visit sequence - Make sure passively-imputed scales are after the individual variables (they are)
visit <- make.visitSequence(altai_main)
visit

# Test imputation to check formulas and that no obvious errors
test <- mice(altai_main, m = 5, maxit = 0, method = meth, predictorMatrix = pred, visitSequence = visit)
test$formulas

## Run imputations - 20 imputations as approx. 20% cases with missing data (and to avoid having too many datasets, which slows down computation when running Bayesian models on these data)
imp <- mice(altai_main, m = 20, maxit = 10, 
            method = meth, predictorMatrix = pred, visitSequence = visit,
            seed = 76123, print = TRUE)

## Check imputations worked correctly
imp1 <- complete(imp, 1)

head(altai_main, n = 20L)
head(imp1, n = 20L)


## Save these imputed datasets, to save time if need them later
save(imp, file = "altai_imp.RData")
#load("altai_imp.RData")


#### Descriptive statistics for variables with missing data
temp_imp <- complete(imp, "long")

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

## LGDIE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGDIE == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGFEEL
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGFEEL == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LGSEE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGSEE == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGPUNISH
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGPUNISH == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGDIE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGDIE == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGFEEL
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGFEEL == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MGSEE
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGSEE == "1"), per = N / nrow(altai_main) * 100) %>%
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

## MG omniscience mini-scale
temp_imp %>%
  group_by(.imp) %>%
  summarise(mean = mean(BGOMNI_AVE)) %>%
  ungroup() %>%
  summarise(mean_mean = mean(mean))

## LG Believe
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGBLV == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MG Believe
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGBLV == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## LG rituals
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(LGPERF == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))

## MG rituals
temp_imp %>%
  group_by(.imp) %>%
  summarise(N = sum(BGPERF == "1"), per = N / nrow(altai_main) * 100) %>%
  ungroup() %>%
  summarise(mean_N = mean(N), mean_per = mean(per))


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


#### Main regression analyses using causal model (with age, sex, education, number of children and material security as confounders, plus game order as covariates [no RAG comprehension checks as no variability]) #####

## Using 'brm_multiple' to perform analyses on each imputed dataset and then pool results together (see https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html)

## Extract each imputed dataset and convert binary variables back to integer from factors
imp_all <- complete(imp, action = "all")

for (i in 1:length(imp_all)) {
  temp <- imp_all[[i]]
  temp$SEX <- as.integer(temp$SEX) - 1
  temp$INGFIRST <- as.integer(temp$INGFIRST) - 1
  temp$INGFIRST_DG <- as.integer(temp$INGFIRST_DG) - 1
  temp$LGPUNISH <- as.integer(temp$LGPUNISH) - 1
  temp$LGFEEL <- as.integer(temp$LGFEEL) - 1
  temp$COREL.L_DG <- factor(temp$COREL.L_DG, ordered = TRUE)
  temp$COREL.S_DG <- factor(temp$COREL.S_DG, ordered = TRUE)
  
  # Replace old imputed data with new one
  imp_all[[i]] <- temp
}

str(imp_all)


### RAG - DISTANT vs LOCAL

## Additive model
mod_RAG.L <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN +
                            INGFIRST,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 1673,
                          combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.L, check_data = FALSE)

## To note: r-hat and effective sample size values may indicate poor fitting and convergence issues, but this is because different imputed datasets have different values, so may not give exactly the same results - This is completely normal (and to be expected with MI!), so is likely a false positive. Still, worth checking that each individual chain converged, just to make sure
summary(mod_RAG.L[[1]])
summary(mod_RAG.L[[2]])


## Model with interaction between exposures
mod_RAG.L_int <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN +
                            INGFIRST,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 61784,
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
                            AGE + SEX + FORMALED + MMAT + CHILDREN +
                            INGFIRST,
                          family = "binomial",
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 63339,
                          combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_RAG.S, check_data = FALSE)


## Model with interaction between exposures
mod_RAG.S_int <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                                AGE + SEX + FORMALED + MMAT + CHILDREN +
                                INGFIRST,
                              family = "binomial",
                              data = imp_all,
                              prior = c(prior(normal(0, 1), class = b),
                                        prior(normal(0, 2), class = Intercept)),
                              iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 14157,
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
pdf(file = "Altai_RAG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_RAG.L, na.rm = TRUE), ylim = c(0, 0.35), xlim = c(-9, 4),
     main = "LOCAL RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_RAG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_RAG.S, na.rm = TRUE), ylim = c(0, 0.35), xlim = c(-9, 4),
     main = "SELF RAG", xlab = "Marginal diff to distant")
polygon(density(diff_RAG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_RAG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_RAG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()



### DG - DISTANT vs LOCAL

## Additive model
mod_DG.L <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                            AGE + SEX + FORMALED + MMAT + CHILDREN +
                            INGFIRST_DG,
                          family = cumulative("logit"),
                          data = imp_all,
                          prior = c(prior(normal(0, 1), class = b),
                                    prior(normal(0, 2), class = Intercept)),
                          iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 10229,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.L, check_data = FALSE)


## Model with interaction between exposures
mod_DG.L_int <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                                AGE + SEX + FORMALED + MMAT + CHILDREN +
                                INGFIRST_DG,
                              family = cumulative("logit"),
                              data = imp_all,
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
                           AGE + SEX + FORMALED + MMAT + CHILDREN +
                           INGFIRST_DG,
                         family = cumulative("logit"),
                         data = imp_all,
                         prior = c(prior(normal(0, 1), class = b),
                                   prior(normal(0, 2), class = Intercept)),
                         iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 86438,
                         combine = FALSE)

# Combine results across imputed datasets
combine_models(mlist = mod_DG.S, check_data = FALSE)


## Model with interaction between exposures
mod_DG.S_int <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE + LGPUN_AVE:LGOMNI_AVE +
                               AGE + SEX + FORMALED + MMAT + CHILDREN +
                               INGFIRST_DG,
                             family = cumulative("logit"),
                             data = imp_all,
                             prior = c(prior(normal(0, 1), class = b),
                                       prior(normal(0, 2), class = Intercept)),
                             iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 39775,
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
pdf(file = "Altai_DG_gcomp.pdf", height = 5, width = 10)

par(mfrow = c(1, 2))

plot(density(diff_DG.L, na.rm = TRUE), ylim = c(0, 0.8), xlim = c(-4, 2),
     main = "LOCAL DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.L, na.rm = TRUE), col=rgb(1, 0, 0, 0.25))
abline(v = quantile(diff_DG.L, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.L, c(0.025, 0.975)), lty = "dashed", col = "red")

plot(density(diff_DG.S, na.rm = TRUE), ylim = c(0, 0.8), xlim = c(-4, 2),
     main = "SELF DG", xlab = "Marginal diff to distant")
polygon(density(diff_DG.S, na.rm = TRUE), col=rgb(0, 0, 1, 0.25))
abline(v = quantile(diff_DG.S, 0.5), lty = "dashed", lw = 2)
abline(v = quantile(diff_DG.S, c(0.025, 0.975)), lty = "dashed", col = "blue")

dev.off()



#################################################################
#### Sensitivity analysis 1: Inclusion of moralistic god (Buddha) terms in model as potential confounder

## As little evidence of multiplicative effect between exposures, will just focus on additive models here

### RAG - DISTANT vs LOCAL
mod_RAG.L_sens1 <- brm_multiple(COREL.L | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  BGPUN_AVE + BGOMNI_AVE +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                                  INGFIRST,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 35424,
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

plot(density(diff_RAG.L_sens1), main = "Local RAG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens1)
quantile(diff_RAG.L_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens1 <- brm_multiple(COREL.S | trials(30) ~ LGPUN_AVE + LGOMNI_AVE +
                                  BGPUN_AVE + BGOMNI_AVE +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                                  INGFIRST,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 91497,
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

plot(density(diff_RAG.S_sens1), main = "Self RAG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens1)
quantile(diff_RAG.S_sens1, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens1, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens1 <- brm_multiple(COREL.L_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 BGPUN_AVE + BGOMNI_AVE +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 81681,
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

plot(density(diff_DG.L_sens1), main = "Local DG (Buddha)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens1)
quantile(diff_DG.L_sens1, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens1, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens1, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens1 <- brm_multiple(COREL.S_DG ~ LGPUN_AVE + LGOMNI_AVE +
                                 BGPUN_AVE + BGOMNI_AVE +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 61378,
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
#### Sensitivity analysis 2: Use of single variables for punishment and omniscience, rather than mini-scales

## As little evidence of multiplicative effect between exposures, will just focus on additive models here

### RAG - DISTANT vs LOCAL
mod_RAG.L_sens2 <- brm_multiple(COREL.L | trials(30) ~ LGPUNISH + LGFEEL +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                                  INGFIRST,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 76481,
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
  temp$LGPUNISH <- 0
  temp$LGFEEL <- 0
  sup0_temp <- predict(mod_RAG.L_sens2[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
  temp$LGFEEL <- 1
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

plot(density(diff_RAG.L_sens2), main = "Local RAG (single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.L_sens2)
quantile(diff_RAG.L_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.L_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.L, c(0.025, 0.5, 0.975))


### RAG - DISTANT vs SELF
mod_RAG.S_sens2 <- brm_multiple(COREL.S | trials(30) ~ LGPUNISH + LGFEEL +
                                  AGE + SEX + FORMALED + MMAT + CHILDREN +
                                  INGFIRST,
                                family = "binomial",
                                data = imp_all,
                                prior = c(prior(normal(0, 1), class = b),
                                          prior(normal(0, 2), class = Intercept)),
                                iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 84000,
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
  temp$LGPUNISH <- 0
  temp$LGFEEL <- 0
  sup0_temp <- predict(mod_RAG.S_sens2[[i]], newdata = temp, summary = FALSE)
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
  temp$LGFEEL <- 1
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

plot(density(diff_RAG.S_sens2), main = "Self RAG (single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_RAG.S_sens2)
quantile(diff_RAG.S_sens2, c(0.025, 0.5, 0.975))

quantile(RAG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(RAG.S_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_RAG.S, c(0.025, 0.5, 0.975))


### DG - DISTANT vs LOCAL
mod_DG.L_sens2 <- brm_multiple(COREL.L_DG ~ LGPUNISH + LGFEEL +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 12318,
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
  temp$LGPUNISH <- 0
  temp$LGFEEL <- 0
  sup0_temp <- predict(mod_DG.L_sens2[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
  temp$LGFEEL <- 1
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

plot(density(diff_DG.L_sens2), main = "Local DG (single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.L_sens2)
quantile(diff_DG.L_sens2, c(0.025, 0.5, 0.975))

quantile(DG.L_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.L_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.L, c(0.025, 0.5, 0.975))


### DG - DISTANT vs SELF
mod_DG.S_sens2 <- brm_multiple(COREL.S_DG ~ LGPUNISH + LGFEEL +
                                 AGE + SEX + FORMALED + MMAT + CHILDREN +
                                 INGFIRST_DG,
                               family = cumulative("logit"),
                               data = imp_all,
                               prior = c(prior(normal(0, 1), class = b),
                                         prior(normal(0, 2), class = Intercept)),
                               iter = 2000, warmup = 1000, chains = 4, cores = 4, seed = 42352,
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
  temp$LGPUNISH <- 0
  temp$LGFEEL <- 0
  sup0_temp <- predict(mod_DG.S_sens2[[i]], newdata = temp, summary = FALSE)
  
  # Convert these back from factors to numbers of tokens
  sup0_temp2 <- sup0_temp
  for(j in 1:length(attr(sup0_temp, "levels"))) {
    sup0_temp2[sup0_temp == j] <- as.numeric(attr(sup0_temp, "levels")[j])
  }
  sup0_temp <- sup0_temp2
  
  sup0[[i]] <- sup0_temp
  
  
  ## Exposures at 1
  temp$LGPUNISH <- 1
  temp$LGFEEL <- 1
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

plot(density(diff_DG.S_sens2), main = "Self DG (single vars)", xlab = "Marginal difference in cooperation to DISTANT")
summary(diff_DG.S_sens2)
quantile(diff_DG.S_sens2, c(0.025, 0.5, 0.975))

quantile(DG.S_0_sens2, c(0.025, 0.5, 0.975))
quantile(DG.S_1_sens2, c(0.025, 0.5, 0.975))

# Compare to core model
#quantile(diff_DG.S, c(0.025, 0.5, 0.975))
