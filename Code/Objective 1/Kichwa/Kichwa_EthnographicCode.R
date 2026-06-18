### Objective 1 Initial Info for Ethnographic Section - Kichwa
### Created 5/5/2026 by Dan Major-Smith
### R 4.4.1

####################################################################
#### Clear workspace, set working directory and install/load packages
rm(list=ls())
Sys.setenv(LANG = "en")

setwd("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/Projects/Connor_Kichwa/Objective 1 - Kichwa")
#setwd("")


#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library(readxl)

## Note that, for brms to work, 'stan' will also need to be installed on your computer - see https://mc-stan.org/install/
#install.packages("brms")
library(brms)

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


### Religious Landscape Interview data 

## Read in data (note that this step processes the raw data which is not openly-available. To follow these analyses, read in the data on line 57)
rli <- read_excel(path = "C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Ecuador - Wood/Data_v3/English/RLI English Anon FINAL_DMS.xlsx",
                  sheet = "RLI",
                  skip = 1)

head(rli)

## Keep necessary columns
rli <- rli %>%
  select(c(ID:REWARD))

## Save this dataset (in both RData and CSV formats)
save(rli, file = "Kichwa_RLI.RData")
write_csv(rli, file = "Kichwa_RLI.csv")

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("Kichwa_RLI.RData")


#### Cultural salience of deities
table(rli$GOD, useNA = "ifany")

## Tidy and group some of these answers
rli <- rli %>%
  mutate(GOD.CLEAN = recode(GOD, "God (Catholic)" = "Christian God", "God (Christian)" = "Christian God",
                            "God the Father" = "Christian God", "Trinity/God the Father" = "Christian God",
                            "God" = "Christian God",
                            "Jesus" = "Jesus Christ", "Holy Child" = "Jesus Christ", 
                            "The Divine Child" = "Jesus Christ", 
                            "The Virgin" = "Virgin Mary", "The Virgin of the Swan" = "Virgin Mary", 
                            "Virgin of Quinche" = "Virgin Mary", 
                            "Spirit of nature" = "Pachamama", "Spirit of Nature" = "Pachamama", 
                            "Spirit of nature (Pachamama) which leads people through shamans" = "Pachamama", 
                            "Yacha (spirit of shamans and nature; spirit that enters the shaman)" = "Yacha"))

table(rli$GOD.CLEAN, useNA = "ifany")

# Remove missing data
rli <- rli %>%
  filter(!is.na(GOD))

table(rli$GOD.CLEAN, useNA = "ifany")

# Make 'ORDER' numeric
rli <- rli %>%
  mutate(ORDERGOD = as.numeric(ORDERGOD))

str(rli)

# Gods list
rli <- as.data.frame(rli)

# Calculate item salience and Smith's S
d.s <- CalculateSalience(rli, Order = "ORDERGOD", CODE = "GOD.CLEAN",
                         Subj = "ID", Salience = "godsal") 
godstab <- FreeListTable(d.s, Subj = "ID", Order = "ORDERGOD", 
                         CODE = "GOD.CLEAN", Salience = "godsal", tableType = "MAX_SALIENCE")
model <- SalienceByCode(d.s, Subj = "ID", CODE = "GOD.CLEAN", Salience = "godsal", dealWithDoubles = "MAX")

# Ordinal beta model for Smith's S for top 5 deities (i.e., deities listed by more than one person)
godsord <- SalienceOrdBeta(godstab, var_sel = "TOP", top = 5, seed = 72457, IDs_first = TRUE) 

# Summary
res <- SalienceEstimateSummary(godsord, quantiles = c(0.025, 0.5, 0.975))

# Flower plot
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = model, S_uncert = res, label = "Deities", 
                    lower_int = "2.5%", upper_int = "97.5%")

# Distribution plot
SalienceEstimatePlot(godsord, order = "high-low")

# The 'Yacha' distribution is skewed, and messes up the other plots, so will drop this
godsord_b <- godsord[-5]
SalienceEstimatePlot(godsord_b, order = "high-low")

# Save this plot
pdf(file = "GodsSalience.pdf", height = 6, width = 8)
SalienceEstimatePlot(godsord_b, order = "high-low")
dev.off()


## Gods' traits
rli %>%
  filter(GOD.CLEAN == "Christian God" | GOD.CLEAN == "Pachamama" | GOD.CLEAN == "Virgin Mary" | 
           GOD.CLEAN == "Jesus Christ" | GOD.CLEAN == "Yacha") %>%
  group_by(GOD.CLEAN) %>%
  summarise(n_pun = sum(!is.na(PUNISH)), yes_pun = sum(PUNISH), per_pun = round((yes_pun / n_pun) * 100, 1),
            n_see = sum(!is.na(SEE)), yes_see = sum(SEE, na.rm = TRUE), per_see = round((yes_see / n_see) * 100, 1),
            n_mor = sum(!is.na(CONCERN)), yes_mor = sum(CONCERN), per_mor = round((yes_mor / n_mor) * 100, 1),
            n_rew = sum(!is.na(REWARD)), yes_rew = sum(REWARD), per_rew = round((yes_rew / n_rew) * 100, 1))



##########
## Extra 'market' data from O2 - Basic descriptive stats

### Read in O2 data
kichwa_market <- read_excel("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/GGSL Data/Ecuador - Wood/Data_v3/English/EXTRA O2 QUESTIONS FINAL English.xlsx")

head(kichwa_market)
glimpse(kichwa_market)

## Keep necessary columns
kichwa_market <- kichwa_market %>%
  select(c(ID, MARKET1, MARKET12))

## Save this dataset (in both RData and CSV formats)
save(kichwa_market, file = "Kichwa_market.RData")
write_csv(kichwa_market, file = "Kichwa_market.csv")

## Read in processed data here, if needed (using RData format here, as keeps any formatting of variables)
#load("Kichwa_market.RData")

# Just tidy the market variables
table(kichwa_market$MARKET1, useNA = "ifany")
table(kichwa_market$MARKET12, useNA = "ifany")

kichwa_market <- kichwa_market %>%
  mutate(MARKET12 = ifelse(is.na(MARKET12), NA,
                           ifelse(MARKET12 == "Si" | MARKET12 == "Yes", MARKET1, MARKET12))) %>%
  mutate(MARKET1 = as.numeric(MARKET1)) %>%
  mutate(MARKET12 = as.numeric(MARKET12))

table(kichwa_market$MARKET1, useNA = "ifany")
table(kichwa_market$MARKET12, useNA = "ifany")

# Market 1
summary(kichwa_market$MARKET1)
sd(kichwa_market$MARKET1, na.rm = TRUE)

plot(density(kichwa_market$MARKET1, na.rm = TRUE), main = "Food from market yesterday", xlab = "%")
hist(kichwa_market$MARKET1, main = "Food from market yesterday", xlab = "%")

# Market 2
summary(kichwa_market$MARKET12)
sd(kichwa_market$MARKET12, na.rm = TRUE)

plot(density(kichwa_market$MARKET12, na.rm = TRUE), main = "Food from market in past month", xlab = "%")
hist(kichwa_market$MARKET12, main = "Food from market in past month", xlab = "%")

# Correlation between market variables
cor.test(kichwa_market$MARKET1, kichwa_market$MARKET12)

