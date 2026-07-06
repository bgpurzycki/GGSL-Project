### BaYaka - Objective 1; Study 1 - Quantitative Ethnography of BaYaka Religious System
### Created by Martin Kocsis
### R 4.4.1

## Install and load packages
rm(list=ls())
Sys.setenv(LANG = "en")

# For BRMS, make sure that Stan is installed
#install.packages("brms")
library(brms)

#install.packages("readxl")
library(readxl)

#install.packages("AnthroTools")
library(AnthroTools)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("ordbetareg")
library(ordbetareg)

#install.packages("ggdist")
library(ggdist)

#install.packages("igraph")
library(igraph)

#install.packages("scales")
library(scales)


#### Read and prepare free-list data

setwd("C:/Users/au776065/Dropbox/Major-Smith-Purzycki Shared/Projects/Martin_BaYaka/Objective 1 - BaYaka/Prereg/Objective 1 - BaYaka")
#setwd("")

rli <- read_excel("RLI_bayaka_forO1Study1.xlsx", sheet = "RLI")

head(rli)
str(rli)


###################################################################
#### Smith's S cultural salience
rli_salience <- rli %>%
  select(ID, ORDER, GOD, RANKGOD) %>%
  mutate(
    GOD = str_trim(GOD)
  ) %>%
  filter(GOD != "NA")

rli_salience_df <- as.data.frame(rli_salience)

# Calculate Smith's S with AnthroTools
rli_salience_rows <- CalculateSalience(
  rli_salience_df,
  Subj = "ID",
  Order = "ORDER",
  CODE = "GOD"
)

rli_smiths <- SalienceByCode(
  rli_salience_rows,
  Subj = "ID",
  CODE = "GOD",
  Salience = "Salience",
  dealWithDoubles = "MAX"
)

rli_smiths_clean <- rli_smiths %>%
  mutate(
    CODE = recode(
      CODE,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi"
    )
  ) %>%
  arrange(desc(SmithsS))

# Bayesian salience estimation
rli_max_salience <- FreeListTable(
  rli_salience_rows,
  Subj = "ID",
  CODE = "GOD",
  Salience = "Salience",
  tableType = "MAX_SALIENCE"
)

rli_bayes_top8 <- SalienceOrdBeta(
  data = rli_max_salience,
  var_sel = "TOP",
  top = 8,
  seed = 182,
  chains = 4,
  iterations = 4000,
  warmup = 2000,
  cores = 4,
  IDs_first = TRUE,
  print_model = TRUE
)

rli_bayes_summary_top8 <- SalienceEstimateSummary(
  rli_bayes_top8,
  quantiles = c(0.025, 0.5, 0.975)
)

rli_bayes_summary_top8 <- as.data.frame(rli_bayes_summary_top8)

rli_bayes_summary_top8$Spirit <- rownames(rli_bayes_summary_top8)

rownames(rli_bayes_summary_top8) <- NULL

rli_bayes_summary_top8 <- rli_bayes_summary_top8 %>%
  relocate(Spirit) %>%
  mutate(
    Spirit = recode(
      Spirit,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi"
    )
  )

# Extract posterior draws for plotting
p_top8 <- SalienceEstimatePlot(
  rli_bayes_top8,
  order = "high-low"
)

plot_draws_top8 <- p_top8$data %>%
  mutate(
    item = as.character(item),
    item = recode(
      item,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi",
      .default = item
    )
  )


# Sample-size-corrected female-bias score
respondent_sex <- rli_salience %>%
  group_by(ID) %>%
  summarise(
    female_inferred = any(GOD %in% c("Gonku", "Eki")),
    .groups = "drop"
  ) %>%
  mutate(
    sex = if_else(female_inferred, "female", "male")
  )

total_females <- sum(respondent_sex$sex == "female")
total_males   <- sum(respondent_sex$sex == "male")

sex_bias_top8 <- rli_salience %>%
  left_join(respondent_sex, by = "ID") %>%
  mutate(
    item = recode(
      GOD,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi",
      .default = GOD
    )
  ) %>%
  filter(item %in% c("Ejengi", "Bolobe", "Komba", "Ngoku",
                     "Eki", "Mandopa", "Lyoko", "Mipudza")) %>%
  group_by(item) %>%
  summarise(
    female_mentions = sum(sex == "female"),
    male_mentions   = sum(sex == "male"),
    female_rate = female_mentions / total_females,
    male_rate   = male_mentions / total_males,
    female_bias = female_rate / (female_rate + male_rate),
    .groups = "drop"
  )


#salience plot with sex-bias gradient
plot_draws_top8 <- plot_draws_top8 %>%
  left_join(sex_bias_top8, by = "item") %>%
  mutate(
    item = factor(
      item,
      levels = c("Mipudza", "Lyoko", "Mandopa", "Eki",
                 "Ngoku", "Komba", "Bolobe", "Ejengi")
    )
  )

p_gradient_top8 <- ggplot(
  plot_draws_top8,
  aes(x = S, y = item, fill = female_bias)
) +
  ggdist::stat_halfeye(
    alpha = 0.65,
    slab_color = NA,
    point_color = "black",
    interval_color = "black",
    .width = c(0.50, 0.95),
    point_interval = median_qi
  ) +
  scale_fill_gradient(
    low = "darkgreen",
    high = "cyan1",
    limits = c(0, 1),
    name = "female-biased"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    x = "Ŝ cultural salience",
    y = "Item"
  ) +
  theme_minimal()

p_gradient_top8

ggsave(
  filename = "bayaka_salience_gradient_top8.png",
  plot = p_gradient_top8,
  width = 10,
  height = 7.5,
  dpi = 600,
  bg = "white"
)


###################################################################
# Perceived power/importance estimation conditional on mention
# RANKGOD = power/importance order
top8_spirits <- rli_smiths_clean %>%
  slice_max(SmithsS, n = 8) %>%
  pull(CODE)

top8_spirits <- dplyr::recode(
  top8_spirits,
  "Gonku"   = "Ngoku",
  "Edjengi" = "Ejengi",
  .default = top8_spirits
)

# Prep power-ranking data
rli_power_all <- rli %>%
  select(ID, GOD, RANKGOD) %>%
  mutate(
    GOD = str_trim(GOD),
    GOD = dplyr::recode(
      GOD,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi",
      .default = GOD
    ),
    RANKGOD = readr::parse_number(as.character(RANKGOD))
  ) %>%
  filter(
    !is.na(GOD),
    GOD != "NA",
    !is.na(RANKGOD)
  )

rli_power_all_df <- as.data.frame(rli_power_all)

# Rank-based salience from RANKGOD
rli_power_rows <- CalculateSalience(
  rli_power_all_df,
  Subj = "ID",
  Order = "RANKGOD",
  CODE = "GOD"
)

# Manually add in salience for RLM06, as ranked two deities as equally powerful.
rli_power_rows$Salience[rli_power_rows$ID == "RLM06" & rli_power_rows$RANKGOD == 1] <- 1
rli_power_rows$Salience[rli_power_rows$ID == "RLM06" & rli_power_rows$RANKGOD == 2] <- 0.8
rli_power_rows$Salience[rli_power_rows$ID == "RLM06" & rli_power_rows$RANKGOD == 3] <- 0.6
rli_power_rows$Salience[rli_power_rows$ID == "RLM06" & rli_power_rows$RANKGOD == 4] <- 0.4
rli_power_rows$Salience[rli_power_rows$ID == "RLM06" & rli_power_rows$RANKGOD == 5] <- 0.2

# Bayesian salience estimation
rli_max_power <- FreeListTable(
  rli_power_rows,
  Subj = "ID",
  CODE = "GOD",
  Salience = "Salience",
  tableType = "MAX_SALIENCE"
)

# Keep just the top 8 most salient deities, then code as NA if not named by participant
rli_max_power <- rli_max_power %>%
  select(c("Subject", all_of(top8_spirits))) %>%
  mutate(Ejengi = ifelse(Ejengi == 0, NA, Ejengi)) %>%
  mutate(Bolobe = ifelse(Bolobe == 0, NA, Bolobe)) %>%
  mutate(Komba = ifelse(Komba == 0, NA, Komba)) %>%
  mutate(Ngoku = ifelse(Ngoku == 0, NA, Ngoku)) %>%
  mutate(Eki = ifelse(Eki == 0, NA, Eki)) %>%
  mutate(Mandopa = ifelse(Mandopa == 0, NA, Mandopa)) %>%
  mutate(Lyoko = ifelse(Lyoko == 0, NA, Lyoko)) %>%
  mutate(Mipudza = ifelse(Mipudza == 0, NA, Mipudza))

## Loop over all deities, storing the salience of the 'power' for each

# Empty list to store results in
res_list <- list()

# Modelling deets
cut_no0s <- -10
cut_no1s <- 10
chains <- 4
iterations <- 4000
warmup <- 2000
cores <- 4
print_model <- TRUE

# Select the top X variables in terms of Smith's S. Vary depending on whether IDs are in first column or not
x <- colMeans(rli_max_power[, -1], na.rm = TRUE)

x <- x[order(x, decreasing = TRUE)] # Order by Smith's S
vars <- attributes(x)$names # Extract variable names

# Setting seed
set.seed(182)

# Loop over each of the variables, first checking for 0s and 1s, then running the appropriate ordered Beta model
for (i in 1:length(vars)) { 
  
  var <- vars[i] # Take each variable
  print(paste0("On variable ", i, ": ", var))
  
  # Make variable into a dataframe and remove NAs
  df_temp <- as.data.frame(cbind(v1 = rli_max_power[[var]]))
  df_temp <- df_temp[!is.na(df_temp)]
  df_temp <- as.data.frame(cbind(v1 = df_temp))
  
  # See if 0s and 1s, to run appropriate ordered Beta model
  any_0s <- ifelse(min(df_temp$v1) == 0, TRUE, FALSE)
  any_1s <- ifelse(max(df_temp$v1) == 1, TRUE, FALSE)
  
  model_type <- ifelse(any_0s == TRUE & any_1s == TRUE, "Standard", 
                       ifelse(any_0s == TRUE & any_1s == FALSE, "No 1s",
                              ifelse(any_0s == FALSE & any_1s == TRUE, "No 0s", "No 0s or 1s")))
  
  ## Run appropriate ordered Beta model
  
  # Specify weakly-informative priors for phi intercept term
  priors <- c(prior(student_t(3, 0, 2.5), class = Intercept, dpar = phi))
  
  if (model_type == "Standard") {
    
    print(paste0("There are both 0s and 1s for variable ", var, ". Running standard Ordered Beta."))
    
    ordBeta_mod <- ordbetareg(formula = bf(v1 ~ 1, phi ~ 1), 
                              data = df_temp,
                              true_bounds = c(0, 1),
                              manual_prior = priors,
                              phi_reg = "only",
                              chains = chains, iter = iterations, warmup = warmup, cores = cores)
    
  } else if (model_type == "No 1s") {
    
    print(paste0("There are 0s but no 1s for variable ", var, ". Running Ordered Beta with one cut-point term fixed to ", cut_no1s, " (i.e., a one value is almost impossible)."))
    
    ordBeta_mod <- ordbetareg(formula = bf(v1 ~ 1, phi ~ 1, cutone = cut_no1s), 
                              data = df_temp,
                              true_bounds = c(0, 1),
                              manual_prior = priors,
                              phi_reg = "only",
                              chains = chains, iter = iterations, warmup = warmup, cores = cores)
    
  } else if (model_type == "No 0s") {
    
    print(paste0("There are 1s but no 0s for variable ", var, ". Running Ordered Beta with zero cut-point term fixed to ", cut_no0s, " (i.e., a zero value is almost impossible)."))
    
    ordBeta_mod <- ordbetareg(formula = bf(v1 ~ 1, phi ~ 1, cutzero = cut_no0s), 
                              data = df_temp,
                              true_bounds = c(0, 1),
                              manual_prior = priors,
                              phi_reg = "only",
                              chains = chains, iter = iterations, warmup = warmup, cores = cores)
    
  } else if (model_type == "No 0s or 1s") {
    
    print(paste0("There are no 0s and no 1s for variable ", var, ". Running Ordered Beta with zero and one cut-point terms fixed to ", cut_no0s, " and ", cut_no1s, ", respectively (i.e., a standard beta model)."))
    
    ordBeta_mod <- ordbetareg(formula = bf(v1 ~ 1, phi ~ 1, cutzero = cut_no0s, cutone = cut_no1s), 
                              data = df_temp,
                              true_bounds = c(0, 1),
                              manual_prior = priors,
                              phi_reg = "only",
                              chains = chains, iter = iterations, warmup = warmup, cores = cores)
    
  }
  
  if (print_model == TRUE) {
    print(ordBeta_mod)
  }
  
  # Posterior predictions from this model
  post_ordBeta <- predict(ordBeta_mod, summary = FALSE)
  
  # Smith's S in each posterior sample
  S_post_ordBeta <- rep(NA, nrow(post_ordBeta))
  for (j in 1:nrow(post_ordBeta)) {
    S_post_ordBeta[j] <- mean(post_ordBeta[j, ])
  }
  
  # Store results in list
  res_list[[i]] <- S_post_ordBeta
  names(res_list)[i] <- var
}

# Summary of estimates
SalienceEstimateSummary(res_list, quantiles = c(0.025, 0.5, 0.975))

# Make long format and add in sex bias in deity nomination
draws_power_top8 <- as.data.frame(res_list)

plot_draws_power_top8 <- draws_power_top8 %>%
  pivot_longer(cols = everything(), names_to = "item", values_to = "S") %>%
  arrange(item) %>%
  left_join(sex_bias_top8, by = "item") %>%
  mutate(item = factor(item, levels = rev(names(draws_power_top8))))
plot_draws_power_top8

# Plot of perceived power/importance conditional on mention
p_power_gradient_top8 <- ggplot(
  plot_draws_power_top8,
  aes(x = S, y = item, fill = female_bias)
) +
  ggdist::stat_halfeye(
    alpha = 0.65,
    slab_color = NA,
    point_color = "black",
    interval_color = "black",
    .width = c(0.50, 0.95),
    point_interval = median_qi, density = "unbounded"
  ) +
  scale_fill_gradient(
    low = "darkgreen",
    high = "cyan1",
    limits = c(0, 1),
    name = "female-biased"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    x = "Perceived power conditional on mention",
    y = "Item"
  ) +
  theme_minimal()

p_power_gradient_top8

ggsave(
  filename = "bayaka_power_conditional_gradient_top8.png",
  plot = p_power_gradient_top8,
  width = 10,
  height = 7.5,
  dpi = 600,
  bg = "white"
)


###########################################################
### Bar plot of traits for top 5 deities

top5_spirits <- c(
  "Ejengi",
  "Bolobe",
  "Komba",
  "Ngoku",
  "Eki"
)

# Prep data
trait_plot_data <- rli %>%
  select(ID, GOD, CONCERN, SEE, PUNISH, REWARD) %>%
  mutate(
    GOD = str_trim(GOD),
    GOD = recode(
      GOD,
      "Gonku" = "Ngoku",
      "Edjengi" = "Ejengi",
      .default = GOD
    ),
    CONCERN = parse_number(as.character(CONCERN)),
    SEE     = parse_number(as.character(SEE)),
    PUNISH  = parse_number(as.character(PUNISH)),
    REWARD  = parse_number(as.character(REWARD))
  ) %>%
  filter(
    !is.na(GOD),
    GOD != "NA",
    GOD %in% top5_spirits
  ) %>%
  group_by(GOD) %>%
  summarise(
    Punishing = mean(PUNISH, na.rm = TRUE) * 100,
    Omniscience = mean(SEE, na.rm = TRUE) * 100,
    `Morally concerned` = mean(CONCERN, na.rm = TRUE) * 100,
    Rewarding = mean(REWARD, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      Punishing,
      Omniscience,
      `Morally concerned`,
      Rewarding
    ),
    names_to = "Trait",
    values_to = "Percent"
  ) %>%
  mutate(
    GOD = factor(
      GOD,
      levels = rev(top5_spirits)
    ),
    Trait = factor(
      Trait,
      levels = c(
        "Punishing",
        "Omniscience",
        "Morally concerned",
        "Rewarding"
      )
    )
  )

# Plot
p_traits_top5 <- ggplot(
  trait_plot_data,
  aes(
    x = Percent,
    y = GOD,
    fill = Trait
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Punishing" = "black",
      "Omniscience" = "gray45",
      "Morally concerned" = "gray70",
      "Rewarding" = "gray85"
    ),
    breaks = c(
      "Rewarding",
      "Morally concerned",
      "Omniscience",
      "Punishing"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    x = "% of participants attributing trait",
    y = "Deity",
    fill = "Trait"
  ) +
  theme_minimal()

p_traits_top5

ggsave(
  filename = "bayaka_deity_traits_top5_horizontal.png",
  plot = p_traits_top5,
  width = 10,
  height = 7.5,
  dpi = 600,
  bg = "white"
)


#########################################################
#### Semantic network plot of deities

# Prepare top-12 salience items
TOP_N <- 12

rli_sal_items <- rli_smiths %>%
  as.data.frame() %>%
  rename(
    GOD_clean     = CODE,
    mean_salience = MeanSalience,
    sum_salience  = SumSalience,
    smith_s       = SmithsS
  ) %>%
  mutate(
    GOD_display = recode(
      GOD_clean,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi",
      .default  = GOD_clean
    )
  ) %>%
  arrange(desc(smith_s))

top_items <- rli_sal_items %>%
  slice_head(n = TOP_N) %>%
  pull(GOD_clean)

rli_fl_top <- rli_salience %>%
  filter(GOD %in% top_items) %>%
  mutate(
    GOD_clean = GOD,
    GOD_display = recode(
      GOD_clean,
      "Gonku"   = "Ngoku",
      "Edjengi" = "Ejengi",
      .default  = GOD_clean
    )
  )


# Sample-size-corrected female-bias score
female_base_rate <- mean(respondent_sex$sex == "female")

gender_summary <- rli_fl_top %>%
  select(ID, GOD_clean) %>%
  distinct() %>%
  left_join(respondent_sex, by = "ID") %>%
  group_by(GOD_clean) %>%
  summarise(
    n_listers   = n(),
    prop_female = mean(sex == "female"),
    prop_male   = mean(sex == "male"),
    .groups = "drop"
  ) %>%
  mutate(
    female_bias_scaled = case_when(
      prop_female >= female_base_rate ~
        0.5 + 0.5 * ((prop_female - female_base_rate) / (1 - female_base_rate)),
      prop_female < female_base_rate ~
        0.5 * (prop_female / female_base_rate)
    )
  )


# Perceived power conditional on mention--how powerful is the deity perceived among people who recognize it
rank_denominators <- rli_salience %>%
  mutate(RANKGOD = as.numeric(RANKGOD)) %>%
  group_by(ID) %>%
  summarise(
    max_rank_in_full_list = max(RANKGOD, na.rm = TRUE),
    .groups = "drop"
  )

power_rows <- rli_fl_top %>%
  mutate(RANKGOD = as.numeric(RANKGOD)) %>%
  left_join(rank_denominators, by = "ID") %>%
  mutate(
    power_priority = if_else(
      !is.na(RANKGOD) & max_rank_in_full_list > 1,
      (max_rank_in_full_list - RANKGOD) / (max_rank_in_full_list - 1),
      if_else(!is.na(RANKGOD) & max_rank_in_full_list == 1, 1, NA_real_)
    )
  )

power_summary <- power_rows %>%
  filter(!is.na(power_priority)) %>%
  group_by(GOD_clean) %>%
  summarise(
    mean_power_rank             = mean(RANKGOD, na.rm = TRUE),
    median_power_rank           = median(RANKGOD, na.rm = TRUE),
    power_conditional           = mean(power_priority, na.rm = TRUE),
    power_conditional_median    = median(power_priority, na.rm = TRUE),
    power_top_share             = mean(RANKGOD == 1, na.rm = TRUE),
    power_n                     = n(),
    .groups = "drop"
  )


# Nodes and co-mention edges
nodes <- rli_sal_items %>%
  filter(GOD_clean %in% top_items) %>%
  left_join(gender_summary, by = "GOD_clean") %>%
  left_join(power_summary, by = "GOD_clean") %>%
  arrange(desc(smith_s))

pair_list <- rli_fl_top %>%
  select(ID, GOD_clean) %>%
  distinct() %>%
  group_by(ID) %>%
  summarise(
    items = list(sort(unique(GOD_clean))),
    .groups = "drop"
  )

make_pairs_one_list <- function(x) {
  if(length(x) < 2) return(NULL)
  
  combn(x, 2, simplify = FALSE) %>%
    lapply(function(pair) {
      tibble(
        from = pair[1],
        to   = pair[2]
      )
    }) %>%
    bind_rows()
}

edges <- bind_rows(lapply(pair_list$items, make_pairs_one_list)) %>%
  count(from, to, name = "weight") %>%
  arrange(desc(weight))


# Network layout
g_rli <- graph_from_data_frame(
  d = edges,
  vertices = nodes %>% rename(name = GOD_clean),
  directed = FALSE
)

set.seed(182)

lay <- layout_with_fr(
  g_rli,
  weights = E(g_rli)$weight,
  niter = 4000
)

layout_df <- as.data.frame(lay)
names(layout_df) <- c("x", "y")
layout_df$GOD_clean <- V(g_rli)$name

nodes_plot <- nodes %>%
  left_join(layout_df, by = "GOD_clean") %>%
  mutate(
    x = scales::rescale(x, to = c(-2.8, 2.8)),
    y = scales::rescale(y, to = c(-1.75, 1.75)),
    power_ring = scales::rescale(
      power_conditional,
      to = c(0.06, 0.24),
      from = c(0, 1)
    )
  )

# Plot
p_rli_network <- ggplot(nodes_plot, aes(x = x, y = y)) +
  
  geom_point(
    aes(
      size = smith_s,
      colour = female_bias_scaled
    ),
    alpha = 0.60,
    shape = 16,
    stroke = 0,
    show.legend = TRUE
  ) +
  
  geom_point(
    aes(size = power_ring),
    shape = 1,
    stroke = 0.50,
    colour = "grey35",
    alpha = 1,
    show.legend = FALSE
  ) +
  
  geom_text(
    aes(label = GOD_display),
    size = 4.8,
    color = "grey10"
  ) +
  
  scale_size_area(
    max_size = 100,
    guide = "none"
  ) +
  
  scale_colour_gradient(
    low = "darkgreen",
    high = "cyan1",
    limits = c(0, 1),
    name = "female-biased"
  ) +
  
  coord_equal(
    xlim = c(-2.5, 2.5),
    ylim = c(-1.8, 1.8),
    clip = "off"
  ) +
  
  theme_void(base_size = 14) +
  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "left", legend.margin = margin(b = -0.4, unit = "npc"),
    plot.margin = margin(40, 80, 40, 40)
  )

p_rli_network

ggsave(
  filename = "bayaka_network_top12.png",
  plot = p_rli_network,
  width = 12,
  height = 8,
  dpi = 600,
  bg = "white"
)
