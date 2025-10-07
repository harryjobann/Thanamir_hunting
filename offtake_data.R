

############################################################################################################
#--------------------------------------------Packages-------------------------
############################################################################################################

# Function to check if a package is installed, install it if necessary, and then load it
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of packages you need
packages <-
  c(
    "fpp3",
    "mgcv",
    "lubridate",
    "ggplot2",
    "fable",
    "fabletools",
    "urca",
    "feasts"
  )

# Apply the function to each package
invisible(lapply(packages, install_and_load))


############################################################################################################
#--------------------------------------------Read in data-------------------------
############################################################################################################


offtake_df <- read.csv("https://raw.githubusercontent.com/harryjobann/Thanamir_hunting/refs/heads/main/clean_offtake.csv")
sp_mass_values <- read.csv('https://raw.githubusercontent.com/harryjobann/Thanamir_hunting/refs/heads/main/Thanamir_species%20mass%20values.xlsx%20-%20Mass%20values.csv')




############################################################################################################
############################################################################################################
#--------------------------------------------offtake df -----------------------
############################################################################################################
############################################################################################################


# we can try to use the time series models with different granularity of time. First, we'll start with daily (the most detailed),
# then fit weekly and finally, monthly. After, we'll compare between the different models to see which offers the most value. 



############################################################################################################
#--------------------------------------------Data prep------------------------
############################################################################################################

# Convert the date column to Date format
offtake_df <- offtake_df %>%
  dplyr::mutate(date = lubridate::dmy(date))

# Set start date as earliest date in the dataset
start_date <- min(offtake_df$date, na.rm = TRUE)

# Also find mid point and end date
end_date <- max(offtake_df$date, na.rm = TRUE)
mid_date <- start_date + as.integer(difftime(end_date, start_date, units = "days") / 2)

# Print the dates
cat("Start date:", start_date, "\n")
cat("Mid point :", mid_date, "\n")
cat("End date  :", end_date, "\n")

# Create month and week indices
offtake_df <- offtake_df %>%
  dplyr::mutate(
    month_index = (lubridate::year(date) - lubridate::year(start_date)) * 12 +
      lubridate::month(date) - lubridate::month(start_date) + 1,
    week_index = as.integer(difftime(date, start_date, units = "weeks")) + 1
  )

# Monthly offtake
monthly_offtake <- offtake_df %>%
  dplyr::group_by(month_index) %>%
  dplyr::summarise(total_individuals = sum(individuals), .groups = "drop") %>%
  dplyr::arrange(month_index)

# Weekly offtake
weekly_offtake <- offtake_df %>%
  dplyr::group_by(week_index) %>%
  dplyr::summarise(total_individuals = sum(individuals), .groups = "drop") %>%
  dplyr::arrange(week_index)



########################################################
# daily offtake 
########################################################

daily_offtake <- offtake_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  count(date, wt = individuals, name = "total_offtake") %>%
  as_tsibble(index = date) %>%
  fill_gaps(total_offtake = 0)



acf_values <- feasts::ACF(daily_offtake, total_offtake)
autoplot(acf_values)




# Fit ARIMA with auto selection
model_daily_arima <- daily_offtake %>%
  model(ARIMA(total_offtake))

report(model_daily_arima)



gg_tsresiduals(model_daily_arima)



model_auto <- daily_offtake %>%
  model(auto = ARIMA(total_offtake))

report(model_auto)





########################################################
# weekly offtake 
########################################################


# Aggregating to weekly time series and trying again


# Convert the df into a tsibble with a numeric index
# Fill in any missing weeks with NA for total_individuals
weekly_offtake_ts <- weekly_offtake %>%
  as_tsibble(index = week_index) %>%
  tsibble::fill_gaps(total_individuals = NA)



model_weekly_arima <- weekly_offtake_ts %>%
  model(ARIMA(total_individuals))


report(model_weekly_arima)


model_weekly_arima %>%
  augment() %>%
  ggplot(aes(x = week_index)) +
  geom_line(aes(y = total_individuals), colour = "grey40") +
  geom_line(aes(y = .fitted), colour = "blue", linewidth = 1) +
  labs(title = "ARIMA Fit to Weekly Hunting Offtake",
       x = "Week Index", y = "Individuals Hunted") +
  theme_minimal()


gg_tsresiduals(model_weekly_arima)



########################################################
# monthly offtake 
########################################################

# and finally, an offtake time series on a monthly basis

monthly_offtake_ts <- monthly_offtake %>%
  arrange(month_index) %>%
  tsibble::as_tsibble(index = month_index) %>%
  tsibble::fill_gaps(total_individuals = NA)


model_monthly_arima <- monthly_offtake_ts %>%
  model(ARIMA(total_individuals))


report(model_monthly_arima)


model_monthly_arima %>%
  augment() %>%
  ggplot(aes(x = month_index)) +
  geom_line(aes(y = total_individuals), colour = "grey40") +
  geom_line(aes(y = .fitted), colour = "darkblue", linewidth = 1) +
  labs(title = "ARIMA Model Fit to Monthly Offtake",
       x = "Month Index", y = "Individuals Hunted") +
  theme_minimal()



########################################################
# Comparing between models 
########################################################


report(model_monthly_arima)
report(model_weekly_arima)
report(model_daily_arima)


# Comment on our 3 models:

# DAILY: We used an ARIMA model on the daily hunting data. The daily data was v. noisy and unpredictable, making
# it harder to trust the daily model for interpretation or forecasting.


# WEEKLY: The weekly data provides us with a much better model. The model detects short-term patterns and gives a more structured 
# fit than the daily version. 


# MONTHLY: This model fit to the monthly data is very simple and only includes a constant, which suggests it
# doesn't pick up much structure. Likely because there are too few data points to detect clear trends or patterns.


# for now, we'll stick with the weekly data and proceed...




########################################################
# Pushing the weekly model further 
########################################################



gg_tsresiduals(model_weekly_arima)

# From looking at the residuals, it looks like they still show some skew and large spikes, which suggests that a few extreme 
# weeks may be influencing the model. We can try to fix this by log transforming the data.


# Transform with log1p
weekly_offtake_log <- weekly_offtake_ts %>%
  dplyr::mutate(log_offtake = log1p(total_individuals))

# Fit ARIMA to log transformed data
model_weekly_log_arima <- weekly_offtake_log %>%
  model(ARIMA(log_offtake))

# Report model details
report(model_weekly_log_arima)


# plot
model_weekly_log_arima %>%
  augment() %>%
  ggplot(aes(x = week_index)) +
  geom_line(aes(y = log_offtake), colour = "grey40") +
  geom_line(aes(y = .fitted), colour = "darkgreen", linewidth = 1) +
  labs(title = "ARIMA Fit to log(1 + Weekly Offtake)",
       x = "Week Index", y = "log(1 + Individuals Hunted)") +
  theme_minimal()


# check residuals
gg_tsresiduals(model_weekly_log_arima)


# Much closer to normal now — more symmetrical and centred around 0.
# The large spike near zero is reduced, and the long right tail is largely gone.
# Still a slight left skew (some very negative residuals), but vastly improved from before.


# Therefore log transforming the data improved things, as Dan suggested. 
# The log1p() transformation:
#   Reduced extreme values and stabilised variance.
#   Helped produce more normally distributed and uncorrelated residuals.
#   Made the ARIMA model a more appropriate fit for the weekly offtake data.




# Now let's try to understand the impact of species on the patterns...




# Modelling by species

species_ts <- offtake_df %>%
  dplyr::group_by(species, week_index) %>%
  dplyr::summarise(weekly_total = sum(individuals), .groups = "drop") %>%
  dplyr::group_by(species) %>%
  tidyr::complete(week_index = full_seq(week_index, 1), fill = list(weekly_total = 0)) %>%
  tsibble::as_tsibble(key = species, index = week_index) %>%
  dplyr::mutate(log_total = log1p(weekly_total))

models_by_species <- species_ts %>%
  model(ARIMA(log_total))

models_by_species %>% report()



models_by_species %>%
  dplyr::filter(species %in% c("birds", "barking_deer", "assamese_macaque", "civet")) %>%
  forecast(h = 5) %>%
  autoplot(species_ts)


# 
# # Some species do not have enough data for this approach to succeed. Therefore, we'll group into taxonomic groups and retry.
# 
# offtake_df <- offtake_df %>%
#   dplyr::mutate(
#     taxon_group = dplyr::case_when(
#       # Primates
#       species %in% c("assamese_macaque", "macaque_sp", "stump_tailed_macaque") ~ "primate",
#       
#       # Ungulates
#       species %in% c("barking_deer", "serow", "wild_boar") ~ "ungulate",
#       
#       # Rodents
#       species %in% c("giant_squirrel", "squirrel", "flying_squirrel", "bamboo_rat", "brush_tailed_porcupine", "malayan_porcupine ") ~ "rodent",
#       
#       # Carnivores
#       species %in% c("civet", "himalayan_palm_civet", "leopard_cat", "dhole", "spotted_linsang") ~ "carnivore",
#       
#       # Birds
#       species %in% c("birds", "bamboo_partridge", "kalij_pheasant", "hill_partridge", "green_pigeon") ~ "bird",
#       
#       # Fish & Amphibians
#       species == "fish" ~ "fish",
#       species == "frogs" ~ "amphibian",
#       
#       # Default catch-all
#       TRUE ~ "other"
#     )
#   )
# 






# -----------------------------------------------------
# Regroup species into categories based on size 
# -----------------------------------------------------

unique(offtake_df$species)



offtake_df <- offtake_df %>%
  dplyr::mutate(
    size_group = dplyr::case_when(
      # Fish & amphibians combined
      species %in% c("fish", "frogs") ~ "fish",
      
      # Birds split by size
      species %in% c("kalij_pheasant") ~ "large_bird",
      species %in% c("birds", "green_pigeon", "bamboo_partridge", "hill_partridge") ~ "small_bird",
      
      # Large mammals (explicit list)
      species %in% c("barking_deer", "serow", "wild_boar", "dhole",
                     "malayan_porcupine", "malayan_porcupine ") ~ "large_mammal",
      
      # Everything else defaults to small mammals for this dataset
      TRUE ~ "small_mammal"
    )
  )

# Check the new grouping
table(offtake_df$size_group, useNA = "ifany")
unique(offtake_df$species)

# Time series prep and modelling (unchanged aside from new groups)
weekly_taxa <- offtake_df %>%
  dplyr::group_by(size_group, week_index) %>%
  dplyr::summarise(total = sum(individuals), .groups = "drop") %>%
  dplyr::mutate(log_total = log1p(total)) %>%
  tsibble::as_tsibble(key = size_group, index = week_index) %>%
  tsibble::fill_gaps(total = 0, log_total = 0)

models_by_group <- weekly_taxa %>%
  model(ARIMA(log_total))

models_by_group %>%
  forecast(h = 5) %>%
  autoplot(weekly_taxa)






# Sahil comment: move away from panel - group them into one plate with diff plate. Also add confidence internals shaded around.
# edits to groups - one for carnivore

# Understand more about the impact of seasonality - check with Ramya about importance of each season. 




########################################################
# EDIT: Using species mass values in the time series 
########################################################


# standardise species names in both data frames 
offtake_df <- offtake_df |>
  dplyr::mutate(species = stringr::str_squish(tolower(species)))

sp_mass_values <- sp_mass_values |>
  dplyr::rename(
    species_raw = dplyr::matches("^species$", ignore.case = TRUE),
    mass_kg_raw = dplyr::matches("mass", ignore.case = TRUE)
  ) |>
  dplyr::mutate(
    species_raw = stringr::str_squish(species_raw),
    mass_kg = as.numeric(mass_kg_raw)
  )

# Map the names across each dataset (name standardisation only; no mass overrides)
name_map <- tibble::tribble(
  ~species_raw,                 ~species,
  "Asiatic wild dog",           "dhole",
  "Assamaese macaque",          "assamese_macaque",
  "Barking deer",               "barking_deer",
  "Brush tailed porcupine",     "brush_tailed_porcupine",
  "Hill partridge",             "hill_partridge",
  "Himalayan black bear",       "himalayan_black_bear",
  "Himalayan palm civet",       "himalayan_palm_civet",
  "Leopard cat",                "leopard_cat",
  "Red serow",                  "serow",
  "Stumped tailed macaque",     "stump_tailed_macaque",
  "Yellow throated marten",     "yellow_throated_marten"
)

# Merge the map into sp_mass_values, fixing names only (no overrides)
sp_mass_values <- sp_mass_values |>
  dplyr::left_join(name_map, by = "species_raw") |>
  dplyr::mutate(
    # Prefer the mapped canonical name when available; otherwise use cleaned original
    species = dplyr::coalesce(species, stringr::str_squish(tolower(species_raw)))
  ) |>
  dplyr::select(species, mass_kg) |>
  dplyr::arrange(species) |>
  dplyr::distinct()

# For any species in offtake_df that still aren’t present in sp_mass_values, add rows with NA
missing_in_mass <- setdiff(unique(offtake_df$species), sp_mass_values$species)
if (length(missing_in_mass) > 0) {
  sp_mass_values <- dplyr::bind_rows(
    sp_mass_values,
    tibble::tibble(species = missing_in_mass, mass_kg = NA_real_)
  ) |>
    dplyr::arrange(species)
}

# Quick check: which species still have NA mass?
na_mass <- sp_mass_values |>
  dplyr::filter(is.na(mass_kg)) |>
  dplyr::arrange(species)
print(na_mass)

# Ensure "green_pigeon" is explicitly in the birds category 
taxon_cols <- intersect(c("category", "taxon", "taxa", "taxon_group"), names(offtake_df))
if (length(taxon_cols) > 0) {
  for (col in taxon_cols) {
    offtake_df[[col]] <- ifelse(offtake_df$species == "green_pigeon", "birds", offtake_df[[col]])
  }
}




# There are a few species with missing mass values, so i'll dig rough estimates for for adult body mass.

# New literature-based estimates for named species only (Ofc, feel free to edit!)
mass_lookup <- tibble::tribble(
  ~species,             ~mass_kg,
  "bamboo_partridge",     0.33,
  "bamboo_rat",           0.65,
  "giant_squirrel",       1.20,
  "kalij_pheasant",       0.80,
  "malayan_porcupine",   13.00,
  "spotted_linsang",      0.90,
  "wild_boar",           70.00,
  "civet",                4.30,
  "flying_squirrel",      1.75,
  "macaque_sp",           8.00,
  "squirrel",             0.35
)



# put into the existing table 
sp_mass_values <- sp_mass_values %>%
  dplyr::anti_join(mass_lookup, by = "species") %>%
  dplyr::bind_rows(mass_lookup) %>%
  dplyr::arrange(species)

# again, quick check that the additional species now have mass values associated.
na_mass <- sp_mass_values |>
  dplyr::filter(is.na(mass_kg)) |>
  dplyr::arrange(species)
print(na_mass)




# Join data
offtake_df <- offtake_df |>
  dplyr::select(-dplyr::any_of("mass_kg")) |>
  dplyr::left_join(sp_mass_values, by = "species")

# Compute biomass (kg) = individuals * mass_kg
#    If mass is missing (generic categories), biomass will be NA 

offtake_df <- offtake_df |>
  dplyr::mutate(
    individuals = suppressWarnings(as.numeric(individuals)),
    biomass_kg  = individuals * mass_kg
  )

# Check data
offtake_df |>
  dplyr::summarise(
    n_rows               = dplyr::n(),
    pct_missing_mass     = round(mean(is.na(mass_kg)) * 100, 1),
    pct_missing_biomass  = round(mean(is.na(biomass_kg)) * 100, 1),
    total_biomass_kg_obs = round(sum(biomass_kg, na.rm = TRUE), 1)
  )




# Weekly biomass totals
weekly_biomass <- offtake_df |>
  dplyr::group_by(week_index) |>
  dplyr::summarise(total_biomass_kg = sum(biomass_kg, na.rm = TRUE), .groups = "drop") |>
  tsibble::as_tsibble(index = week_index) |>
  tsibble::fill_gaps(total_biomass_kg = 0)



# check distribution to see if worth log transforming

ggplot(weekly_biomass, aes(x = total_biomass_kg)) +
  geom_histogram(bins = 30, fill = "steelblue", colour = "white", alpha = 0.7) +
  geom_density(colour = "red", linewidth = 1) +
  labs(title = "Distribution of Weekly Biomass",
       x = "Total biomass (kg)", y = "Density") +
  theme_minimal()


# Distribution of weekly biomass is heavily right-skewed:
# - Most weeks have very little biomass harvested
# - A few weeks have extremely high totals (100–500 kg)

# 
# so lets log-transform biomass 

weekly_biomass <- weekly_biomass %>%
  dplyr::mutate(log_biomass = log1p(total_biomass_kg))



# Lets see if that worked 

weekly_biomass %>%
  tidyr::pivot_longer(cols = c(total_biomass_kg, log_biomass),
                      names_to = "scale", values_to = "value") %>%
  ggplot(aes(x = value, fill = scale)) +
  geom_histogram(bins = 30, colour = "white", alpha = 0.7) +
  facet_wrap(~ scale, scales = "free", ncol = 2) +
  labs(title = "Distribution of Weekly Biomass: Raw vs Log-transformed",
       x = "Biomass value", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")


# much better, now we can proceed with the time series. Before we do that, lets quickly use the "groups" we created earlier



# weekly biomass per size group
weekly_biomass_group <- offtake_df %>%
  dplyr::group_by(size_group, week_index) %>%
  dplyr::summarise(total_biomass_kg = sum(biomass_kg, na.rm = TRUE), .groups = "drop") %>%
  tsibble::as_tsibble(key = size_group, index = week_index) %>%
  tsibble::fill_gaps(total_biomass_kg = 0) %>%
  dplyr::mutate(log_biomass = log1p(total_biomass_kg))

# Fit ARIMA models to log-biomass per group
models_biomass_group <- weekly_biomass_group %>%
  fabletools::model(ARIMA(log_biomass))



weekly_biomass_group %>%
  autoplot(log_biomass) +
  ggplot2::labs(
    title = "Weekly biomass offtake by size/method group (log scale)",
    x = "Week index",
    y = "log1p(total biomass, kg)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~ size_group, ncol = 1, scales = "free_y")





########################################################
# Weekly ARIMA using log-transformed biomass
########################################################

# Step 1: Convert to tsibble
weekly_biomass_ts <- weekly_biomass %>%
  tsibble::as_tsibble(index = week_index) %>%
  tsibble::fill_gaps(total_biomass_kg = 0, log_biomass = 0)

# Step 2: Fit ARIMA model on log-transformed biomass
model_weekly_biomass <- weekly_biomass_ts %>%
  model(ARIMA(log_biomass))

# Step 3: Report model details
report(model_weekly_biomass)

# Step 4: Plot fitted values vs observed
model_weekly_biomass %>%
  augment() %>%
  ggplot(aes(x = week_index)) +
  geom_line(aes(y = log_biomass), colour = "grey40") +
  geom_line(aes(y = .fitted), colour = "darkblue", linewidth = 1) +
  labs(title = "ARIMA Fit to Weekly log(1 + Biomass Offtake)",
       x = "Week Index", y = "log1p(total biomass kg)") +
  theme_minimal()

# Step 5: Check residuals
gg_tsresiduals(model_weekly_biomass)








########################################################
# Compare ARIMA: counts vs biomass
########################################################

# Extract fitted values from both models
fits_counts <- model_weekly_log_arima %>%
  augment() %>%
  dplyr::mutate(type = "Counts (log individuals)",
                fitted = .fitted,
                observed = log_offtake)

fits_biomass <- model_weekly_biomass %>%
  augment() %>%
  dplyr::mutate(type = "Biomass (log kg)",
                fitted = .fitted,
                observed = log_biomass)

# Combine into one dataframe
fits_compare <- dplyr::bind_rows(fits_counts, fits_biomass)

# Plot side by side
ggplot(fits_compare, aes(x = week_index)) +
  geom_line(aes(y = observed), colour = "grey40") +
  geom_line(aes(y = fitted), colour = "blue", linewidth = 1) +
  facet_wrap(~ type, ncol = 1, scales = "free_y") +
  labs(title = "Weekly ARIMA fits: counts vs biomass",
       x = "Week index",
       y = "log-transformed value") +
  theme_minimal()

# Residuals: counts vs biomass
gg_tsresiduals(model_weekly_log_arima)
gg_tsresiduals(model_weekly_biomass)








# Compare ARIMA models (counts vs biomass) using fabletools::glance
counts_glance  <- fabletools::glance(model_weekly_log_arima)
biomass_glance <- fabletools::glance(model_weekly_biomass)

model_compare <- dplyr::bind_rows(
  dplyr::mutate(counts_glance,  Model = "Counts (log individuals)"),
  dplyr::mutate(biomass_glance, Model = "Biomass (log kg)")
) |>
  dplyr::select(Model, AIC, AICc, BIC, sigma2, log_lik)

model_compare


# ---------------------------------------------------
# Model comparison: Counts vs Biomass
# ---------------------------------------------------
# Comparing the time series using counts (number of individuals) and biomass (kg)

# Interpretation:
# - Counts remain a cleaner signal of short-term activity and seasonality.
# - Biomass is inherently more volatile, because a few heavy taxa can dominate weekly total
# - Use counts to track temporal patterns efficiently, and biomass to describe ecological impact.




# # ---------------------------------------------------
# # 4-week rolling mean smoothing and ARIMA refit
# # ---------------------------------------------------

# Why a 4-week rolling mean?
# Weekly biomass is spiky because a few heavy taxa can dominate single weeks. A 4-week mean reduces high-frequency 
# noise so underlying trends and seasonal signals are easier to see.

# This is preferred to a monthly total because the 4-week rolling mean smooths short-term spikes while keeping weekly 
# timing and resolution, whereas monthly totals collapse detail, and can shift or dilute real peaks.

# =========================
# Build raw and 4w-smoothed series by size_group
# =========================
weekly_biomass_by_group_raw <- offtake_df %>%
  dplyr::group_by(size_group, week_index) %>%
  dplyr::summarise(total_biomass_kg = sum(biomass_kg, na.rm = TRUE), .groups = "drop") %>%
  tsibble::as_tsibble(key = size_group, index = week_index) %>%
  tsibble::fill_gaps(total_biomass_kg = 0) %>%
  dplyr::mutate(
    log_biomass = log1p(total_biomass_kg),
    size_group = factor(size_group, levels = c("fish", "small_bird", "large_bird", "small_mammal", "large_mammal"))
  )

weekly_biomass_by_group_roll4 <- weekly_biomass_by_group_raw %>%
  dplyr::arrange(size_group, week_index) %>%
  dplyr::group_by(size_group) %>%
  dplyr::mutate(log_biomass_roll4 = zoo::rollmean(log_biomass, k = 4, align = "right", fill = NA_real_)) %>%
  dplyr::ungroup()

# =========================
# Plot: single panel, 4w mean by group (raw in background for context)
# =========================
weekly_biomass_by_group_roll4 %>%
  ggplot2::ggplot(ggplot2::aes(x = week_index, colour = size_group)) +
  ggplot2::geom_line(ggplot2::aes(y = log_biomass), linewidth = 0.4, alpha = 0.25, show.legend = FALSE) +
  ggplot2::geom_line(ggplot2::aes(y = log_biomass_roll4), linewidth = 1.0, na.rm = TRUE) +
  ggplot2::labs(
    title = "Weekly log biomass by size group, 4-week rolling mean",
    x = "Week index",
    y = "log(1 + biomass kg)",
    colour = "Group"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 panel.grid.minor = ggplot2::element_blank())

# =========================
# Did smoothing help? Fit-vs-forecast tests
# =========================

# 1) Hold-out forecast comparison (last h weeks)
h <- 8  # change if you prefer a different hold-out
last_week <- max(weekly_biomass_by_group_raw$week_index, na.rm = TRUE)
train_end <- last_week - h

train_raw   <- weekly_biomass_by_group_raw   %>% dplyr::filter(week_index <= train_end)
test_raw    <- weekly_biomass_by_group_raw   %>% dplyr::filter(week_index >  train_end)

train_roll4 <- weekly_biomass_by_group_roll4 %>% dplyr::filter(week_index <= train_end) %>% tidyr::drop_na(log_biomass_roll4)
test_roll4  <- weekly_biomass_by_group_roll4 %>% dplyr::filter(week_index >  train_end)

# Fit ARIMA to each series per group
fit_raw   <- train_raw   %>% model(ARIMA(log_biomass))
fit_roll4 <- train_roll4 %>% model(ARIMA(log_biomass_roll4))

# Forecast h steps and compute accuracy against their respective targets
fc_raw   <- fit_raw   %>% forecast(h = h)
fc_roll4 <- fit_roll4 %>% forecast(h = h)

acc_raw <- fabletools::accuracy(fc_raw, test_raw) %>%
  dplyr::transmute(size_group, Series = "Raw", RMSE, MAE, MAPE)

acc_roll4 <- fabletools::accuracy(fc_roll4, test_roll4) %>%
  dplyr::transmute(size_group, Series = "Roll4", RMSE, MAE, MAPE)

acc_compare_groups <- acc_raw %>%
  dplyr::left_join(acc_roll4, by = "size_group", suffix = c("_raw", "_roll4")) %>%
  dplyr::mutate(
    RMSE_delta = RMSE_roll4 - RMSE_raw,
    MAE_delta  = MAE_roll4  - MAE_raw,
    MAPE_delta = MAPE_roll4 - MAPE_raw
  ) %>%
  dplyr::arrange(size_group)

acc_compare_groups
# Interpretation: negative deltas mean the 4-week mean improved forecast accuracy for that group.

# 2) In-sample fit comparison (AIC, BIC, residual variance)
gl_raw   <- fabletools::glance(fit_raw)   %>% dplyr::mutate(Series = "Raw")
gl_roll4 <- fabletools::glance(fit_roll4) %>% dplyr::mutate(Series = "Roll4")

model_compare_groups <- dplyr::bind_rows(gl_raw, gl_roll4) %>%
  dplyr::select(size_group, Series, AIC, AICc, BIC, sigma2, log_lik) %>%
  dplyr::arrange(size_group, Series)

model_compare_groups


# These results are super clear, the 4-week mean gives a much cleaner signal in every group.
#
# 
# - AIC drops massively for Roll4 in all groups, meaning the smoothed series are far easier for ARIMA to model.
# - Residual variance shrinks by ~90 percent across the board, so weekly noise is largely tamed.





########################################################################
########################################################################
#     GLMM & GAM for offtake df
########################################################################
########################################################################

# At this point, we may have pushed the time series approach as far as we can, so lets move onto some alternative methods for 
# the offtake df, starting with GLMM 


# Lets load some more packages we might need

# Extra packages we need
install_and_load("glmmTMB")
install_and_load("DHARMa")
install_and_load("stringr")
install_and_load("splines")  # for GLMM spline
library(mgcv)  # ensure s() is available in formulas


### Build a weekly dataset per species
# Wweekly totals to smooth daily noise but keep seasonal detail.

species_week <- offtake_df |>
  dplyr::filter(!is.na(week_index)) |>
  dplyr::mutate(
    species = factor(stringr::str_squish(tolower(species))),
    week_in_year = as.numeric(((week_index - 1) %% 52) + 1)
  ) |>
  dplyr::group_by(species, week_index, week_in_year) |>
  dplyr::summarise(total = sum(individuals), .groups = "drop") |>
  dplyr::arrange(species, week_index)




########################################################
# GAM
########################################################


# Why use a GAM here?
# - Generalised Additive Models are a flexible way to model non-linear patterns over time 
#
# - The smooth term for week_in_year can capture complex seasonal patterns in hunting offtake, 
#   while still respecting that weeks wrap around (week 52 connects back to week 1).
#
# - By adding a species-level term, we can account for differences in baseline offtake between species,
#   and, if desired, allow the shape of the seasonal curve itself to vary for each species.
#
# - This makes GAMs ideal for detecting and describing broad seasonal trends, as well as species-specific
#   deviations from those trends.



### GAM with Negative Binomial

# s(week_in_year, bs = "cc") is a smooth that loops thorugh the weeks in the year 1 -> 52 like a circle.
# s(species, bs = "re") gives each species its own baseline level.

gam_seasonal <- gam(
  total ~ s(week_in_year, bs = "cc", k = 10) + s(species, bs = "re"),
  data   = species_week,
  family = nb(),
  knots  = list(week_in_year = c(0.5, 52.5))
)

# Lets check how this model performed
summary(gam_seasonal)
plot(gam_seasonal, pages = 1, shade = TRUE)


# Understanding results of "gam_seasonal":
# - The seasonal smooth is strong: s(week_in_year) has edf ≈ 4.1 and p = 0.003. 
#   That means a clear, non-linear within-year cycle in offtake rather than a flat or simple linear trend.
#
# - There is big species-level heterogeneity: s(species) is highly significant (p < 2e-16), 
#   so species differ a lot in their baseline offtake.
#
# - The intercept is 1.313 on the log scale, i.e. an average weekly offtake of about exp(1.313) ≈ 3.7 individuals for a 
#   species with an average baseline.
# 
# - Fit summaries (Adj. R² ≈ 0.51, Deviance explained ≈ 87%) suggest the model captures most of the structure. 


# Tweak to the GAM: allow for species-specific deviations in the seasonal pattern
gam_species_fs <- gam(
  total ~ s(week_in_year, bs = "cc", k = 10) +
    s(week_in_year, species, bs = "fs", k = 5, m = 1),
  data   = species_week,
  family = nb(),
  knots  = list(week_in_year = c(0.5, 52.5))
)


# Lets check how this model performed
summary(gam_species_fs)
plot(gam_species_fs, pages = 1, shade = TRUE)


# compare AIC vals of the two models
AIC(gam_seasonal)
AIC(gam_species_fs)



# Understanding results of "gam_species_fs":
# - Strong overall seasonal pattern remains: s(week_in_year) edf ~ 3.48, p < 0.001 → general hunting seasonality.
# - Species-specific curves: s(week_in_year, species) edf ~ 33, p < 2e-16 → species differ in timing/strength of seasonal peaks.
# - Model fit marginally improves compared to simple seasonal model: Adj. R^2 ~ 0.572 (vs 0.511 in simpler GAM), deviance explained ~ 88.6% (vs 87%).
# - So the seasonal pattern varies slightly by species — this extra complexity slightly improves model fit, but this is very marginal.
# It's worth exploring other approaches to see if this species-specific approach is justified.



# Pick the better GAM based on AIC
best_gam <- if (AIC(gam_seasonal) <= AIC(gam_species_fs)) gam_seasonal else gam_species_fs


# Add fitted values for plotting
species_week$fit_gam <- stats::predict(best_gam, type = "response")

# Plot fitted seasonal curves per species
ggplot2::ggplot(species_week, ggplot2::aes(week_index, total)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = fit_gam), linewidth = 1) +
  ggplot2::facet_wrap(~ species, scales = "free_y") +
  ggplot2::labs(
    title = "Weekly offtake by species with cyclic GAM (NB)",
    x = "Week index", y = "Individuals hunted"
  ) +
  ggplot2::theme_minimal()



# Some species really don't have enough data, so let's focus on just those with over 10.


species_to_plot <- species_week |>
  dplyr::group_by(species) |>
  dplyr::summarise(total_records = n()) |>
  dplyr::filter(total_records > 10) |>
  dplyr::pull(species)

plot_df <- species_week |>
  dplyr::filter(species %in% species_to_plot)


ggplot2::ggplot(plot_df, ggplot2::aes(week_index, total)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = fit_gam), linewidth = 1) +
  ggplot2::facet_wrap(~ species, scales = "free_y") +
  ggplot2::labs(
    title = "Weekly offtake by species with cyclic GAM (NB)",
    x = "Week index", y = "Individuals hunted"
  ) +
  ggplot2::theme_minimal()



# **** This plot quite clearly shows that, although slight variation exists, the patterns are broadly the consistent across species,
# **** and therefore breaking down the data into individual species models may not improve our understanding 


########################################################
# GLMM
########################################################


# Lastly, with the offtake_df, let's try a GLMM. 
# GLMMs are useful here because they let us model overall time trends while accounting 
# for repeated measurements within species (pseudoreplication), avoiding inflated significance.



# 1) Random-intercept model: each species has its own baseline level
glmm_ns <- glmmTMB::glmmTMB(
  total ~ splines::ns(week_index, df = 5) + (1 | species),
  data   = species_week,
  family = glmmTMB::nbinom2()
)

summary(glmm_ns)
DHARMa::simulateResiduals(glmm_ns) |> plot()

# 2) Random-slopes model: allows species to have different time trends
glmm_rs <- glmmTMB::glmmTMB(
  total ~ splines::ns(week_index, df = 5) + (week_index | species),
  data   = species_week,
  family = glmmTMB::nbinom2()
)

summary(glmm_rs)
DHARMa::simulateResiduals(glmm_rs) |> plot()

# Compare models using AIC (lower is better)
AIC(glmm_ns, glmm_rs)


# Interpretation:
#   
# Random-intercept model (glmm_ns): AIC = 2038.6
# Random-slope model (glmm_rs): AIC = 2041.4
# 
# - Since the AIC is slightly lower for the random-intercept model, adding species-specific time trends (random slopes) 
#   doesn’t improve the fit enough to justify the extra complexity.
# 
# - The significant spline terms (ns(week_index, df = 5)2 and ns(week_index, df = 5)4) mean there are non-linear time patterns 
#   in offtake, but they’re consistent enough across species that a single smooth for all species is adequate.
# 
# - The very small variance for the random slope (week_index = 0.0025) confirms what the GAM per species plot shows above, that 
#   there’s minimal variation between species in how offtake changes over time.






############################################################################################################
#                                         SUMMARY OF FINDINGS: OFFTAKE DATA                                 #
############################################################################################################

# OK, so as a summary: we analysed hunting offtake using a combination of time series models (ARIMA), 
# Generalised Additive Models (GAM), and Generalised Linear Mixed Models (GLMM) 
# to explore temporal hunting data and any species-level variation.

# ----------------------------------------------------------------------------------------------------------
# 1. Temporal resolution
# ----------------------------------------------------------------------------------------------------------
# - Daily ARIMA models: too noisy and unpredictable → little value for interpretation or forecasting.
# - Monthly ARIMA models: too coarse with too few points → fail to capture trends.
# - Weekly ARIMA models: best balance of detail and structure → capture short-term dynamics 
#   while smoothing daily noise.

# ----------------------------------------------------------------------------------------------------------
# 2. Counts vs biomass
# ----------------------------------------------------------------------------------------------------------
# - Weekly ARIMA based on COUNTS (individuals) fit much better (lower AIC, smaller residual variance) 
#   than BIOMASS (kg).
# - Biomass series is noisier since a few large animals dominate weekly totals.
# - Missing mass values for broad categories (e.g. “birds”, “fish”) weaken biomass analysis.
#   Filling these gaps with reliable estimates would strengthen results, but rough guesses 
#   risk undermining the exercise.

# ----------------------------------------------------------------------------------------------------------
# 3. Seasonal dynamics (GAMs)
# ----------------------------------------------------------------------------------------------------------
# - Clear, non-linear within-year cycle in offtake detected (p < 0.01).
# - Strong species-level differences in baseline hunting intensity.
# - However, seasonal curves are broadly similar across species; allowing species-specific curves 
#   only marginally improved model fit.
# - Interpretation: seasonality is consistent across taxa, not strongly species-specific.

# ----------------------------------------------------------------------------------------------------------
# 4. Species variation (GLMMs)
# ----------------------------------------------------------------------------------------------------------
# - Temporal dynamics are well explained by a shared non-linear trend across species.
# - Random-intercept model (species with different baselines) outperforms random-slope model 
#   (species with different time trends).
# - Confirms species differ mainly in baseline hunting pressure, not in the shape of 
#   their temporal patterns.

# ----------------------------------------------------------------------------------------------------------
# KEY FINDINGS
# ----------------------------------------------------------------------------------------------------------
# - For this dataset, weekly resolution is the most informative timescale for analysing offtake.
# - Based on the state of the data, counts (rather than biomass) are  more robust for temporal analysis.
# - Hunting offtake follows a clear seasonal cycle that is consistent across species.
# - Differences between species reflect baseline pressure rather than distinct seasonal behaviours.

############################################################################################################





### Code addition from Feedback 07th October ###

# ============================================================
# Offtake: seasonality across species categories (counts vs biomass)
# ============================================================

# 1) Make a season factor (Winter, Spring, Summer, Autumn)
offtake_season <- offtake_df %>%
  dplyr::mutate(
    date  = as.Date(date),
    month = lubridate::month(date),
    season = dplyr::case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("Winter","Spring","Summer","Autumn"))
  )

# Choose your category variable: here we use size_group from your head()
cat_var <- "size_group"

# 2) Season x category summaries: counts and biomass
season_cat <- offtake_season %>%
  dplyr::filter(!is.na(season), !is.na(.data[[cat_var]])) %>%
  dplyr::group_by(season, !!rlang::sym(cat_var)) %>%
  dplyr::summarise(
    individuals = sum(individuals, na.rm = TRUE),
    biomass_kg  = sum(biomass_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::rename(category = !!rlang::sym(cat_var))


# 4) Optional: weekly seasonality by biomass (smooth lines across the year)
biomass_week <- offtake_season %>%
  dplyr::mutate(week_in_year = as.numeric(((week_index - 1) %% 52) + 1)) %>%
  dplyr::filter(!is.na(week_in_year)) %>%
  dplyr::group_by(week_in_year, !!rlang::sym(cat_var)) %>%
  dplyr::summarise(biomass_kg = sum(biomass_kg, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(category = !!rlang::sym(cat_var))

ggplot2::ggplot(biomass_week,
                ggplot2::aes(x = week_in_year, y = biomass_kg, colour = category)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "Weekly biomass offtake by category",
    x = "Week of year", y = "Biomass (kg)"
  )



# ============================================================
#### Fit a GAM as the raw biomass is very difficult to interpret
# ============================================================

# Ensure factor + no NAs
biomass_week <- biomass_week %>%
  dplyr::filter(!is.na(category), !is.na(week_in_year)) %>%
  dplyr::mutate(
    category    = factor(category),
    biomass_kg  = pmax(biomass_kg, 0) + 1e-6  # avoid zeros for log link
  )

# Cyclic GAM for seasonal smoothing (Gamma w/ log link fits biomass)
gam_biomass <- mgcv::gam(
  biomass_kg ~ s(week_in_year, bs = "cc", k = 10) + s(category, bs = "re"),
  data   = biomass_week,
  family = Gamma(link = "log"),
  method = "REML",
  knots  = list(week_in_year = c(0.5, 52.5))
)

# Smoothed predictions
biomass_week$fit_gam <- as.numeric(predict(gam_biomass, type = "response"))

# Plot: smoothed seasonal curves (log y for readability)
ggplot2::ggplot(biomass_week,
                ggplot2::aes(x = week_in_year, y = fit_gam, colour = category)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "Smoothed weekly biomass offtake by category (cyclic GAM)",
    x = "Week of year", y = "Predicted biomass (kg, log10 scale)"
  ) +
  ggplot2::theme_minimal()




#### Now fit the same for the raw counts, to compare to the biomass above ####



# 1) Weekly totals (counts)
counts_week <- offtake_season %>%
  dplyr::mutate(week_in_year = as.numeric(((week_index - 1) %% 52) + 1)) %>%
  dplyr::filter(!is.na(week_in_year), !is.na(.data[[cat_var]])) %>%
  dplyr::group_by(week_in_year, !!rlang::sym(cat_var)) %>%
  dplyr::summarise(individuals = sum(individuals, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(category = !!rlang::sym(cat_var)) %>%
  dplyr::mutate(category = factor(category))

# 2) Cyclic GAM smoothing for counts (Negative Binomial)
gam_counts <- mgcv::gam(
  individuals ~ s(week_in_year, bs = "cc", k = 10) + s(category, bs = "re"),
  data   = counts_week,
  family = mgcv::nb(),
  method = "REML",
  knots  = list(week_in_year = c(0.5, 52.5))
)

# 3) Predicted smooth counts
counts_week$fit_gam <- as.numeric(predict(gam_counts, type = "response"))

# 4) Plot: smoothed seasonal curves (log y to match biomass plot)
ggplot2::ggplot(counts_week,
                ggplot2::aes(x = week_in_year, y = fit_gam, colour = category)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "Smoothed weekly offtake COUNTS (cyclic GAM)",
    x = "Week of year", y = "Predicted count (log10 scale)"
  ) +
  ggplot2::theme_minimal()




# Summary tables for key stats
summary(gam_counts)
summary(gam_biomass)
summary(gam_counts)$s.table
summary(gam_biomass)$s.table

# Deviance explained and adjusted R²
list(
  counts_dev = summary(gam_counts)$dev.expl,
  counts_r2  = summary(gam_counts)$r.sq,
  biomass_dev = summary(gam_biomass)$dev.expl,
  biomass_r2  = summary(gam_biomass)$r.sq
)





# Testing for underfit or overfit of the smooth GAM
mgcv::gam.check(gam_counts)    # look at 'k-index' and its p-value
mgcv::gam.check(gam_biomass)



# ============================================================
### Summary for this addition ###
# ============================================================


### Approach ###

# Here, I used GAMs with cyclic smoothers to describe seasonal patterns in hunting offtake. The cyclic smoother treats 
# the year as a continuous loop (week 52 connects to week 1), ideal for seasonal data.


# We compared offtake counts and offtake biomass. For offtake, we used a Negative Binomial family for overdispersed count data.
# Biomass used a Gamma(log) family since biomass is continuous and positive.

# Including s(category, bs = "re") accounts for differences in baseline level of hunting between taxonomic or size groups 
# (e.g. small birds vs mammals).

# This approach overall lets us test whether hunting intensity or total biomass varies seasonally, 
# and whether the shape of that seasonal pattern differs across taxa.


### Results ###

# Both models show a clear seasonal signal, with non-linear fluctuations through the year.

# Offtake counts show a stronger seasonal pattern and higher explanatory power (Deviance ≈ 82 %).
# Hunting events peak and dip sharply across the year, suggesting accessibility or activity cycles.

# Biomass shows a similar but less pronounced cycle (Deviance ≈ 57 %), implying that although hunting frequency changes seasonally,
# the total mass removed is buffered - likely because bird-dominated peaks contribute many individuals but little mass, while 
# mammal periods yield fewer but heavier carcasses.

# In both cases, taxonomic category clearly accounts for the distinctive baseline variation, confirming that differences between 
# groups (rather than just time) drive much of the variation in totals.



### Testing for underfit or overfit of the smoothing ### 


# I verified the adequacy of the basis dimension (k) using gam.check(). 
# For both count and biomass models, the k-index (~1.0, p > 0.7) and effective degrees of freedom well below k′ 
# confirm that smoothing was neither over- nor underfitted.