

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
# Ramya feedback edit 25/08: Regroup species into categories based on size & hunting method
# -----------------------------------------------------
# This grouping reflects how hunters actually distinguish species:
# - Smaller vs larger rodents/carnivores hunted with different methods
# - Birds vs pheasants as separate groups
# - Ungulates, primates, fish, amphibians kept distinct



offtake_df <- offtake_df %>%
  dplyr::mutate(
    size_group = dplyr::case_when(
      # Primates
      species %in% c("assamese_macaque", "macaque_sp", "stump_tailed_macaque") ~ "primate",
      
      # Ungulates
      species %in% c("barking_deer", "serow", "wild_boar") ~ "ungulate",
      
      # Rodents split by size
      species %in% c("squirrel", "flying_squirrel", "giant_squirrel") ~ "small_rodent",
      species %in% c("bamboo_rat", "brush_tailed_porcupine", "malayan_porcupine") ~ "large_rodent",
      
      # Carnivores split by size
      species %in% c("civet", "himalayan_palm_civet", "spotted_linsang") ~ "small_carnivore",
      species %in% c("leopard_cat", "dhole") ~ "large_carnivore",
      
      # Birds vs pheasants
      species %in% c("birds", "green_pigeon") ~ "bird",
      species %in% c("bamboo_partridge", "kalij_pheasant", "hill_partridge") ~ "pheasant",
      
      # Fish & Amphibians
      species == "fish" ~ "fish",
      species == "frogs" ~ "amphibian",
      
      # Catch-all
      TRUE ~ "other"
    )
  )

# Check the new grouping
table(offtake_df$size_group, useNA = "ifany")



unique(offtake_df$species)


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









########################################################
# EDIT: Using species mass values in the time series 
########################################################


# standardise species names in both data frames 
offtake_df <- offtake_df |>
  dplyr::mutate(species = stringr::str_squish(tolower(species)))

sp_mass_values <- sp_mass_values |>
  dplyr::rename(species_raw = dplyr::matches("^species$", ignore.case = TRUE),
                mass_kg_raw = dplyr::matches("mass", ignore.case = TRUE)) |>
  dplyr::mutate(
    species_raw = stringr::str_squish(species_raw),
    mass_kg = as.numeric(mass_kg_raw)
  )


# Map the names across each dataset
name_map <- tibble::tribble(
  ~species_raw,                 ~species,                    ~mass_kg_override,
  "Asiatic wild dog",           "dhole",                     16.850,
  "Assamaese macaque",          "assamese_macaque",          9.400,
  "Barking deer",               "barking_deer",              22.900,
  "Brush tailed porcupine",     "brush_tailed_porcupine",     2.500,
  "Hill partridge",             "hill_partridge",             0.319,
  "Himalayan black bear",       "himalayan_black_bear",     103.600,
  "Himalayan palm civet",       "himalayan_palm_civet",       4.270,
  "Leopard cat",                "leopard_cat",                4.000,
  "Red serow",                  "serow",                    135.000,
  "Stumped tailed macaque",     "stump_tailed_macaque",       9.000,
  "Yellow throated marten",     "yellow_throated_marten",     2.900
)


# Merge the map into sp_mass_values, fixing names and overriding mass where provided
sp_mass_values <- sp_mass_values |>
  dplyr::left_join(name_map, by = "species_raw") |>
  dplyr::mutate(
    species = dplyr::coalesce(species, stringr::str_squish(tolower(species_raw))),
    mass_kg = dplyr::coalesce(mass_kg_override, mass_kg)  # prefer Ramya's figure when given
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



# There are a few species with missing mass values, so i'll dig rough estimates from the literature for adult body mass.

# Generic/unspecified categories (e.g. 'birds', 'fish', 'civet', 'squirrel', 'macaque_sp', 'flying_squirrel') are left as NA 
# so we don't pretend to know their mass. This keeps biomass estimates transparent and conservative.




# New literature-based estimates for named species only (Ofc, feel free to edit!)
mass_lookup <- tibble::tribble(
  ~species,             ~mass_kg,
  "bamboo_partridge",     0.33,
  "bamboo_rat",           0.65,
  "giant_squirrel",       1.20,
  "green_pigeon",         0.21,
  "kalij_pheasant",       0.80,
  "malayan_porcupine",   13.00,
  "spotted_linsang",      0.90,
  "wild_boar",           70.00
)

# Upsert into the existing table (keeps existing rows not listed; replaces/sets these)
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


# much better, now we can proceed with the time series. 






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

# Results:
# - The counts model has much lower AIC and smaller residual variance, meaning the counts model explains the weekly time 
# series pattern more efficiently.
# - The biomass model is noisier: biomass fluctuates more than counts week-to-week.
#

# Interpretation:
# Counts here are better for capturing general temporal trends (seasonality/short-term shifts), but biomass might be more useful for
# showing impact on the overall ecosystem. 

# One thing to note - the biomass time series would be strengthened considerably if we had mass values for general groups like 
# "fish" and "birds", but estimating here might undermine the exercise. 




########################################################
# Weekly biomass model using species groups 
########################################################

# Again, the plot below is not particularly helpful because we lose so much data from groups such as "birds" and "fish". 

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


# this approach (using species groups) doesn't yield great results because we're missing mass values for the majority of the data 
# (e.g. the records labelled 'fish', 'birds' etc which we don't have mass values for).




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









# OK, now we'll move onto the consumption df. For simplicity, we'll do this in another script 






