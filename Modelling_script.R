

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


consumption_df <- read.csv("https://raw.githubusercontent.com/harryjobann/Thanamir_hunting/refs/heads/main/finalconsumption_cleaned%20-%20finalconsumption_cleaned.csv")
offtake_df <- read.csv("https://raw.githubusercontent.com/harryjobann/Thanamir_hunting/refs/heads/main/clean_offtake.csv")






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

# and finally, a offtake time series on a monthly basis

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



# Some species do not have enough data for this approach to succeed. Therefore, we'll group into taxonomic groups and retry.

offtake_df <- offtake_df %>%
  dplyr::mutate(
    taxon_group = dplyr::case_when(
      # Primates
      species %in% c("assamese_macaque", "macaque_sp", "stump_tailed_macaque") ~ "primate",
      
      # Ungulates
      species %in% c("barking_deer", "serow", "wild_boar") ~ "ungulate",
      
      # Rodents
      species %in% c("giant_squirrel", "squirrel", "flying_squirrel", "bamboo_rat", "brush_tailed_porcupine", "malayan_porcupine ") ~ "rodent",
      
      # Carnivores
      species %in% c("civet", "himalayan_palm_civet", "leopard_cat", "dhole", "spotted_linsang") ~ "carnivore",
      
      # Birds
      species %in% c("birds", "bamboo_partridge", "kalij_pheasant", "hill_partridge", "green_pigeon") ~ "bird",
      
      # Fish & Amphibians
      species == "fish" ~ "fish",
      species == "frogs" ~ "amphibian",
      
      # Default catch-all
      TRUE ~ "other"
    )
  )



unique(offtake_df$species)


weekly_taxa <- offtake_df %>%
  dplyr::group_by(taxon_group, week_index) %>%
  dplyr::summarise(total = sum(individuals), .groups = "drop") %>%
  dplyr::mutate(log_total = log1p(total)) %>%
  tsibble::as_tsibble(key = taxon_group, index = week_index) %>%
  tsibble::fill_gaps(total = 0, log_total = 0)



models_by_group <- weekly_taxa %>%
  model(ARIMA(log_total))


models_by_group %>%
  forecast(h = 5) %>%
  autoplot(weekly_taxa)



# Further tweaks - Lets maybe group mammals together as too many categories with insufficient records


# 1. Assign simplified  groups
offtake_df <- offtake_df %>%
  dplyr::mutate(
    simplified_group = dplyr::case_when(
      taxon_group %in% c("primate", "ungulate", "rodent", "carnivore") ~ "mammal",
      taxon_group %in% c("bird", "fish") ~ taxon_group,
      TRUE ~ NA_character_
    )
  )

# 2. Aggregate to weekly totals 
weekly_simplified <- offtake_df %>%
  dplyr::filter(!is.na(simplified_group)) %>%
  dplyr::group_by(simplified_group, week_index) %>%
  dplyr::summarise(total = sum(individuals), .groups = "drop") %>%
  dplyr::mutate(log_total = log1p(total))

# 3. Convert to tsibble 
weekly_simplified <- weekly_simplified %>%
  tsibble::as_tsibble(key = simplified_group, index = week_index) %>%
  tsibble::fill_gaps(total = 0, log_total = 0) %>%
  dplyr::mutate(simplified_group = factor(
    simplified_group,
    levels = c("mammal", "bird", "fish")
  ))

# 4. Fit ARIMA models per simplified group
models_by_group <- weekly_simplified %>%
  model(ARIMA(log_total))



# When we plot the timeseries we can choose to include an inbuilt 'forecasting' feature. Two facet plots are created below - one 
# with forecasting and one without 


# 5. Plot - one with forecast, one without

# 5.1 With forecast (ARIMA on log_total)
fc <- models_by_group %>%
  forecast(h = 5)

p_with_fc <- fc %>%
  autoplot(weekly_simplified) +
  ggplot2::labs(
    title = "Weekly log-transformed offtake by simplified group (with 5-week forecast)",
    x = "Week index",
    y = "log1p(total individuals)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~ simplified_group, ncol = 1, scales = "free_y")


# 5.2 Without forecast (historical only)
p_no_fc <- weekly_simplified %>%
  autoplot(log_total) +
  ggplot2::labs(
    title = "Weekly log-transformed offtake by simplified group (historical only)",
    x = "Week index",
    y = "log1p(total individuals)"
  ) +
  ggplot2::theme_minimal() + 
  ggplot2::facet_wrap(~ simplified_group, ncol = 1, scales = "free_y")


# Print
p_with_fc
p_no_fc



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
# - Interpretation: The seasonal pattern varies slightly by species — this extra complexity slightly improves model fit.



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



# ****This plot quite clearly shows that, although slight variation exists, the patterns are broadly the consistent across species,
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



# OK, now we'll move onto the consumption df. 



############################################################################################################
#--------------------------------------------Consumption df -----------------------
############################################################################################################

# Frequency of each month
table(consumption_df$month)


# Step 1: Clean and summarise wild meat consumption
monthly_consumption <- consumption_df %>%
  filter(meat == "wild") %>%
  mutate(month = yearmonth(month)) %>%  # now works cleanly!
  group_by(month) %>%
  summarise(total_wild = sum(times)) %>%
  as_tsibble(index = month) %>%
  fill_gaps(total_wild = 0) %>%
  arrange(month) %>%
  mutate(t = 1:n())  # numeric time index for GAM




# plot wild meat consumption by month
monthly_consumption %>%
  autoplot(total_wild) +
  labs(title = "Wild Meat Consumption Over 2022",
       x = "Month", y = "Total Consumption Events")






gam_model <- gam(total_wild ~ s(t, k = 6), data = monthly_consumption, family = quasipoisson)

# Predict and plot
monthly_consumption$pred <- predict(gam_model, type = "response")

ggplot(monthly_consumption, aes(x = month)) +
  geom_point(aes(y = total_wild)) +
  geom_line(aes(y = pred), colour = "blue") +
  labs(title = "Smoothed Trend in Wild Meat Consumption",
       y = "Consumption Events", x = "Month")




# wild vs domestic meat over time 
monthly_meat <- consumption_df %>%
  mutate(month = yearmonth(month)) %>%
  group_by(month, meat) %>%
  summarise(total = sum(times), .groups = "drop") %>%
  as_tsibble(index = month, key = meat)  # <-- this solves it


# Plot
ggplot(monthly_meat, aes(x = month, y = total, colour = meat)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Wild vs. Domestic Meat Consumption Over Time",
       y = "Consumption Events", x = "Month")



# wild meat consumption: hunter vs non-hunter
monthly_hunter <- consumption_df %>%
  filter(meat == "wild") %>%
  mutate(month = yearmonth(month)) %>%
  group_by(month, hunter) %>%
  summarise(total = sum(times), .groups = "drop") %>%
  as_tsibble(index = month, key = hunter)  # Key added here


ggplot(monthly_hunter, aes(x = month, y = total, colour = hunter)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Wild Meat Consumption: Hunters vs Non-Hunters",
       y = "Consumption Events", x = "Month")



# GAM hunter vs non hunter
monthly_hunter_gam <- monthly_hunter %>%
  mutate(hunter = as.factor(hunter)) %>%
  group_by(hunter) %>%
  mutate(t = 1:n())  # reset time index within each group

gam_hunter <- gam(total ~ s(t, by = hunter) + hunter, data = monthly_hunter_gam, family = quasipoisson)

monthly_hunter_gam$pred <- predict(gam_hunter, type = "response")

ggplot(monthly_hunter_gam, aes(x = month)) +
  geom_point(aes(y = total, colour = hunter)) +
  geom_line(aes(y = pred, colour = hunter), linewidth = 1.2) +
  labs(title = "Smoothed Wild Meat Consumption: Hunters vs Non-Hunters",
       y = "Consumption Events", x = "Month")


# source of wild meat
monthly_source <- consumption_df %>%
  filter(meat == "wild") %>%
  mutate(month = yearmonth(month)) %>%
  group_by(month, source) %>%
  summarise(total = sum(times), .groups = "drop") %>%
  as_tsibble(index = month, key = source)  # Key added here


ggplot(monthly_source, aes(x = month, y = total, colour = source)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Wild Meat Consumption by Source",
       y = "Consumption Events", x = "Month")




#### TBC....


