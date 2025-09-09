




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
    "feasts",
    "broom",
    "purrr",
    "tidyr",
    "scales",
    "zoo",
    "MASS",
    "nnet"
  )

# Apply the function to each package
invisible(lapply(packages, install_and_load))



############################################################################################################
#--------------------------------------------Read in data-------------------------
############################################################################################################


consumption_df <- read.csv("https://raw.githubusercontent.com/harryjobann/Thanamir_hunting/refs/heads/main/finalconsumption_cleaned%20-%20finalconsumption_cleaned.csv")




# Ensure clean factors & dates
consumption_df <- consumption_df |>
  dplyr::mutate(
    month  = tsibble::yearmonth(month),
    season = factor(season, levels = c("Winter","Spring","Summer","Autumn")),
    meat   = factor(tolower(meat), 
                    levels = c("wild","domestic","none")),  
    source = factor(tolower(source))
  )


# Check NA values across all columns (with % missing too)
na_summary <- consumption_df |>
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) |>
  tidyr::pivot_longer(everything(),
                      names_to = "column",
                      values_to = "n_missing") |>
  dplyr::mutate(pct_missing = round(100 * n_missing / nrow(consumption_df), 1)) |>
  dplyr::arrange(desc(n_missing))

print(na_summary)

# Frequency per month check
table(consumption_df$month)








############################################################################################################
#--------------------------------------------Data prep------------------------------------------------------#
############################################################################################################

# A0) Basic hygiene: standardise keys we’ll use a lot
consumption_df <- consumption_df |>
  dplyr::mutate(
    month  = tsibble::yearmonth(month),
    season = factor(season, levels = c("Winter","Spring","Summer","Autumn")),
    meat   = factor(tolower(meat), levels = c("wild","domestic","none")),
    source = factor(tolower(source))
  )

# A1) Quick NA audit (so we know where gaps are before summarising)
na_summary <- consumption_df |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.)))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "n_missing") |>
  dplyr::mutate(pct_missing = round(100 * n_missing / nrow(consumption_df), 1)) |>
  dplyr::arrange(dplyr::desc(n_missing))
na_summary  # <- check this table

# A2) Frequency per month (simple sense-check)
table(consumption_df$month)




############################################################################################################
#--------------------------------------------Analysis------------------------------------------------------#
############################################################################################################

# ----------------------------------------------------------------------------------------------------------
# Wild meat: monthly pattern + GAM smooth 
# ----------------------------------------------------------------------------------------------------------


# Build monthly totals
monthly_consumption <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(total_wild = sum(times, na.rm = TRUE), .groups = "drop") %>%
  tsibble::as_tsibble(index = month) %>%
  tsibble::fill_gaps(total_wild = 0) %>%
  dplyr::arrange(month) %>%
  dplyr::mutate(t = 1:dplyr::n())

# Fit GAM on a data
monthly_consumption_df <- monthly_consumption |> as.data.frame()
gam_model <- mgcv::gam(
  total_wild ~ s(t, k = 6),
  data   = monthly_consumption_df,
  family = quasipoisson()
)

monthly_consumption$pred <- stats::predict(gam_model, type = "response")



# Plot
monthly_consumption %>%
  fabletools::autoplot(total_wild) +
  ggplot2::geom_line(ggplot2::aes(y = pred), colour = "blue") +
  ggplot2::labs(title = "Wild Meat Consumption Over 2022",
                subtitle = "Points = monthly totals; blue line = GAM smooth (quasi-Poisson)",
                x = "Month", y = "Total consumption events")




# ----------------------------------------------------------------------------------------------------------
# C) Wild vs domestic (and 'none') over time
# ----------------------------------------------------------------------------------------------------------
monthly_meat <- consumption_df %>%
  dplyr::group_by(month, meat) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") %>%
  tsibble::as_tsibble(index = month, key = meat)

ggplot2::ggplot(monthly_meat, ggplot2::aes(x = month, y = total, colour = meat)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::labs(title = "Wild vs. Domestic (and 'none') Over Time",
                y = "Consumption events", x = "Month")




# ----------------------------------------------------------------------------------------------------------
# Hunter vs non-hunter (wild meat only)
# ----------------------------------------------------------------------------------------------------------


monthly_hunter <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(month, hunter) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") %>%
  tsibble::as_tsibble(index = month, key = hunter)

# Build GAMs
monthly_hunter_gam <- monthly_hunter %>%
  dplyr::mutate(hunter = as.factor(hunter)) %>%
  dplyr::group_by(hunter) %>%
  dplyr::arrange(month, .by_group = TRUE) %>%
  dplyr::mutate(t = 1:dplyr::n()) %>%
  dplyr::ungroup()

monthly_hunter_gam_df <- monthly_hunter_gam |> as.data.frame()
gam_hunter <- mgcv::gam(
  total ~ s(t, by = hunter) + hunter,
  data   = monthly_hunter_gam_df,
  family = quasipoisson()
)
monthly_hunter_gam$pred <- stats::predict(gam_hunter, type = "response")

# INSIGHTS: overall share
insight_hunter_overall <- consumption_df |>
  dplyr::filter(meat == "wild") |>
  dplyr::group_by(hunter) |>
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(share = total / sum(total)) |>
  dplyr::mutate(total = round(total, 0), share_chr = scales::percent(share, accuracy = 0.1))


glm_hunter <- consumption_df |>
  dplyr::filter(meat == "wild") |>
  dplyr::group_by(id, hunter) |>
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") |>
  stats::glm(total ~ hunter, family = stats::poisson(), data = _)
irr_row <- broom::tidy(glm_hunter, exponentiate = TRUE, conf.int = TRUE) |>
  dplyr::filter(term == "hunteryes")


# Plot GAM smooth
ggplot2::ggplot(monthly_hunter_gam, ggplot2::aes(x = month)) +
  ggplot2::geom_point(ggplot2::aes(y = total, colour = hunter)) +
  ggplot2::geom_line(ggplot2::aes(y = pred, colour = hunter), linewidth = 1.2) +
  ggplot2::labs(title = "Smoothed Wild Meat Consumption: Hunters vs Non-Hunters",
                subtitle = "Points = monthly totals; lines = group-specific GAM smooths",
                y = "Consumption events", x = "Month")


# get outputs from analysis
print(insight_hunter_overall, n = Inf)
print(irr_row)
print(summary(gam_hunter))



# -----------------------------------------------------------
# Summary: Hunters vs Non-Hunters (wild meat only)
# -----------------------------------------------------------
# Overall totals show hunters ate slightly more wild meat than non-hunters across the year, 55% vs 45%. So not a massive gap, but 
# hunters do take the bigger share.
#
# The Poisson regression backs this up. Hunters consumed more than twice as much wild meat as non-hunters (IRR = 2.27, 95% CI 1.97–2.61,
# p < 0.001). That’s a strong and highly significant difference.
#
# The GAM smooths don’t add much extra. The seasonal wiggles for hunters and non-hunters aren’t statistically significant. But the
# explained deviance is decent, around 39 percent, which means the model still picks up a fair chunk of the variation.
#
# Overall, hunters are clearly the heavier consumers of wild meat, with more than double the intake of non-hunters, even if the seasonal
# patterns aren’t sharply different between the two groups.






# ----------------------------------------------------------------------------------------------------------
# Source of wild meat over time 
# ----------------------------------------------------------------------------------------------------------

# Monthly totals by source
monthly_source <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(month, source) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") %>%
  tsibble::as_tsibble(index = month, key = source)


# Overall source composition across the whole year
overall_source <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(source) %>%
  dplyr::summarise(n = sum(times, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(share = n / sum(n),
                share_pct = scales::percent(share, accuracy = 0.1)) %>%
  dplyr::arrange(dplyr::desc(share))

cat("\n--- Wild meat source: overall composition (year) ---\n")
print(overall_source, n = Inf)



# Plot with caption
ggplot2::ggplot(monthly_source, ggplot2::aes(x = month, y = total, colour = source)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::labs(
    title = "Wild Meat Consumption by Source",
    subtitle = "Monthly totals; caption summarises seasonal composition",
    y = "Consumption events", x = "Month"
  )








# ----------------------------------------------------------------------------------------------------------
# Wealth / power/ household effects 
# ----------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------
# 1. Household-level linear model: reliance on wild meat
# -----------------------------------------------------------

# Do household wealth/power predict the proportion of meals that are wild (vs domestic) when aggregated at the household level?

household_summary <- consumption_df %>%
  dplyr::group_by(id, wealth, power) %>%
  dplyr::summarise(
    wild_total = sum(times[meat == "wild"], na.rm = TRUE),
    domestic_total = sum(times[meat == "domestic"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    reliance_wild = wild_total / (wild_total + domestic_total)
  )

ggplot(household_summary, aes(x = wealth, y = wild_total)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "red") +
  labs(x = "Wealth category", y = "Wild meat consumption (times)")

lm_reliance <- lm(reliance_wild ~ wealth + power, data = household_summary)
summary(lm_reliance)


# -----------------------------------------------------------
# 2. Event-level logistic model
# -----------------------------------------------------------
#  For each consumption event, do wealth/power affect the odds it is wild vs domestic?

glm_event <- glm(I(meat == "wild") ~ wealth + power,
                 data = consumption_df,
                 weights = times,
                 family = binomial)
summary(glm_event)


# -----------------------------------------------------------
# 3. Count model (Negative Binomial)
# -----------------------------------------------------------
# Do wealth/power influence the number of wild vs domestic meals consumed (count-based approach)?

glm_counts <- MASS::glm.nb(times ~ meat * wealth + meat * power,
                           data = consumption_df)
summary(glm_counts)


# -----------------------------------------------------------
# 4. Seasonal interaction model
# -----------------------------------------------------------
# Do wealth/power effects appear only in certain seasons (e.g. when wild meat is scarce)?

glm_interaction <- glm(I(meat == "wild") ~ wealth * season + power * season,
                       data = consumption_df,
                       weights = times,
                       family = binomial)
summary(glm_interaction)


# -----------------------------------------------------------
# 5. Multinomial logistic model: source of wild meat
# -----------------------------------------------------------
# Question: Does household wealth/power predict whether wild meat
# is obtained by hunting, being gifted, or buying?
#
# Here, I've restricted to rows where meat == "wild".

library(nnet)

wild_source_df <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::mutate(source = droplevels(as.factor(source)))

# Run multinomial model
# (weights = times to account for frequency of each event)
glm_source <- nnet::multinom(source ~ wealth + power,
                             data = wild_source_df,
                             weights = times)

summary(glm_source)


# Get p-values 
z_vals <- summary(glm_source)$coefficients / summary(glm_source)$standard.errors
p_vals <- (1 - pnorm(abs(z_vals), 0, 1)) * 2
p_vals






# -----------------------------------------------------------
# Summary: Wealth & Power Effects on Wild Meat Consumption
# -----------------------------------------------------------
# 1. Household-level linear model (reliance_wild ~ wealth + power)
#    → Nothing doing here. Total reliance on wild meat looks
#    pretty similar across wealth/power groups once you average
#    it all up at the household level.
#
# 2. Event-level logistic model (wild vs domestic meal odds)
#    → Same story. No evidence that wealth or power make it more
#    or less likely that any given meal is wild.
#
# 3. Count model (Negative Binomial, wild vs domestic counts)
#    → Flat again. No clear differences in the number of wild vs
#    domestic meals eaten.
#
# 4. Seasonal interaction model (wild vs domestic by season)
#    → Tried looking by season to see if effects pop up in leaner
#    or richer times of year. Still nothing.
#
# 5. Multinomial model (source of wild meat: gifted / hunted / bought)
#    → Finally something. Wealth does make a difference here.
#      Wealthiest households (a) more often buy their wild meat
#      and are less likely to hunt it themselves.
#      Mid-wealth households (b, c, d) lean more towards hunting
#      rather than buying.
#      A hint that wealth might also affect whether meat is gifted
#      vs bought, but only weak evidence.
#    → Power doesn’t really show up as important for sources either.
#
# Overall
# Models 1–4 show no sign that wealth or power change the amount of
# wild meat people eat, or the balance of wild vs domestic.
# Model 5 shows that wealth does shape the way wild meat is obtained.
# Richest households (a) go in for buying, while the mid-groups
# (b–d) are more likely to hunt. Gifting is less clear.
# Power doesn’t seem to matter much either way.
# -----------------------------------------------------------









# ----------------------------------------------------------------------------------------------------------
# Subtribe differences: totals, seasonality, and sources
# ----------------------------------------------------------------------------------------------------------

# A) Overall reliance on wild vs domestic by subtribe
overall_subtribe <- consumption_df %>%
  dplyr::group_by(subtribe, meat) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(subtribe) %>%
  dplyr::mutate(share = total / sum(total)) %>%
  dplyr::ungroup()

# Quick visual: composition of meat type by subtribe
ggplot2::ggplot(overall_subtribe, ggplot2::aes(x = subtribe, y = share, fill = meat)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Meat type composition by subtribe",
                y = "Share within subtribe", x = "Subtribe")

# B) Simple test: meat type ~ subtribe (chi-square on counts)
xt_meat_subtribe <- consumption_df %>%
  dplyr::count(subtribe, meat) %>%
  tidyr::pivot_wider(names_from = meat, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("subtribe") %>%
  as.matrix()

chisq_meat_subtribe <- suppressWarnings(chisq.test(xt_meat_subtribe))

# C) Event-level model: odds a meal is wild vs domestic by subtribe
glm_subtribe <- stats::glm(
  I(meat == "wild") ~ subtribe,
  data    = consumption_df,
  weights = times,
  family  = stats::binomial()
)

# D) Seasonality by subtribe (wild-only), simple and readable
wild_season_subtribe <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(season, subtribe) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop")

ggplot2::ggplot(wild_season_subtribe,
                ggplot2::aes(x = season, y = total, group = subtribe, colour = subtribe)) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Wild meat over seasons by subtribe",
                y = "Consumption events", x = "Season")

# E) Source among wild meat: are subtribes different (gifted / hunted / bought)?
library(nnet)

wild_source_subtribe <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::mutate(source = droplevels(as.factor(source)),
                subtribe = droplevels(as.factor(subtribe)))

multinom_subtribe <- nnet::multinom(
  source ~ subtribe,
  data    = wild_source_subtribe,
  weights = times,
  trace   = FALSE
)

# Wald-style p-values for the multinomial coefficients
mn_sum   <- summary(multinom_subtribe)
mn_z     <- mn_sum$coefficients / mn_sum$standard.errors
mn_pvals <- (1 - pnorm(abs(mn_z))) * 2

# ----------------------------------------------------------------------------------------------------------
# Collect outputs for the subtribe summary
# ----------------------------------------------------------------------------------------------------------

print(overall_subtribe %>% dplyr::arrange(subtribe, dplyr::desc(share)), n = Inf)
print(chisq_meat_subtribe)
print(summary(glm_subtribe))
print(mn_pvals)




# -----------------------------------------------------------
# Summary: Subtribe differences
# -----------------------------------------------------------
# The overall composition shows clear variation between groups.
# Chirr and Langa both get about 62–64% of their meat from wild
# sources, Tikhir are a bit lower at 57%, and Makury sit lowest
# with only 41% wild and a majority of domestic.
#
# The chi-square test confirms these differences are unlikely to
# be by chance (X² = 18.5, df = 6, p = 0.005). So subtribe does
# matter for the wild vs domestic split.
#
# The logistic model tells the same story in another way.
# Compared to the baseline group (Chirr), Makury households are
# significantly less likely to eat wild meat (coefficient = -0.84,
# p < 0.001). Langa and Tikhir aren’t statistically different from
# Chirr in this model.
#
# Looking at sources of wild meat, the multinomial model doesn’t
# show strong differences between subtribes. None of the subtribe
# terms come out significant for whether meat is hunted, gifted,
# or bought. So the main contrasts between subtribes are in the
# overall balance of wild vs domestic, not in how wild meat is
# obtained.
#
# Overall, subtribes differ in their reliance on wild meat, with
# Makury standing out as the least dependent, while Chirr and Langa
# are the most reliant. But the routes of access (hunting, buying,
# gifting) look fairly similar across groups.
# -----------------------------------------------------------









# ----------------------------------------------------------------------------------------------------------
# Taxonomic category differences
# ----------------------------------------------------------------------------------------------------------

# A) Overall shares by taxonomic category
overall_category <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(share = total / sum(total)) %>%
  dplyr::arrange(dplyr::desc(share))


# B) Seasonality: totals per season per category
season_category <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::group_by(season, category) %>%
  dplyr::summarise(total = sum(times, na.rm = TRUE), .groups = "drop")

ggplot2::ggplot(season_category,
                ggplot2::aes(x = season, y = total, fill = category)) +
  ggplot2::geom_col(position = "fill") +
  ggplot2::labs(title = "Wild meat composition by category across seasons",
                y = "Proportion within season", x = "Season")

# C) Chi-square: category vs season (are patterns different?)
xt_cat_season <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::count(category, season) %>%
  tidyr::pivot_wider(names_from = season, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("category") %>%
  as.matrix()

chisq_cat_season <- suppressWarnings(chisq.test(xt_cat_season))


# D) Source of wild meat by category
xt_cat_source <- consumption_df %>%
  dplyr::filter(meat == "wild") %>%
  dplyr::count(category, source) %>%
  tidyr::pivot_wider(names_from = source, values_from = n, values_fill = 0) %>%
  tibble::column_to_rownames("category") %>%
  as.matrix()

chisq_cat_source <- suppressWarnings(chisq.test(xt_cat_source))


# Optional: multinomial model (source ~ category)
glm_cat_source <- nnet::multinom(source ~ category,
                                 data = consumption_df %>% dplyr::filter(meat == "wild"),
                                 weights = times,
                                 trace = FALSE)

mn_sum_cat   <- summary(glm_cat_source)
mn_z_cat     <- mn_sum_cat$coefficients / mn_sum_cat$standard.errors
mn_pvals_cat <- (1 - pnorm(abs(mn_z_cat))) * 2


print(overall_category, n = Inf)
print(chisq_cat_season)
print(chisq_cat_source)
print(mn_pvals_cat)





# -----------------------------------------------------------
# Summary: Taxonomic category differences
# -----------------------------------------------------------
# Overall composition is very skewed. Birds dominate the wild
# meat diet (58%), followed by large mammals (17%) and “other”
# taxa (13%). Small rodents, small mammals, and pheasants are
# eaten in smaller amounts, each under 6%. Domestic large
# mammals are negligible as wild meat (<1%).
#
# The seasonal breakdown shows that this balance shifts over the
# year. Birds stay the top category throughout but fall back to
# around 40–45% in autumn compared with 60%+ in other seasons.
# Large mammals peak in winter and spring, then drop away. The
# “other” category rises sharply in autumn, taking a much bigger
# share than earlier in the year. Smaller groups like rodents,
# small mammals and pheasants remain minor contributors, though
# they also nudge up and down seasonally.
#
# The chi-square test confirms these shifts are significant
# (X² = 60.3, df = 18, p < 0.001). Categories are not evenly
# spread across seasons — their consumption patterns genuinely
# change over time.
#
# Source differences are also highly significant (X² = 223.2,
# df = 12, p < 0.001). The multinomial model underlines this,
# with very small p-values for categories like pheasants, small
# mammals and large mammals. These groups show strong preferences
# for being hunted, gifted, or bought. Birds look less tied to a
# single route in comparison.
#
# Overall, birds dominate in scale, but the more telling point is
# that categories differ in both timing and access routes. Autumn
# in particular stands out, when birds decline, “other” taxa surge,
# and the overall mix of wild meat looks quite different.
# -----------------------------------------------------------
