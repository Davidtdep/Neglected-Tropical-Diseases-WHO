################################################################################
#### 0. Load libraries ####
################################################################################

library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(forcats)
library(stringr)
library(scales)
library(grid)
library(gridExtra)


################################################################################
#### 1. Data input ####
################################################################################

data = read_excel("~/Desktop/RegionNTDs001/manuscript/Supplementary Material 1.xlsx")
region.regression = read_excel("~/Desktop/RegionNTDs001/manuscript/Supplementary Material 2.xlsx")
hierarchical_results = read_excel("~/Desktop/RegionNTDs001/manuscript/Supplementary Material 3.xlsx")

# Filter only some indicators in the list
indicators = c(  "Antiretroviral.therapy.coverage....of.people.living.with.HIV.",
                 "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV.",
                 "Children..0.14..living.with.HIV",
                 "Children..ages.0.14..newly.infected.with.HIV",
                 "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever.",
                 "Incidence.of.HIV..all..per.1.000.uninfected.population.",
                 "Incidence.of.malaria..per.1.000.population.at.risk.",
                 "Incidence.of.tuberculosis..per.100.000.people.",
                 "Tuberculosis.case.detection.rate.....all.forms.",
                 "Tuberculosis.treatment.success.rate....of.new.cases.",
                 "NUMBER.OF.DALYS",
                 "NUMBER.OF.DEATHS",
                 "DEATH.RATE",
                 "Number.of.people.requiring.treatment.against.neglected.tropical.diseases",
                 "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases",
                 "Death.rate.from.venomous.snakes",
                 "Deaths.from.cysticercosis",
                 "Deaths.from.rabies.by.world.region",
                 "Dengue.fever.deaths",
                 "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis",
                 "Number.of.people.requiring.preventive.treatment.for.schistosomiasis",
                 "Reported.cases.of.leprosy",
                 "Antibiotic.consumption.rate")


data <- data %>%
  select(1:3, any_of(indicators))


region.regression <- region.regression[region.regression$dependent_variable %in% indicators | 
                                         region.regression$independent_variable %in% indicators, ]

hierarchical_results <- hierarchical_results[hierarchical_results$dependent_variable %in% indicators | 
                                               hierarchical_results$independent_variable %in% indicators, ]


################################################################################
#### 2. Categorizing indicators ####
################################################################################


# Define sub-categories for different indicators
hiv_indicators <- c("Antiretroviral.therapy.coverage....of.people.living.with.HIV.",
                    "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV.",
                    "Children..0.14..living.with.HIV",
                    "Children..ages.0.14..newly.infected.with.HIV",
                    "Incidence.of.HIV..all..per.1.000.uninfected.population.")

tb_indicators <- c("Incidence.of.tuberculosis..per.100.000.people.",
                   "Tuberculosis.case.detection.rate.....all.forms.",
                   "Tuberculosis.treatment.success.rate....of.new.cases.")

malaria_indicators <- c("Incidence.of.malaria..per.1.000.population.at.risk.",
                        "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever.")

ntd_indicators <- c("Number.of.people.requiring.treatment.against.neglected.tropical.diseases",
                    "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases",
                    "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis",
                    "Number.of.people.requiring.preventive.treatment.for.schistosomiasis",
                    "Reported.cases.of.leprosy")

ntd_deaths_indicators <- c("Death.rate.from.venomous.snakes",
                           "Deaths.from.cysticercosis",
                           "Deaths.from.rabies.by.world.region",
                           "Dengue.fever.deaths")

general_indicators <- c("NUMBER.OF.DALYS",
                        "NUMBER.OF.DEATHS",
                        "DEATH.RATE",
                        "Antibiotic.consumption.rate")



# Create mapping for clean indicator names
indicator_names <- c(
  "Antiretroviral.therapy.coverage....of.people.living.with.HIV." = 
    "Antiretroviral therapy coverage (% of people living with HIV)",
  "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV." = 
    "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
  "Children..0.14..living.with.HIV" = 
    "Children (0-14) living with HIV",
  "Children..ages.0.14..newly.infected.with.HIV" = 
    "Children (ages 0-14) newly infected with HIV",
  "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever." = 
    "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)",
  "Incidence.of.HIV..all..per.1.000.uninfected.population." = 
    "Incidence of HIV, all (per 1,000 uninfected population)",
  "Incidence.of.malaria..per.1.000.population.at.risk." = 
    "Incidence of malaria (per 1,000 population at risk)",
  "Incidence.of.tuberculosis..per.100.000.people." = 
    "Incidence of tuberculosis (per 100,000 people)",
  "Tuberculosis.case.detection.rate.....all.forms." = 
    "Tuberculosis case detection rate (%, all forms)",
  "Tuberculosis.treatment.success.rate....of.new.cases." = 
    "Tuberculosis treatment success rate (% of new cases)",
  "NUMBER.OF.DALYS" = 
    "Number of DALYs",
  "NUMBER.OF.DEATHS" = 
    "Number of Deaths",
  "DEATH.RATE" = 
    "Death Rate",
  "Number.of.people.requiring.treatment.against.neglected.tropical.diseases" = 
    "Number of people requiring treatment against neglected tropical diseases",
  "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases" = 
    "Number of people with mild or severe anemia from neglected tropical diseases",
  "Death.rate.from.venomous.snakes" = 
    "Death rate from venomous snakes",
  "Deaths.from.cysticercosis" = 
    "Deaths from cysticercosis",
  "Deaths.from.rabies.by.world.region" = 
    "Deaths from rabies by world region",
  "Dengue.fever.deaths" = 
    "Dengue fever deaths",
  "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis" = 
    "Number of people requiring preventive treatment for lymphatic filariasis",
  "Number.of.people.requiring.preventive.treatment.for.schistosomiasis" = 
    "Number of people requiring preventive treatment for schistosomiasis",
  "Reported.cases.of.leprosy" = 
    "Reported cases of leprosy",
  "Antibiotic.consumption.rate" = 
    "Antibiotic consumption rate"
)

# Define disease categories
hiv_indicators <- c(
  "Antiretroviral therapy coverage (% of people living with HIV)",
  "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
  "Children (0-14) living with HIV",
  "Children (ages 0-14) newly infected with HIV",
  "Incidence of HIV, all (per 1,000 uninfected population)"
)

tb_indicators <- c(
  "Incidence of tuberculosis (per 100,000 people)",
  "Tuberculosis case detection rate (%, all forms)",
  "Tuberculosis treatment success rate (% of new cases)"
)

malaria_indicators <- c(
  "Incidence of malaria (per 1,000 population at risk)",
  "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)"
)

ntd_indicators <- c(
  "Number of people requiring treatment against neglected tropical diseases",
  "Number of people with mild or severe anemia from neglected tropical diseases",
  "Number of people requiring preventive treatment for lymphatic filariasis",
  "Number of people requiring preventive treatment for schistosomiasis",
  "Reported cases of leprosy"
)

ntd_deaths_indicators <- c(
  "Death rate from venomous snakes",
  "Deaths from cysticercosis",
  "Deaths from rabies by world region",
  "Dengue fever deaths"
)

general_indicators <- c(
  "Number of DALYs",
  "Number of Deaths",
  "Death Rate",
  "Antibiotic consumption rate"
)


################################################################################
#### 3. Regression plot ####
################################################################################

# Step 1: Create a rank column for coefficients within each indicator and add clean names
region_regression_ranked <- region.regression %>%
  group_by(dependent_variable) %>%
  mutate(
    # Rank by actual value (not absolute value)
    coef_rank = rank(effect_size, ties.method = "min"),
    # Convert ranks to factor levels (1 = lowest value, 6 = highest value)
    rank_category = factor(coef_rank,
                           levels = 1:6,
                           labels = c("Lowest", "Very Low", "Low", "Medium", "High", "Highest")),
    # Add clean names for indicators
    indicator_name = indicator_names[dependent_variable]
  ) %>%
  ungroup()

# Step 2: Create category mapping with clean names
category_mapping <- data.frame(
  indicator_name = c(hiv_indicators, tb_indicators, malaria_indicators, 
                     ntd_indicators, ntd_deaths_indicators, general_indicators),
  category = c(rep("HIV", length(hiv_indicators)),
               rep("Tuberculosis", length(tb_indicators)),
               rep("Malaria", length(malaria_indicators)),
               rep("NTDs", length(ntd_indicators)),
               rep("NTD Deaths", length(ntd_deaths_indicators)),
               rep("General Health", length(general_indicators)))
)

# Merge category labels with data
region_regression_ranked <- region_regression_ranked %>%
  left_join(category_mapping, by = "indicator_name")

# Step 3: Add significance symbols based on p-values
region_regression_ranked <- region_regression_ranked %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Step 4: Create a label for each cell that includes significance
region_regression_ranked <- region_regression_ranked %>%
  mutate(cell_label = paste0(significance))

# Step 5: Prepare data for plotting by ordering categories and indicators
# Define order of categories
category_order <- c("HIV", "Tuberculosis", "Malaria", "NTDs", "NTD Deaths", "General Health")

# Order indicators within each category
plot_data <- region_regression_ranked %>%
  mutate(category = factor(category, levels = category_order)) %>%
  arrange(category, indicator_name) %>%
  mutate(indicator_ordered = factor(indicator_name, levels = unique(indicator_name[order(match(category, category_order))])))

# Step 6: Create the heatmap with different shapes for positive/negative coefficients
plot <- ggplot(plot_data, 
               aes(x = factor(region, levels = c("Americas", "Europe", "Eastern Mediterranean", "Western Pacific", "South-East Asia", "Africa")), 
                   y = indicator_ordered)) +
  # For positive effect (squares):
  # - OR >= 1
  # - beta or % change >= 0
  geom_tile(data = subset(plot_data, (effect_unit == "OR" & effect_size >= 1) | 
                            ((effect_unit == "beta" | effect_unit == "% change") & effect_size >= 0)),
            aes(fill = rank_category), color = "white", size = 0.5) +
  # For negative effect (circles):
  # - OR < 1
  # - beta or % change < 0
  geom_point(data = subset(plot_data, (effect_unit == "OR" & effect_size < 1) | 
                             ((effect_unit == "beta" | effect_unit == "% change") & effect_size < 0)),
             aes(fill = rank_category), color = "white", size = 6, shape = 21) +
  # Add significance symbols
  geom_text(aes(label = significance), size = 2.5) +
  # Changed to a palette that works better with 6 categories
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +  # Red-Yellow-Blue color palette with more gradations
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = element_blank(), #"Country Region Classification",
    y = "",
    fill = "Effect\nMagnitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 8, hjust = 1),
    axis.text.x = element_text(size = 9, face = "bold", angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0),
    strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Print the plot
print(plot)





################################################################################
#### 4. Hierarchical model plot ####
################################################################################

# Clean variable names for better display (same as before)
hierarchical_results <- hierarchical_results %>%
  mutate(
    dep_clean = case_when(
      str_detect(dependent_variable, "Antiretroviral.therapy.coverage....of.people.living.with.HIV") ~ 
        "Antiretroviral therapy coverage (% of people living with HIV)",
      str_detect(dependent_variable, "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV") ~ 
        "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
      str_detect(dependent_variable, "Children..0.14..living.with.HIV") ~ 
        "Children (0-14) living with HIV",
      str_detect(dependent_variable, "Children..ages.0.14..newly.infected.with.HIV") ~ 
        "Children (ages 0-14) newly infected with HIV",
      str_detect(dependent_variable, "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever") ~ 
        "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)",
      str_detect(dependent_variable, "Incidence.of.HIV..all..per.1.000.uninfected.population") ~ 
        "Incidence of HIV, all (per 1,000 uninfected population)",
      str_detect(dependent_variable, "Incidence.of.malaria..per.1.000.population.at.risk") ~ 
        "Incidence of malaria (per 1,000 population at risk)",
      str_detect(dependent_variable, "Incidence.of.tuberculosis..per.100.000.people") ~ 
        "Incidence of tuberculosis (per 100,000 people)",
      str_detect(dependent_variable, "Tuberculosis.case.detection.rate.....all.forms") ~ 
        "Tuberculosis case detection rate (%, all forms)",
      str_detect(dependent_variable, "Tuberculosis.treatment.success.rate....of.new.cases") ~ 
        "Tuberculosis treatment success rate (% of new cases)",
      str_detect(dependent_variable, "NUMBER.OF.DALYS") ~ 
        "Number of DALYs",
      str_detect(dependent_variable, "NUMBER.OF.DEATHS") ~ 
        "Number of Deaths",
      str_detect(dependent_variable, "DEATH.RATE") ~ 
        "Death Rate",
      str_detect(dependent_variable, "Number.of.people.requiring.treatment.against.neglected.tropical.diseases") ~ 
        "Number of people requiring treatment against neglected tropical diseases",
      str_detect(dependent_variable, "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases") ~ 
        "Number of people with mild or severe anemia from neglected tropical diseases",
      str_detect(dependent_variable, "Death.rate.from.venomous.snakes") ~ 
        "Death rate from venomous snakes",
      str_detect(dependent_variable, "Deaths.from.cysticercosis") ~ 
        "Deaths from cysticercosis",
      str_detect(dependent_variable, "Deaths.from.rabies.by.world.region") ~ 
        "Deaths from rabies by world region",
      str_detect(dependent_variable, "Dengue.fever.deaths") ~ 
        "Dengue fever deaths",
      str_detect(dependent_variable, "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis") ~ 
        "Number of people requiring preventive treatment for lymphatic filariasis",
      str_detect(dependent_variable, "Number.of.people.requiring.preventive.treatment.for.schistosomiasis") ~ 
        "Number of people requiring preventive treatment for schistosomiasis",
      str_detect(dependent_variable, "Reported.cases.of.leprosy") ~ 
        "Reported cases of leprosy",
      str_detect(dependent_variable, "Antibiotic.consumption.rate") ~ 
        "Antibiotic consumption rate",
      TRUE ~ dependent_variable
    ),
    
    indep_clean = case_when(
      independent_variable == "publication_count" ~ "Scientific publications",
      TRUE ~ independent_variable
    ),
    
    # Create model label showing what predicts what
    model_label = ifelse(
      independent_variable == "publication_count",
      paste("Publications →", dep_clean),
      paste(indep_clean, "→ Publications")
    ),
    
    # Create category groupings based on disease indicators
category = case_when(
  # HIV indicators
  dep_clean %in% c(
    "Antiretroviral therapy coverage (% of people living with HIV)",
    "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
    "Children (0-14) living with HIV",
    "Children (ages 0-14) newly infected with HIV",
    "Incidence of HIV, all (per 1,000 uninfected population)"
  ) ~ "HIV",
  
  # Tuberculosis indicators
  dep_clean %in% c(
    "Incidence of tuberculosis (per 100,000 people)",
    "Tuberculosis case detection rate (%, all forms)",
    "Tuberculosis treatment success rate (% of new cases)"
  ) ~ "Tuberculosis",
  
  # Malaria indicators
  dep_clean %in% c(
    "Incidence of malaria (per 1,000 population at risk)",
    "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)"
  ) ~ "Malaria",
  
  # Neglected Tropical Diseases (NTD) indicators
  dep_clean %in% c(
    "Number of people requiring treatment against neglected tropical diseases",
    "Number of people with mild or severe anemia from neglected tropical diseases",
    "Number of people requiring preventive treatment for lymphatic filariasis",
    "Number of people requiring preventive treatment for schistosomiasis",
    "Reported cases of leprosy"
  ) ~ "Neglected Tropical Diseases",
  
  # NTD deaths indicators
  dep_clean %in% c(
    "Death rate from venomous snakes",
    "Deaths from cysticercosis",
    "Deaths from rabies by world region",
    "Dengue fever deaths"
  ) ~ "NTD Deaths",
  
  # General health indicators
  dep_clean %in% c(
    "Number of DALYs",
    "Number of Deaths",
    "Death Rate",
    "Antibiotic consumption rate"
  ) ~ "General Health Indicators",
  
  # Default
  TRUE ~ "Other"
),
    
    # Set a specific order for categories
    category = factor(category, levels = c("HIV", "Tuberculosis", "Malaria",  "Neglected Tropical Diseases", "NTD Deaths", "General Health Indicators")),
    
    # Determine significance levels for coloring
    significance = case_when(
      adj_p_value < 0.001 ~ "p < 0.001",
      adj_p_value < 0.01 ~ "p < 0.01",
      adj_p_value < 0.05 ~ "p < 0.05",
      TRUE ~ "Not significant"
    )
    # Note: Removed the CI calculation here as we already have those values
  )

# Order by category and significance
hierarchical_results <- hierarchical_results %>%
  arrange(category, desc(abs(effect_size))) %>%  # Using effect_size instead of main_effect
  mutate(model_label = factor(model_label, levels = rev(unique(model_label))))

# Format effect sizes and CIs for display
hierarchical_results <- hierarchical_results %>%
  mutate(
    # Format effect size and CI for display using the new effect_size column
    effect_text = case_when(
      abs(effect_size) < 0.001 ~ sprintf("%.2e", effect_size),
      abs(effect_size) < 0.01 ~ sprintf("%.5f", effect_size),
      abs(effect_size) < 1 ~ sprintf("%.4f", effect_size),
      abs(effect_size) < 10 ~ sprintf("%.3f", effect_size),
      abs(effect_size) < 1000 ~ sprintf("%.2f", effect_size),
      abs(effect_size) < 1000000 ~ sprintf("%.1f", effect_size),
      TRUE ~ sprintf("%.2e", effect_size)
    ),
    
    # Format CI using the new effect_ci_lower and effect_ci_upper columns
    ci_text = case_when(
      # For very small/large values, use scientific notation
      abs(effect_size) < 0.001 | abs(effect_size) > 1000000 ~ 
        sprintf("(%.2e, %.2e)", effect_ci_lower, effect_ci_upper),
      
      # For small values, use more decimal places
      abs(effect_size) < 0.01 ~ 
        sprintf("(%.5f, %.5f)", effect_ci_lower, effect_ci_upper),
      
      # For medium values
      abs(effect_size) < 1 ~ 
        sprintf("(%.4f, %.4f)", effect_ci_lower, effect_ci_upper),
      
      # For standard values
      abs(effect_size) < 10 ~ 
        sprintf("(%.3f, %.3f)", effect_ci_lower, effect_ci_upper),
      
      # For larger values
      abs(effect_size) < 1000 ~ 
        sprintf("(%.2f, %.2f)", effect_ci_lower, effect_ci_upper),
      
      # For much larger values
      abs(effect_size) < 1000000 ~ 
        sprintf("(%.1f, %.1f)", effect_ci_lower, effect_ci_upper),
      
      # Default to scientific notation for very large values
      TRUE ~ sprintf("(%.2e, %.2e)", effect_ci_lower, effect_ci_upper)
    ),
    
    # Complete display text
    display_text = paste0(effect_text, " ", ci_text)
  )

# Create a function for symmetric log transformation (handles negative values)
symlog_trans <- function(base = 10, threshold = 1, scale = 1) {
  trans <- function(x) {
    sign(x) * log10(1 + abs(x)/threshold) * scale
  }
  
  inv <- function(x) {
    sign(x) * (base^(abs(x)/scale) - 1) * threshold
  }
  
  scales::trans_new(
    name = paste0("symlog-", format(threshold)),
    transform = trans,
    inverse = inv,
    domain = c(-Inf, Inf),
    breaks = scales::extended_breaks(),
    format = scales::format_format(scientific = FALSE)
  )
}

# Find a good threshold for the symlog transformation
min_magnitude <- min(abs(hierarchical_results$effect_size[hierarchical_results$effect_size != 0]))
threshold <- min_magnitude / 10

# Create the forest plot with symmetric log scale
p <- ggplot(hierarchical_results, 
            aes(y = model_label, x = effect_size, xmin = effect_ci_lower, xmax = effect_ci_upper)) +
  # Add vertical line at x=0
  geom_vline(xintercept = 0, color = "black", size = 0.5) +
  # Add light reference lines ALIGNED WITH X-AXIS BREAKS
  geom_vline(xintercept = c(-10^7, -10^5, -10^3, -10, -1, -0.1, 0.1, 1, 10, 10^3, 10^5, 10^7), 
             color = "gray90", size = 0.3, linetype = "solid", alpha = 0.5) +
  # Add horizontal lines for the CIs WITHOUT end caps
  geom_errorbarh(height = 0, color = "black", linewidth = 0.3) +
  # Add points with black border and colored fill, smaller size
  geom_point(aes(fill = significance, 
                 size = 3),
             shape = 21, color = "black", stroke = 0.4) +
  # Add labels for model type
  geom_text(aes(label = distribution), hjust = -0.2, vjust = -0.5, 
            size = 2.8, color = "gray50") +
  # Facet by category
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  # Use a colorblind-friendly palette for fill
  scale_fill_manual(values = c(
    "p < 0.001" = "#00a6fb",  # Blue
    "p < 0.01" = "#009E73",   # Green
    "p < 0.05" = "#D55E00",   # Orange
    "Not significant" = "#CCCCCC" # Light gray for non-significant
  )) +
  scale_size_identity() +
  # Use symmetric log scale for x-axis with CAREFULLY SELECTED BREAKS
  scale_x_continuous(
    trans = symlog_trans(threshold = threshold),
    breaks = c(-10^7, -10^5, -10^3, -10, -1, -0.1, 0, 0.1, 1, 10, 10^3, 10^5, 10^7),
    labels = function(x) {
      ifelse(x == 0, "0", 
             ifelse(abs(x) < 0.001 | abs(x) > 999, 
                    scales::scientific(x), 
                    scales::comma(x)))
    }
  ) +
  labs(
    x = "Effect size (symmetric log scale)",
    y = NULL,
    fill = "Significance"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.spacing = unit(1, "lines")
  )

# Create a table for the right side
# First, prepare the table data
table_data <- hierarchical_results %>%
  select(model_label, display_text, category) %>%
  arrange(match(model_label, levels(hierarchical_results$model_label)))

# Create a ggplot for the text table
p_table <- ggplot(table_data, aes(x = 1, y = model_label)) +
  geom_text(aes(label = display_text), hjust = 0, size = 2.8) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(limits = c(1, 2), expand = c(0, 0)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 10, r = 5, b = 10, l = 0),
    # Align the text with the main plot's y-axis positioning
    axis.text.y = element_blank()
  )

# Create a title for the table
p_table_title <- ggplot() +
  annotate("text", x = 1, y = 0.5, label = "β / OR (95% CI)", size = 3.5, fontface = "bold") +
  theme_void() +
  theme(
    plot.margin = margin(t = 18, r = 5, b = 0, l = 0)  # Adjust top margin to align with main plot title
  )

# Combine into a single display
# First combine the table title with table
table_combined <- gridExtra::grid.arrange(p_table_title, p_table, 
                                          ncol = 1, heights = c(0.05, 0.95))

# Then combine the main plot with the table
final_plot <- gridExtra::grid.arrange(p, table_combined, 
                                      ncol = 2, widths = c(0.8, 0.2))

# Display the final combined plot
print(final_plot)




################################################################################
#### 5. Indicators plots ####
################################################################################

# Load required libraries if not already loaded
library(dplyr)
library(ggplot2)
library(scales)
library(ggtext)
library(ggsci) # For the NPG color palette

# Create mapping for clean indicator names IN THE SPECIFIED ORDER
indicator_names <- c(
  # HIV indicators
  "Antiretroviral.therapy.coverage....of.people.living.with.HIV." = 
    "Antiretroviral therapy coverage (% of people living with HIV)",
  "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV." = 
    "Antiretroviral therapy coverage for PMTCT (% of pregnant women living with HIV)",
  "Children..0.14..living.with.HIV" = 
    "Children (0-14) living with HIV",
  "Children..ages.0.14..newly.infected.with.HIV" = 
    "Children (ages 0-14) newly infected with HIV",
  "Incidence.of.HIV..all..per.1.000.uninfected.population." = 
    "Incidence of HIV, all (per 1,000 uninfected population)",
  
  # TB indicators
  "Incidence.of.tuberculosis..per.100.000.people." = 
    "Incidence of tuberculosis (per 100,000 people)",
  "Tuberculosis.case.detection.rate.....all.forms." = 
    "Tuberculosis case detection rate (%, all forms)",
  "Tuberculosis.treatment.success.rate....of.new.cases." = 
    "Tuberculosis treatment success rate (% of new cases)",
  
  # Malaria indicators
  "Incidence.of.malaria..per.1.000.population.at.risk." = 
    "Incidence of malaria (per 1,000 population at risk)",
  "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever." = 
    "Children with fever receiving antimalarial drugs (% of children under age 5 with fever)",
  
  # NTD indicators
  "Number.of.people.requiring.treatment.against.neglected.tropical.diseases" = 
    "Number of people requiring treatment against neglected tropical diseases",
  "Number.of.people.with.mild.or.severe.anemia.from.neglected.tropical.diseases" = 
    "Number of people with mild or severe anemia from neglected tropical diseases",
  "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis" = 
    "Number of people requiring preventive treatment for lymphatic filariasis",
  "Number.of.people.requiring.preventive.treatment.for.schistosomiasis" = 
    "Number of people requiring preventive treatment for schistosomiasis",
  "Reported.cases.of.leprosy" = 
    "Reported cases of leprosy",
  
  # NTD deaths indicators
  "Death.rate.from.venomous.snakes" = 
    "Death rate from venomous snakes",
  "Deaths.from.cysticercosis" = 
    "Deaths from cysticercosis",
  "Deaths.from.rabies.by.world.region" = 
    "Deaths from rabies by world region",
  "Dengue.fever.deaths" = 
    "Dengue fever deaths",
  
  # General indicators
  "NUMBER.OF.DALYS" = 
    "Number of DALYs",
  "NUMBER.OF.DEATHS" = 
    "Number of Deaths",
  "DEATH.RATE" = 
    "Death Rate",
  "Antibiotic.consumption.rate" = 
    "Antibiotic consumption rate"
)

# Define the desired order of regions
region_order <- c("Americas", "Europe", "Eastern Mediterranean", "Western Pacific", "South-East Asia", "Africa")

# Function to create a combined line plot with embedded trend information
generate_combined_plot <- function(data, indicator_col) {
  # Get the clean title for the indicator
  indicator_title <- indicator_names[indicator_col]
  if (is.null(indicator_title)) indicator_title <- indicator_col # Fallback if name not found
  
  # Extract y-label from title
  unit_match <- regmatches(indicator_title, regexpr("\\([^)]+\\)$", indicator_title))
  if (length(unit_match) > 0) {
    y_label <- gsub("[()]", "", unit_match)
  } else {
    y_label <- indicator_title
  }
  
  # Check if there's any data for this indicator
  if (all(is.na(data[[indicator_col]]))) {
    # Create an empty plot with a message if no data available
    p <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No data available for this indicator") +
      theme_void() +
      labs(title = indicator_title)
    return(p)
  }
  
  # Create a working copy of data with region as an ordered factor
  plot_data <- data %>%
    mutate(region = factor(region, levels = region_order))
  
  # Create the summary data (mean per year and region)
  data_summary <- plot_data %>%
    filter(!is.na(!!sym(indicator_col))) %>%
    group_by(region, PY) %>%
    summarize(value = mean(!!sym(indicator_col), na.rm = TRUE), .groups = "drop") %>%
    # Filter out any groups where mean resulted in NA (can happen if all values were NA)
    filter(!is.na(value))
  
  # Check if we have enough data to plot
  if (nrow(data_summary) < 2) {
    # Create an empty plot with a message if insufficient data
    p <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for plotting") +
      theme_void() +
      labs(title = indicator_title)
    return(p)
  }
  
  # Calculate trends for each region
  trend_data <- data_summary %>%
    group_by(region) %>%
    arrange(PY) %>%
    filter(n() >= 2) %>% # Need at least two points for trend
    summarize(
      first_year = first(PY),
      last_year = last(PY),
      first_value = first(value),
      last_value = last(value),
      # Safe percentage calculation to avoid division by zero
      pct_change = if_else(first_value == 0, 
                           if_else(last_value > 0, Inf, 0),
                           (last_value - first_value) / first_value * 100),
      years_span = last_year - first_year,
      .groups = "drop"
    ) %>%
    # Add color and icon based on direction
    mutate(
      direction_icon = ifelse(pct_change >= 0, "▲", "▼"),
      direction_color = ifelse(pct_change >= 0, "#00A087FF", "#E64B35FF"),
      # Handle special cases for percentage display
      label_text = case_when(
        is.infinite(pct_change) & pct_change > 0 ~ "▲ ∞",
        is.infinite(pct_change) & pct_change < 0 ~ "▼ -∞",
        is.nan(pct_change) ~ "◆ N/A",
        abs(pct_change) > 1000 ~ sprintf("%s >1000%%", direction_icon),
        TRUE ~ sprintf("%s %s", direction_icon, percent(abs(pct_change/100), accuracy = 0.1))
      )
    )
  
  # Get the last data point for each region for annotation positioning
  annotation_points <- data_summary %>%
    group_by(region) %>%
    filter(PY == max(PY)) %>%
    left_join(trend_data, by = "region")
  
  # Generate the plot with embedded trend information
  p <- ggplot() +
    # Add a subtle area under the curve - WITH show.legend=FALSE to avoid legend artifacts
    geom_area(
      data = data_summary,
      aes(x = PY, y = value, fill = region),
      alpha = 0.2,
      show.legend = FALSE
    ) +
    # The line plot
    geom_line(
      data = data_summary, 
      aes(x = PY, y = value, color = region),
      size = 0.6, lineend = "round"
    ) +
    # Add a small point at the end of each line
    geom_point(
      data = annotation_points,
      aes(x = PY, y = value, color = region),
      size = 1.5
    ) +
    # Add trend annotations next to the end of each line
    geom_richtext(
      data = annotation_points,
      aes(
        x = PY + 0.5,
        y = value,
        label = label_text,
        color = region
      ),
      hjust = 0, 
      vjust = 0.5,
      size = 3,
      fill = NA,
      label.color = NA,
      show.legend = FALSE
    ) +
    labs(
      title = indicator_title,
      x = NULL,
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    # Extend x-axis slightly to accommodate annotations
    scale_x_continuous(
      breaks = function(limits) {
        min_year <- floor(min(data_summary$PY, na.rm = TRUE))
        max_year <- ceiling(max(data_summary$PY, na.rm = TRUE))
        if (max_year - min_year <= 10) {
          return(seq(min_year, max_year, by = 1))
        } else {
          return(seq(min_year, max_year, by = 2))
        }
      },
      limits = c(NA, max(data_summary$PY, na.rm = TRUE) + 3),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      minor_breaks = NULL,
      labels = function(x) {
        # Format numbers with commas for thousands separators
        format(x, big.mark = ",", scientific = FALSE)
      }
    ) +
    scale_color_npg() +
    scale_fill_npg() +
    # Override guides to ensure clean legend
    guides(
      color = guide_legend(override.aes = list(fill = NA))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, face = "bold"),
      axis.line.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dashed", color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      axis.ticks.x = element_line(size = 0.5),
      axis.ticks.length.x = unit(2, "pt"),
      axis.ticks.y = element_blank(),
      plot.margin = margin(10, 25, 10, 10)  # Add right margin for annotations
    )
  
  return(p)
}

# Before processing, convert region to a factor with desired order
data <- data %>% 
  mutate(region = factor(region, levels = region_order))

# Get list of indicator columns (columns 4:26)
indicator_cols <- names(data)[4:26]

# Reorder indicator_cols to match the desired order in indicator_names
ordered_indicator_cols <- names(indicator_names)[names(indicator_names) %in% indicator_cols]
# Add any indicators that might be in data but not in our named list
remaining_indicators <- setdiff(indicator_cols, ordered_indicator_cols)
indicator_cols <- c(ordered_indicator_cols, remaining_indicators)

# Create a list to store the combined plots
combined_plots <- list()

# Generate all combined plots with error handling in the specified order
for (indicator in indicator_cols) {
  tryCatch({
    # Generate combined plot
    combined_plots[[indicator]] <- generate_combined_plot(data, indicator)
    
    # Print completion message
    cat("Generated combined plot for:", indicator_names[indicator], "\n")
  }, error = function(e) {
    # Create a placeholder plot with error message if plotting fails
    combined_plots[[indicator]] <<- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Error generating plot:", e$message)) +
      theme_void() +
      labs(title = indicator_names[indicator])
    
    cat("ERROR plotting", indicator, ":", e$message, "\n")
  })
}

# Display the first plot to start
print(combined_plots[[1]])

# Function to display all combined plots sequentially
display_all_combined_plots <- function() {
  # Use indicator_cols to ensure plots are displayed in the desired order
  for (indicator in indicator_cols) {
    cat("Displaying plot for:", indicator_names[indicator], "\n")
    print(combined_plots[[indicator]])
    # Uncomment below if you want to pause between plots
    # readline(prompt = "Press [Enter] to see next plot")
  }
}

# Display all plots
display_all_combined_plots()

# Function to save plots to PDF
#save_plots_to_pdf <- function(filename = "regional_health_indicators.pdf", width = 10, height = 7) {
#  pdf(filename, width = width, height = height)
#  # Use indicator_cols to ensure plots are saved in the desired order
#  for (indicator in indicator_cols) {
#    print(combined_plots[[indicator]])
#  }
#  dev.off()
#  cat("Saved all plots to", filename, "\n")
#}

# Uncomment to save plots
# save_plots_to_pdf()