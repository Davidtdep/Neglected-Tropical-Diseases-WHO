################################################################################
#### 0. Libraires ####
################################################################################

library(readxl)
library(MASS)       # For negative binomial regression
library(betareg)    # For beta regression
library(stats)      # For standard regression functions
library(dplyr)      # For data manipulation
library(car)        # For diagnostic tests
library(lmtest)     # For regression diagnostics




################################################################################
#### 1. Data input ####
################################################################################

# Raw data
data_filtered <- read.csv("~/Desktop/neglectedDiseases/data/data_filtered.csv")

# Aggregated data
region_data_filtered <- read_excel("~/Desktop/RegionNTDs001/manuscript/Supplementary Material 1.xlsx")

# Results data
region.regression  = read_excel("~/Desktop/RegionNTDs001/manuscript/Supplementary Material 2.xlsx")


################################################################################
#### 2. Data depuration ####
################################################################################

# Filter years 2000 - 2024 in region_data and income_data
region_data_filtered <- region_data %>%
  filter(PY >= 2000 & PY <= 2024)


#### Eliminate everything except income_data_filtered & region_data_filtered from the environment in R
rm(list = setdiff(ls(), c("region_data_filtered", "data_filtered")))






################################################################################
#### 2. Categorizing indicators as dependent vs. independent ####
################################################################################

# Define which indicators are likely dependent variables (health outcomes) 
# vs independent variables (resources/investments)
categorize_indicators <- function(columns_to_group) {
  # Keywords that suggest a variable is a dependent outcome
  outcome_keywords <- c(
    "prevalence", "incidence", "deaths", "mortality", "case", "dalys",
    "people requiring treatment", "anemia", "death rate", "requiring preventive",
    "reported cases", "attributed to", "homelessness"
  )
  
  # Keywords that suggest a variable is an independent predictor
  predictor_keywords <- c(
    "expenditure", "physicians", "nurses", "research and development", 
    "funding", "intellectual property", "education", "index", 
    "coefficient", "gdp", "years of schooling", "literacy", "capacity", "corruption"
  )
  
  # Specific indicators that must be treated as dependent variables (overriding keyword classification)
  forced_dependent_vars <- c(
    "DEATH.RATE",
    "Healthy.life.expectancy",
    "Antiretroviral.therapy.coverage....of.people.living.with.HIV.",
    "Antiretroviral.therapy.coverage.for.PMTCT....of.pregnant.women.living.with.HIV.",
    "Children..0.14..living.with.HIV",
    "Children..ages.0.14..newly.infected.with.HIV",
    "Children.with.fever.receiving.antimalarial.drugs....of.children.under.age.5.with.fever.",
    "HEALTHCARE.ACCESS.AND.QUALITY",
    "Life.expectancy.at.birth",
    "Antibiotic.consumption.rate",
    "Share.of.the.population.using.safely.managed.drinking.water.sources",
    "Share.of.the.population.with.access.to.basic.handwashing.facilities",
    "Share.of.the.population.using.basic.sanitation.service",
    "Share of deaths attributed to unsafe sanitation",
    "Share.of.people.practicing.open.defecation",
    "drugs.pipeline.for.neglected.tropical.diseases.by.country",
    "drugs.pipeline.for.neglected.tropical.diseases.by.region",
    "drugs.pipeline.for.neglected.tropical.diseases.by.income",
    "Number.of.people.requiring.treatment.against.neglected.tropical.diseases",
    "Death.rate.from.venomous.snakes",
    "Number.of.people.requiring.preventive.treatment.for.lymphatic.filariasis",
    "Number.of.people.requiring.preventive.treatment.for.schistosomiasis",
    "OUT.OF.POCKED.EXPENDITURE.ON.HEALTH",
    "CURRENT.HEALTH.EXPENDITURE....OF.GDP."
  )
  
  # Initialize empty lists for both categories
  dependent_vars <- character(0)
  independent_vars <- character(0)
  
  # Categorize each indicator
  for (column in columns_to_group) {
    # If it's in the forced dependent list, categorize as dependent
    if (column %in% forced_dependent_vars) {
      dependent_vars <- c(dependent_vars, column)
      next
    }
    
    column_lower <- tolower(column)
    
    # Check if any outcome keywords are in the column name
    is_outcome <- any(sapply(outcome_keywords, function(kw) grepl(tolower(kw), column_lower)))
    
    # Check if any predictor keywords are in the column name
    is_predictor <- any(sapply(predictor_keywords, function(kw) grepl(tolower(kw), column_lower)))
    
    if (is_outcome && !is_predictor) {
      dependent_vars <- c(dependent_vars, column)
    } else if (is_predictor && !is_outcome) {
      independent_vars <- c(independent_vars, column)
    } else if (is_outcome && is_predictor) {
      # For ambiguous cases, default to independent variable
      independent_vars <- c(independent_vars, column)
    } else {
      # If neither clear, default to independent variable
      independent_vars <- c(independent_vars, column)
    }
  }
  
  return(list(dependent_vars = dependent_vars, independent_vars = independent_vars))
}


# Save the names of the columns c(16:70, 77:99)
columns_to_group <- names(region_data_filtered)[c(4:77)]


# Categorize indicators as dependent or independent
indicator_categories <- categorize_indicators(columns_to_group)
dependent_vars <- indicator_categories$dependent_vars
independent_vars <- indicator_categories$independent_vars




################################################################################
#### 3. Regression Analyses ####
################################################################################

#' Print data summary for debugging
#' @param y Dependent variable vector
#' @param x Independent variable vector
debug_data <- function(dep_var_name, indep_var_name, region_name, y, x) {
  cat("\nDEBUG INFO:", region_name, "-", dep_var_name, "~", indep_var_name, "\n")
  cat("Y summary:", summary(y), "\n")
  cat("X summary:", summary(x), "\n")
  cat("Complete cases:", sum(complete.cases(y, x)), "out of", length(y), "\n\n")
}

#' Determine the appropriate regression model type based on data characteristics
#' @param y Dependent variable vector
#' @param x Independent variable vector
#' @param dep_var_name Name of dependent variable (for debugging)
#' @param indep_var_name Name of independent variable (for debugging)
#' @param region_name Name of the region (for debugging)
#' @param data_type Optional parameter to force a specific model type
#' @return String indicating the appropriate model type
determine_model_type <- function(y, x, dep_var_name = "", indep_var_name = "", region_name = "", data_type = NULL) {
  # Remove NA values
  complete_cases <- complete.cases(y, x)
  
  if (sum(complete_cases) < 5) {
    cat("Insufficient complete cases for", region_name, "-", dep_var_name, "~", indep_var_name, "\n")
    return(NULL)
  }
  
  y <- y[complete_cases]
  x <- x[complete_cases]
  
  # Print debug information if needed
  # debug_data(dep_var_name, indep_var_name, region_name, y, x)
  
  # If data_type is specified, use that model type
  if (!is.null(data_type)) {
    return(data_type)
  }
  
  # Check if y has variation
  if (length(unique(y)) <= 1) {
    cat("No variation in dependent variable for", region_name, "-", dep_var_name, "~", indep_var_name, "\n")
    return(NULL)
  }
  
  # Check if x has variation
  if (length(unique(x)) <= 1) {
    cat("No variation in independent variable for", region_name, "-", dep_var_name, "~", indep_var_name, "\n")
    return(NULL)
  }
  
  # Check if y is count data (integer values)
  if (all(y == floor(y)) && all(y >= 0)) {
    # Calculate variance to mean ratio for count data
    var_mean_ratio <- var(y) / mean(y)
    
    if (!is.na(var_mean_ratio)) {  # Add check for NA
      if (var_mean_ratio >= 0.7 && var_mean_ratio <= 1.5) {
        return("poisson")
      } else if (var_mean_ratio > 1.5) {
        return("negative_binomial")
      }
    }
  }
  
  # Check if y is a proportion (between 0 and 1)
  if (all(y >= 0 & y <= 1) && (max(y) - min(y) > 0.05)) {
    # Check if y contains exact 0s or 1s
    has_zeros_or_ones <- any(y == 0) || any(y == 1)
    if (has_zeros_or_ones) {
      # Use transformed beta regression for proportions including 0 or 1
      return("beta_transformed")
    } else {
      return("beta")
    }
  }
  
  # For binary data (only 0 and 1)
  if (all(y %in% c(0, 1))) {
    return("binomial")
  }
  
  # Test for normality using Shapiro-Wilk test if we have enough data but not too much
  if (length(y) >= 5 && length(y) <= 5000) {
    sw_test <- tryCatch(shapiro.test(y), error = function(e) list(p.value = 0))
    if (!is.na(sw_test$p.value) && sw_test$p.value > 0.05) {
      return("gaussian")
    } else {
      # For non-normal continuous data, try log transformation
      if (all(y > 0)) {
        sw_test_log <- tryCatch(shapiro.test(log(y)), error = function(e) list(p.value = 0))
        if (!is.na(sw_test_log$p.value) && sw_test_log$p.value > 0.05) {
          return("gaussian_log")
        }
      }
    }
  }
  
  # Default to gaussian if nothing else fits
  return("gaussian")
}

#' Transform data for beta regression to avoid exact 0s and 1s
#' @param y Vector of proportion values
#' @return Transformed vector suitable for beta regression
transform_beta_data <- function(y) {
  n <- length(y)
  # Apply transformation from Smithson & Verkuilen (2006)
  y_transformed <- (y * (n - 1) + 0.5) / n
  return(y_transformed)
}

#' Calculate pseudo R-squared for generalized linear models
#' @param model The fitted model object
#' @return Numeric pseudo R-squared value
calculate_pseudo_rsquared <- function(model) {
  # For beta regression models
  if (inherits(model, "betareg")) {
    if (!is.null(model$loglik) && !is.null(model$null.loglik)) {
      return(1 - model$loglik / model$null.loglik)
    }
    return(NA)
  }
  
  # For GLMs (poisson, binomial, etc.)
  if (inherits(model, "glm")) {
    null_deviance <- model$null.deviance
    residual_deviance <- model$deviance
    if (!is.null(null_deviance) && !is.null(residual_deviance) && null_deviance != 0) {
      return(1 - residual_deviance / null_deviance)
    }
    return(NA)
  }
  
  # For negative binomial models
  if (inherits(model, "negbin")) {
    tryCatch({
      null_model <- update(model, . ~ 1)
      if (!is.null(logLik(model)) && !is.null(logLik(null_model)) && 
          !is.na(logLik(null_model)) && logLik(null_model) != 0) {
        return(1 - logLik(model) / logLik(null_model))
      }
    }, error = function(e) {
      return(NA)
    })
    return(NA)
  }
  
  # Default to NA if model type not recognized
  return(NA)
}

#' Extract coefficient information safely
#' @param model The fitted model
#' @param var_name Name of the variable to extract coefficient for
#' @return List containing coefficient information
extract_coefficient_info <- function(model, var_name = "x") {
  result <- list(
    coefficient = NA,
    std_error = NA,
    test_statistic = NA,
    p_value = NA,
    lower_ci = NA,
    upper_ci = NA,
    test_statistic_type = NA
  )
  
  # Try to extract coefficient information
  tryCatch({
    if (inherits(model, "lm") || inherits(model, "glm") || inherits(model, "negbin")) {
      model_summary <- summary(model)
      coef_table <- coef(model_summary)
      
      if (var_name %in% rownames(coef_table)) {
        result$coefficient <- coef_table[var_name, "Estimate"]
        result$std_error <- coef_table[var_name, "Std. Error"]
        
        # Check if it's a t or z statistic
        if ("t value" %in% colnames(coef_table)) {
          result$test_statistic <- coef_table[var_name, "t value"]
          result$p_value <- coef_table[var_name, "Pr(>|t|)"]
          result$test_statistic_type <- "t"
        } else {
          result$test_statistic <- coef_table[var_name, "z value"]
          result$p_value <- coef_table[var_name, "Pr(>|z|)"]
          result$test_statistic_type <- "z"
        }
        
        # Extract confidence intervals
        ci <- tryCatch(
          confint(model)[var_name, ],
          error = function(e) c(NA, NA)
        )
        result$lower_ci <- ci[1]
        result$upper_ci <- ci[2]
      }
    } else if (inherits(model, "betareg")) {
      model_summary <- summary(model)
      
      # For betareg models, coefficients are stored in a nested structure
      # We need to access the 'mean' component which contains our predictor coefficients
      coef_table <- model_summary$coefficients$mean
      
      if (var_name %in% rownames(coef_table)) {
        result$coefficient <- coef_table[var_name, "Estimate"]
        result$std_error <- coef_table[var_name, "Std. Error"]
        result$test_statistic <- coef_table[var_name, "z value"]
        result$p_value <- coef_table[var_name, "Pr(>|z|)"]
        result$test_statistic_type <- "z"
        
        # Extract confidence intervals
        ci <- tryCatch({
          # Beta regression confint output needs special handling
          all_ci <- confint(model)
          # Filter for coefficients related to the mean model (not precision)
          mean_var_name <- paste0("mean_", var_name)
          if (mean_var_name %in% rownames(all_ci)) {
            all_ci[mean_var_name, ]
          } else {
            # Try without the prefix as a fallback
            all_ci[var_name, ]
          }
        }, error = function(e) {
          c(NA, NA)
        })
        
        result$lower_ci <- ci[1]
        result$upper_ci <- ci[2]
      }
    }
  }, error = function(e) {
    cat("Error extracting coefficients:", e$message, "\n")
  })
  
  return(result)
}

#' Fit regression model and extract statistics
#' @param y Dependent variable vector
#' @param x Independent variable vector
#' @param model_type String indicating model type
#' @param dep_var_name Name of dependent variable
#' @param indep_var_name Name of independent variable
#' @param region_name Name of the region
#' @return List of model statistics
run_regression_model <- function(y, x, model_type, dep_var_name, indep_var_name, region_name) {
  # Remove NA values
  complete_cases <- complete.cases(y, x)
  y <- y[complete_cases]
  x <- x[complete_cases]
  
  if (length(y) < 5) {
    cat("Insufficient data points for", region_name, "-", dep_var_name, "~", indep_var_name, "\n")
    return(NULL)
  }
  
  # Create data frame for regression
  df <- data.frame(y = y, x = x)
  
  # Initialize variables
  model <- NULL
  r_squared <- NA
  adj_r_squared <- NA
  pseudo_r_squared <- NA
  aic_value <- NA
  
  # Fit model based on model_type
  tryCatch({
    if (model_type == "gaussian") {
      model <- lm(y ~ x, data = df)
      
      # Extract basic model fit statistics
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      aic_value <- AIC(model)
      
    } else if (model_type == "gaussian_log") {
      # Log-transform the dependent variable
      df$y_log <- log(df$y)
      
      model <- lm(y_log ~ x, data = df)
      
      # Extract basic model fit statistics
      summary_model <- summary(model)
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      aic_value <- AIC(model)
      
    } else if (model_type == "poisson") {
      model <- glm(y ~ x, data = df, family = poisson())
      
      # Calculate pseudo R-squared
      pseudo_r_squared <- calculate_pseudo_rsquared(model)
      aic_value <- AIC(model)
      
    } else if (model_type == "negative_binomial") {
      model <- MASS::glm.nb(y ~ x, data = df)
      
      # Calculate pseudo R-squared
      pseudo_r_squared <- calculate_pseudo_rsquared(model)
      aic_value <- AIC(model)
      
    } else if (model_type == "binomial") {
      model <- glm(y ~ x, data = df, family = binomial())
      
      # Calculate pseudo R-squared
      pseudo_r_squared <- calculate_pseudo_rsquared(model)
      aic_value <- AIC(model)
      
    } else if (model_type == "beta" || model_type == "beta_transformed") {
      # Transform data if needed
      if (model_type == "beta_transformed") {
        df$y_beta <- transform_beta_data(df$y)
      } else {
        df$y_beta <- df$y
      }
      
      model <- betareg(y_beta ~ x, data = df)
      
      # Calculate pseudo R-squared
      pseudo_r_squared <- calculate_pseudo_rsquared(model)
      aic_value <- AIC(model)
    }
    
    # Check if model was successfully fit
    if (is.null(model)) {
      cat("Model fitting failed for", region_name, "-", dep_var_name, "~", indep_var_name, "\n")
      return(NULL)
    }
    
    # Extract coefficient information
    coef_info <- extract_coefficient_info(model, "x")
    
    # Create result list with all statistics
    result <- list(
      region = region_name,
      dependent_variable = dep_var_name,
      independent_variable = indep_var_name,
      model_type = model_type,
      n_observations = length(y),
      coefficient = coef_info$coefficient,
      std_error = coef_info$std_error,
      test_statistic = coef_info$test_statistic,
      test_statistic_type = coef_info$test_statistic_type,
      p_value = coef_info$p_value,
      lower_ci = coef_info$lower_ci,
      upper_ci = coef_info$upper_ci,
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      pseudo_r_squared = pseudo_r_squared,
      aic = aic_value
    )
    
    return(result)
    
  }, error = function(e) {
    cat("Error in model fitting for", region_name, ":", dep_var_name, "~", indep_var_name, "\n")
    cat(e$message, "\n")
    return(NULL)
  })
}

#' Apply Benjamini-Hochberg correction to p-values
#' @param results_df Dataframe with model results
#' @return Dataframe with adjusted p-values
adjust_p_values <- function(results_df) {
  if (nrow(results_df) == 0 || !("p_value" %in% colnames(results_df))) {
    return(results_df)
  }
  
  # Initialize adjusted p-value column
  results_df$p_value_adjusted <- NA
  
  # Adjust p-values within each region
  regions <- unique(results_df$region)
  
  for (region in regions) {
    region_idx <- which(results_df$region == region)
    if (length(region_idx) > 0) {
      # Only adjust non-NA p-values
      valid_p_idx <- region_idx[!is.na(results_df$p_value[region_idx])]
      if (length(valid_p_idx) > 0) {
        results_df$p_value_adjusted[valid_p_idx] <- p.adjust(
          results_df$p_value[valid_p_idx], 
          method = "BH"
        )
      }
    }
  }
  
  # Add significance indicator column
  results_df$significant <- !is.na(results_df$p_value_adjusted) & 
    results_df$p_value_adjusted < 0.05
  
  return(results_df)
}

#' Check if variable has sufficient non-missing data for analysis
#' @param data Data frame
#' @param var_name Variable name
#' @param min_obs Minimum number of observations required
#' @return Boolean indicating if variable has sufficient data
has_sufficient_data <- function(data, var_name, min_obs = 5) {
  if (!(var_name %in% colnames(data))) {
    return(FALSE)
  }
  
  sum(!is.na(data[[var_name]])) >= min_obs
}

# Main analysis function
run_analysis <- function(region_data_filtered, dependent_vars, independent_vars) {
  # Initialize results list
  results_list <- list()
  result_count <- 0
  
  # Get unique regions
  regions <- unique(region_data_filtered$region)
  
  cat("Starting analysis for", length(regions), "regions\n")
  
  # Loop through each region
  for (region_name in regions) {
    cat("\nProcessing region:", region_name, "\n")
    
    # Subset data for this region
    region_data <- region_data_filtered[region_data_filtered$region == region_name, ]
    
    # Skip if we have fewer than 5 observations
    if (nrow(region_data) < 5) {
      cat("Skipping region", region_name, "due to insufficient observations\n")
      next
    }
    
    # Publication count for this region
    pub_count <- region_data$publication_count
    
    if (sum(!is.na(pub_count)) < 5) {
      cat("Insufficient publication count data for region", region_name, "\n")
      next
    }
    
    # 1. Models where indicators are dependent variables and publication count is independent
    cat("Running models with publication count as independent variable...\n")
    for (dep_var in dependent_vars) {
      # Check if variable exists and has sufficient data
      if (!has_sufficient_data(region_data, dep_var)) {
        next
      }
      
      # Extract dependent variable values
      dep_values <- region_data[[dep_var]]
      
      # Determine model type
      model_type <- determine_model_type(
        dep_values, pub_count, 
        dep_var, "publication_count", region_name
      )
      
      # Skip if model type could not be determined
      if (is.null(model_type)) {
        next
      }
      
      # Run regression model
      model_results <- run_regression_model(
        dep_values, 
        pub_count, 
        model_type, 
        dep_var, 
        "publication_count", 
        region_name
      )
      
      # Store results if successful
      if (!is.null(model_results)) {
        result_count <- result_count + 1
        results_list[[result_count]] <- model_results
      }
    }
    
    # 2. Models where publication count is dependent variable and indicators are independent
    cat("Running models with publication count as dependent variable...\n")
    for (indep_var in independent_vars) {
      # Check if variable exists and has sufficient data
      if (!has_sufficient_data(region_data, indep_var)) {
        next
      }
      
      # Extract independent variable values
      indep_values <- region_data[[indep_var]]
      
      # Determine model type for publication count as dependent variable
      model_type <- determine_model_type(
        pub_count, indep_values, 
        "publication_count", indep_var, region_name
      )
      
      # Skip if model type could not be determined
      if (is.null(model_type)) {
        next
      }
      
      # Run regression model
      model_results <- run_regression_model(
        pub_count, 
        indep_values, 
        model_type, 
        "publication_count", 
        indep_var, 
        region_name
      )
      
      # Store results if successful
      if (!is.null(model_results)) {
        result_count <- result_count + 1
        results_list[[result_count]] <- model_results
      }
    }
    
    cat("Completed analysis for region:", region_name, "\n")
  }
  
  cat("\nFinished running all models. Converting results to dataframe...\n")
  
  # Convert results list to dataframe
  if (length(results_list) == 0) {
    cat("No valid models were generated\n")
    return(data.frame())
  }
  
  # Convert list of lists to dataframe
  results_df <- do.call(rbind.data.frame, lapply(results_list, function(x) {
    if (is.null(x)) return(NULL)
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  
  # Add adjusted p-values
  cat("Adjusting p-values for multiple comparisons...\n")
  if (nrow(results_df) > 0) {
    results_df <- adjust_p_values(results_df)
  }
  
  return(results_df)
}

# Function to check and report data structure
examine_data <- function(data) {
  cat("\nData Structure Summary:\n")
  cat("Dimensions:", dim(data), "\n")
  cat("Regions:", paste(unique(data$region), collapse=", "), "\n")
  cat("Years range:", range(data$PY, na.rm=TRUE), "\n")
  cat("Number of complete cases:", sum(complete.cases(data)), "out of", nrow(data), "\n\n")
  
  # Check for variables with too many missing values
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  problem_vars <- names(na_counts[na_counts > nrow(data)*0.5])
  
  if (length(problem_vars) > 0) {
    cat("Warning: The following variables have >50% missing values:\n")
    for (var in problem_vars) {
      cat("- ", var, ": ", na_counts[var], " missing values (", 
          round(na_counts[var]/nrow(data)*100, 1), "%)\n", sep="")
    }
    cat("\n")
  }
  
  # Check for constant variables
  constant_vars <- sapply(data, function(x) length(unique(x[!is.na(x)])) <= 1)
  if (any(constant_vars)) {
    cat("Warning: The following variables are constant (no variation):\n")
    cat(paste(names(constant_vars)[constant_vars], collapse=", "), "\n\n")
  }
}

# Main execution code
cat("Starting regional regression analysis...\n")

# Examine data structure before analysis
examine_data(region_data_filtered)

# Run the analysis
results <- run_analysis(region_data_filtered, dependent_vars, independent_vars)




###------------------------------------------
###------------------------------------------
# Add the interpretaions
###------------------------------------------
###------------------------------------------


# Script to transform coefficients and add effect size columns
# This script assumes 'results' dataframe is already in the environment

# Function to determine effect unit based on model type
get_effect_unit <- function(model_type) {
  switch(model_type,
         "gaussian" = "beta",
         "beta" = "OR",
         "beta_transformed" = "OR",
         "gaussian_log" = "% change",
         "negative_binomial" = "IRR",
         "poisson" = "IRR",
         "binomial" = "OR",
         "NA")  # Default case
}

# Create a new dataframe with transformed coefficients
results <- results %>%
  mutate(
    # Determine effect unit based on model type
    effect_unit = sapply(model_type, get_effect_unit),
    
    # Calculate transformed effect size and confidence intervals
    effect_size = case_when(
      effect_unit == "beta" ~ coefficient,
      effect_unit == "OR" ~ exp(coefficient),
      effect_unit == "IRR" ~ exp(coefficient),
      effect_unit == "% change" ~ (exp(coefficient) - 1) * 100,
      TRUE ~ NA_real_
    ),
    
    # Transform confidence intervals
    effect_ci_lower = case_when(
      effect_unit == "beta" ~ lower_ci,
      effect_unit == "OR" ~ exp(lower_ci),
      effect_unit == "IRR" ~ exp(lower_ci),
      effect_unit == "% change" ~ (exp(lower_ci) - 1) * 100,
      TRUE ~ NA_real_
    ),
    
    effect_ci_upper = case_when(
      effect_unit == "beta" ~ upper_ci,
      effect_unit == "OR" ~ exp(upper_ci),
      effect_unit == "IRR" ~ exp(upper_ci),
      effect_unit == "% change" ~ (exp(upper_ci) - 1) * 100,
      TRUE ~ NA_real_
    ),
    
    # Create a formatted effect size with CI for reporting
    effect_with_ci = case_when(
      effect_unit == "beta" ~ sprintf("β=%.3f (95%% CI: %.3f - %.3f)", 
                                      effect_size, effect_ci_lower, effect_ci_upper),
      effect_unit == "OR" ~ sprintf("OR: %.3f (95%% CI: %.3f - %.3f)", 
                                    effect_size, effect_ci_lower, effect_ci_upper),
      effect_unit == "IRR" ~ sprintf("IRR: %.3f (95%% CI: %.3f - %.3f)", 
                                     effect_size, effect_ci_lower, effect_ci_upper),
      effect_unit == "% change" ~ sprintf("%.1f%% (95%% CI: %.1f%% - %.1f%%)", 
                                          effect_size, effect_ci_lower, effect_ci_upper),
      TRUE ~ NA_character_
    )
  )




################################################################################
#### 4. Hierarchical models ####
################################################################################

# Load required libraries
library(lme4)          # For linear mixed effects models
library(glmmTMB)       # For negative binomial and other complex mixed models
library(betareg)       # For beta regression
library(performance)   # For model performance metrics
library(car)           # For additional statistical tests
library(MASS)          # For fitdistr and other statistical functions
library(tidyverse)     # For data manipulation and visualization
library(broom.mixed)   # For tidying mixed model output
library(DHARMa)        # For residual diagnostics of GLMMs
library(pscl)          # For pseudo R-squared calculations

# Create a template for result data frames to ensure consistent structure
create_result_template <- function() {
  data.frame(
    dependent_variable = character(0),
    independent_variable = character(0),
    distribution = character(0),
    coefficient = numeric(0),
    std_error = numeric(0),
    statistic = numeric(0),
    p_value = numeric(0),
    adj_p_value = numeric(0),
    conf_low = numeric(0),
    conf_high = numeric(0),
    r_squared = numeric(0),
    adj_r_squared = numeric(0),
    aic = numeric(0),
    bic = numeric(0),
    n_observations = integer(0),
    converged = logical(0),
    error = character(0),
    stringsAsFactors = FALSE
  )
}

#' Function to determine the appropriate distribution for a variable
#' @param data A vector of data
#' @return A character string indicating the suggested distribution
determine_distribution <- function(data) {
  # Remove NA values
  data <- data[!is.na(data)]
  
  # Check if data is empty or has insufficient values
  if (length(data) < 5) {
    return("insufficient_data")
  }
  
  # Check if data is count-like (all integers)
  is_count <- all(data == round(data)) && all(data >= 0)
  
  # Check if data is proportion-like (between 0 and 1)
  is_proportion <- all(data >= 0 & data <= 1) && any(data > 0 & data < 1)
  
  if (is_count) {
    # Calculate variance to mean ratio for count data
    var_mean_ratio <- var(data) / mean(data)
    
    if (var_mean_ratio >= 0.7 && var_mean_ratio <= 1.5) {
      return("poisson")
    } else if (var_mean_ratio > 1.5) {
      return("negative_binomial")
    }
  }
  
  if (is_proportion) {
    return("beta")
  }
  
  # Test for normality using Shapiro-Wilk test (only if not too many data points)
  if (length(data) <= 5000) {
    shapiro_test <- try(shapiro.test(data), silent = TRUE)
    if (!inherits(shapiro_test, "try-error") && shapiro_test$p.value > 0.05) {
      return("gaussian")
    }
    
    # If log-transform improves normality, suggest log-normal
    if (all(data > 0)) {
      log_data <- try(log(data), silent = TRUE)
      if (!inherits(log_data, "try-error") && !any(is.infinite(log_data)) && !any(is.na(log_data))) {
        shapiro_test_log <- try(shapiro.test(log_data), silent = TRUE)
        if (!inherits(shapiro_test_log, "try-error") && 
            shapiro_test_log$p.value > shapiro_test$p.value) {
          return("log_normal")
        }
      }
    }
  } else {
    # For large datasets, use visual inspection or skewness/kurtosis
    if (all(data > 0) && (sd(data)/mean(data) > 1)) {
      return("log_normal")
    }
  }
  
  # Default to gaussian
  return("gaussian")
}

#' Function to fit the appropriate hierarchical model
#' @param df The data frame
#' @param dep_var The dependent variable name
#' @param indep_var The independent variable name
#' @return A list containing the model and related metrics
fit_hierarchical_model <- function(df, dep_var, indep_var) {
  # Ensure we have sufficient data
  valid_data <- df[!is.na(df[[dep_var]]) & !is.na(df[[indep_var]]), ]
  
  if (nrow(valid_data) < 20) {
    cat(paste0("Warning: Insufficient data for ", dep_var, " ~ ", indep_var, 
               " (n=", nrow(valid_data), ")\n"))
    return(list(
      model = NULL,
      error = paste0("Insufficient data (n=", nrow(valid_data), ", minimum required: 20)"),
      distribution = "insufficient_data",
      converged = FALSE
    ))
  }
  
  # Determine appropriate distribution
  distribution <- determine_distribution(valid_data[[dep_var]])
  cat(paste0("Using '", distribution, "' distribution for ", dep_var, "\n"))
  
  # Prepare formula - always include year as a covariate
  formula <- as.formula(paste(dep_var, "~", indep_var, "+ PY + (1|region)"))
  
  # Fit model based on determined distribution
  tryCatch({
    if (distribution == "poisson") {
      model <- glmmTMB(formula, data = valid_data, family = poisson())
    } else if (distribution == "negative_binomial") {
      model <- glmmTMB(formula, data = valid_data, family = nbinom2())
    } else if (distribution == "beta") {
      # Beta regression requires values strictly between 0 and 1
      transformed_data <- valid_data
      # Transform values that are exactly 0 or 1
      epsilon <- 0.001
      transformed_data[[dep_var]] <- ifelse(transformed_data[[dep_var]] == 0, 
                                            epsilon, 
                                            ifelse(transformed_data[[dep_var]] == 1, 
                                                   1 - epsilon, 
                                                   transformed_data[[dep_var]]))
      # Beta regression with glmmTMB
      model <- glmmTMB(formula, data = transformed_data, family = beta_family())
    } else if (distribution == "log_normal") {
      # Log transform the dependent variable
      transformed_data <- valid_data
      transformed_data[[dep_var]] <- log(transformed_data[[dep_var]])
      model <- lmer(formula, data = transformed_data)
    } else {
      # Default to gaussian
      model <- lmer(formula, data = valid_data)
    }
    
    # Model diagnostics
    converged <- TRUE  # Default assumption
    
    # Check for convergence issues in lmer models
    if (inherits(model, "lmerMod")) {
      converged <- !is.null(model@optinfo$conv$lme4$code) && model@optinfo$conv$lme4$code == 0
    }
    
    # For glmmTMB models
    if (inherits(model, "glmmTMB")) {
      converged <- !is.null(model$sdr) && !any(is.na(model$sdr$cov.fixed))
    }
    
    if (!converged) {
      cat(paste0("Warning: Mixed model did not converge for ", dep_var, " ~ ", indep_var, 
                 ". Trying simpler model...\n"))
      
      # If model didn't converge, try a simpler model without random effects
      simple_formula <- as.formula(paste(dep_var, "~", indep_var, "+ PY"))
      
      if (distribution == "poisson") {
        model <- glm(simple_formula, data = valid_data, family = poisson())
      } else if (distribution == "negative_binomial") {
        model <- MASS::glm.nb(simple_formula, data = valid_data)
      } else if (distribution == "beta") {
        transformed_data <- valid_data
        epsilon <- 0.001
        transformed_data[[dep_var]] <- ifelse(transformed_data[[dep_var]] == 0, 
                                              epsilon, 
                                              ifelse(transformed_data[[dep_var]] == 1, 
                                                     1 - epsilon, 
                                                     transformed_data[[dep_var]]))
        model <- betareg(simple_formula, data = transformed_data)
      } else if (distribution == "log_normal") {
        transformed_data <- valid_data
        transformed_data[[dep_var]] <- log(transformed_data[[dep_var]])
        model <- lm(simple_formula, data = transformed_data)
      } else {
        model <- lm(simple_formula, data = valid_data)
      }
    }
    
    return(list(
      model = model,
      distribution = distribution,
      error = NULL,
      converged = converged
    ))
  }, error = function(e) {
    cat(paste0("Error fitting model for ", dep_var, " ~ ", indep_var, ": ", e$message, "\n"))
    return(list(
      model = NULL,
      distribution = distribution,
      error = paste("Error fitting model:", e$message),
      converged = FALSE
    ))
  })
}

#' Function to extract model statistics
#' @param model_result The result from fit_hierarchical_model
#' @param dep_var The dependent variable name
#' @param indep_var The independent variable name
#' @return A data frame with model statistics
extract_model_stats <- function(model_result, dep_var, indep_var) {
  # Start with template data frame to ensure consistent structure
  result_df <- data.frame(
    dependent_variable = dep_var,
    independent_variable = indep_var,
    distribution = ifelse(!is.null(model_result$distribution), model_result$distribution, "unknown"),
    coefficient = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    adj_p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    aic = NA_real_,
    bic = NA_real_,
    n_observations = NA_integer_,
    converged = FALSE,
    error = ifelse(!is.null(model_result$error), model_result$error, NA_character_),
    stringsAsFactors = FALSE
  )
  
  # Check if model fitting produced an error
  if (is.null(model_result$model) || !is.null(model_result$error)) {
    return(result_df)
  }
  
  model <- model_result$model
  distribution <- model_result$distribution
  
  # Extract model statistics - wrapped in tryCatch to handle any errors
  tryCatch({
    # Extract model summary
    if (inherits(model, "lmerMod") || inherits(model, "glmerMod") || inherits(model, "glmmTMB")) {
      # For mixed models
      summary_obj <- summary(model)
      
      # For glmmTMB models, the coefficient table structure is different
      if (inherits(model, "glmmTMB")) {
        coef_table <- summary_obj$coefficients$cond
      } else {
        coef_table <- summary_obj$coefficients
      }
      
      # Find the row corresponding to the independent variable
      # First check for exact match
      indep_var_row <- which(rownames(coef_table) == indep_var)
      # If not found, try more flexible matching
      if (length(indep_var_row) == 0) {
        indep_var_row <- grep(paste0("^", indep_var, "$"), rownames(coef_table))
      }
      
      if (length(indep_var_row) > 0) {
        result_df$coefficient <- coef_table[indep_var_row[1], "Estimate"]
        result_df$std_error <- coef_table[indep_var_row[1], "Std. Error"]
        
        # For lmer models, t-value is reported; for glmer models, z-value
        if ("t value" %in% colnames(coef_table)) {
          result_df$statistic <- coef_table[indep_var_row[1], "t value"]
          # For t-values, calculate p-value using degrees of freedom
          df_residual <- try(df.residual(model), silent = TRUE)
          if (inherits(df_residual, "try-error") || is.null(df_residual)) {
            df_residual <- nrow(model.frame(model)) - length(fixef(model))
          }
          result_df$p_value <- 2 * pt(abs(result_df$statistic), df = df_residual, lower.tail = FALSE)
        } else {
          result_df$statistic <- coef_table[indep_var_row[1], "z value"]
          result_df$p_value <- coef_table[indep_var_row[1], "Pr(>|z|)"]
        }
      }
      
      # Calculate confidence intervals safely
      ci <- try({
        # First try Wald method
        confint(model, parm = indep_var, method = "Wald")
      }, silent = TRUE)
      
      if (inherits(ci, "try-error") || is.null(ci) || length(ci) < 1) {
        # If Wald fails, try profile method with limited iterations
        ci <- try(confint(model, parm = indep_var, method = "profile", maxpts = 100), silent = TRUE)
      }
      
      if (!inherits(ci, "try-error") && !is.null(ci) && length(ci) >= 2) {
        result_df$conf_low <- ci[1]
        result_df$conf_high <- ci[2]
      }
      
      # Calculate R-squared for mixed models safely
      r2 <- try(performance::r2(model), silent = TRUE)
      if (!inherits(r2, "try-error") && !is.null(r2)) {
        # Check what type of R2 values are available
        if (!is.null(r2$R2_conditional)) {
          result_df$r_squared <- r2$R2_conditional  # Total variance explained
        }
        if (!is.null(r2$R2_marginal)) {
          result_df$adj_r_squared <- r2$R2_marginal  # Variance explained by fixed effects only
        }
      }
      
      # AIC and BIC
      result_df$aic <- try(AIC(model), silent = TRUE)
      if (inherits(result_df$aic, "try-error")) result_df$aic <- NA_real_
      
      result_df$bic <- try(BIC(model), silent = TRUE)
      if (inherits(result_df$bic, "try-error")) result_df$bic <- NA_real_
      
      # Number of observations
      result_df$n_observations <- try(nrow(model.frame(model)), silent = TRUE)
      if (inherits(result_df$n_observations, "try-error")) result_df$n_observations <- NA_integer_
      
    } else {
      # For standard regression models
      summary_obj <- summary(model)
      
      # Find the coefficient for the independent variable
      coef_table <- summary_obj$coefficients
      
      # First check for exact match
      indep_var_row <- which(rownames(coef_table) == indep_var)
      # If not found, try more flexible matching
      if (length(indep_var_row) == 0) {
        indep_var_row <- grep(paste0("^", indep_var, "$"), rownames(coef_table))
      }
      
      if (length(indep_var_row) > 0) {
        result_df$coefficient <- coef_table[indep_var_row[1], "Estimate"]
        result_df$std_error <- coef_table[indep_var_row[1], "Std. Error"]
        
        # For lm, t-value is reported; for glm, z or t depending on the family
        if ("t value" %in% colnames(coef_table)) {
          result_df$statistic <- coef_table[indep_var_row[1], "t value"]
          result_df$p_value <- coef_table[indep_var_row[1], "Pr(>|t|)"]
        } else {
          result_df$statistic <- coef_table[indep_var_row[1], "z value"]
          result_df$p_value <- coef_table[indep_var_row[1], "Pr(>|z|)"]
        }
      }
      
      # Calculate confidence intervals
      ci <- try(confint(model, parm = indep_var), silent = TRUE)
      if (!inherits(ci, "try-error") && !is.null(ci) && length(ci) >= 2) {
        result_df$conf_low <- ci[1]
        result_df$conf_high <- ci[2]
      }
      
      # R-squared and adjusted R-squared
      if (inherits(model, "lm")) {
        result_df$r_squared <- summary_obj$r.squared
        result_df$adj_r_squared <- summary_obj$adj.r.squared
      } else if (inherits(model, "glm") || inherits(model, "negbin")) {
        # Pseudo R-squared for GLMs
        null_deviance <- model$null.deviance
        residual_deviance <- model$deviance
        result_df$r_squared <- 1 - (residual_deviance / null_deviance)
      } else if (inherits(model, "betareg")) {
        result_df$r_squared <- summary_obj$pseudo.r.squared
      }
      
      # AIC and BIC
      result_df$aic <- try(AIC(model), silent = TRUE)
      if (inherits(result_df$aic, "try-error")) result_df$aic <- NA_real_
      
      result_df$bic <- try(BIC(model), silent = TRUE)
      if (inherits(result_df$bic, "try-error")) result_df$bic <- NA_real_
      
      # Number of observations
      result_df$n_observations <- try(nrow(model.frame(model)), silent = TRUE)
      if (inherits(result_df$n_observations, "try-error")) result_df$n_observations <- NA_integer_
    }
    
    # Mark as converged
    result_df$converged <- model_result$converged
    
    return(result_df)
  }, error = function(e) {
    # If any error occurs during extraction, return the base data frame with error message
    cat(paste0("Error extracting statistics for ", dep_var, " ~ ", indep_var, ": ", e$message, "\n"))
    result_df$error <- paste("Error extracting statistics:", e$message)
    return(result_df)
  })
}

# Main analysis function
run_hierarchical_models <- function(region_data_filtered, dependent_vars, independent_vars) {
  # Initialize results template
  template <- create_result_template()
  
  # Store all model results
  all_results <- list()
  result_count <- 0
  
  # 1. Models where indicators are dependent variables and publication count is independent
  cat("Running models with indicators as dependent variables...\n")
  for (dep_var in dependent_vars) {
    cat(paste0("Analyzing ", dep_var, " (", which(dependent_vars == dep_var), 
               " of ", length(dependent_vars), ")\n"))
    
    # Fit model with publication count as independent variable
    indep_var <- "publication_count"
    model_result <- fit_hierarchical_model(region_data_filtered, dep_var, indep_var)
    
    # Extract statistics
    result_count <- result_count + 1
    all_results[[result_count]] <- extract_model_stats(model_result, dep_var, indep_var)
    
    # Verify structure consistency
    if (!identical(names(all_results[[result_count]]), names(template))) {
      cat("Warning: Inconsistent column names detected in result. Fixing...\n")
      # Ensure consistent structure
      missing_cols <- setdiff(names(template), names(all_results[[result_count]]))
      for (col in missing_cols) {
        all_results[[result_count]][[col]] <- NA
      }
      all_results[[result_count]] <- all_results[[result_count]][, names(template)]
    }
  }
  
  # 2. Models where publication count is the dependent variable and indicators are independent
  cat("Running models with publication count as dependent variable...\n")
  for (indep_var in independent_vars) {
    cat(paste0("Analyzing ", indep_var, " (", which(independent_vars == indep_var), 
               " of ", length(independent_vars), ")\n"))
    
    # Fit model with publication count as dependent variable
    dep_var <- "publication_count"
    model_result <- fit_hierarchical_model(region_data_filtered, dep_var, indep_var)
    
    # Extract statistics
    result_count <- result_count + 1
    all_results[[result_count]] <- extract_model_stats(model_result, dep_var, indep_var)
    
    # Verify structure consistency
    if (!identical(names(all_results[[result_count]]), names(template))) {
      cat("Warning: Inconsistent column names detected in result. Fixing...\n")
      # Ensure consistent structure
      missing_cols <- setdiff(names(template), names(all_results[[result_count]]))
      for (col in missing_cols) {
        all_results[[result_count]][[col]] <- NA
      }
      all_results[[result_count]] <- all_results[[result_count]][, names(template)]
    }
  }
  
  # Combine all results using rbind.fill from plyr for safety
  cat("Combining all model results...\n")
  all_stats <- do.call(dplyr::bind_rows, all_results)
  
  if (nrow(all_stats) == 0) {
    stop("No valid model results to combine!")
  }
  
  # Apply Benjamini-Hochberg correction for multiple testing
  valid_p <- !is.na(all_stats$p_value)
  if (sum(valid_p) > 0) {
    all_stats$adj_p_value[valid_p] <- p.adjust(all_stats$p_value[valid_p], method = "BH")
  }
  
  cat("Analysis complete. Generated results for", nrow(all_stats), "models.\n")
  return(all_stats)
}

# Set a seed for reproducibility
set.seed(123)

# Run the analysis with error handling
cat("Starting hierarchical modeling analysis...\n")
results <- tryCatch({
  run_hierarchical_models(region_data_filtered, dependent_vars, independent_vars)
}, error = function(e) {
  cat("ERROR: Analysis failed with message:", e$message, "\n")
  cat("Attempting to save any partial results...\n")
  return(NULL)
})


### ------------------------------------------
### Add some interpretable statistics
### ------------------------------------------

# Function to determine effect unit and transform coefficients based on model type
transform_coefficients <- function(results) {
  # Create copies of the original data
  results$effect_size <- results$coefficient
  results$effect_ci_lower <- results$conf_low
  results$effect_ci_upper <- results$conf_high
  results$effect_unit <- NA_character_
  
  # Transform based on distribution type
  # 1. Gaussian models (linear regression) - no transformation needed
  gaussian_idx <- results$distribution == "gaussian"
  results$effect_unit[gaussian_idx] <- "β"
  
  # 2. Beta regression - exponentiate to get odds ratios
  beta_idx <- results$distribution == "beta"
  results$effect_size[beta_idx] <- exp(results$coefficient[beta_idx])
  results$effect_ci_lower[beta_idx] <- exp(results$conf_low[beta_idx])
  results$effect_ci_upper[beta_idx] <- exp(results$conf_high[beta_idx])
  results$effect_unit[beta_idx] <- "OR"
  
  # 3. Log-normal regression - calculate percentage change
  lognormal_idx <- results$distribution == "log_normal"
  results$effect_size[lognormal_idx] <- (exp(results$coefficient[lognormal_idx]) - 1) * 100
  results$effect_ci_lower[lognormal_idx] <- (exp(results$conf_low[lognormal_idx]) - 1) * 100
  results$effect_ci_upper[lognormal_idx] <- (exp(results$conf_high[lognormal_idx]) - 1) * 100
  results$effect_unit[lognormal_idx] <- "% change"
  
  # 4. Negative binomial - exponentiate for incidence rate ratios
  negbin_idx <- results$distribution == "negative_binomial"
  results$effect_size[negbin_idx] <- exp(results$coefficient[negbin_idx])
  results$effect_ci_lower[negbin_idx] <- exp(results$conf_low[negbin_idx])
  results$effect_ci_upper[negbin_idx] <- exp(results$conf_high[negbin_idx])
  results$effect_unit[negbin_idx] <- "IRR"
  
  # 5. Handle cases with missing confidence intervals
  na_conf <- is.na(results$conf_low) | is.na(results$conf_high)
  if (any(na_conf)) {
    # For cases where standard error is available but CI is missing
    # Calculate approximate CI using the coefficient and standard error
    se_available <- !is.na(results$std_error) & na_conf
    
    if (any(se_available)) {
      # For Gaussian models (using 1.96 for ~95% CI)
      gaussian_se <- se_available & results$distribution == "gaussian"
      if (any(gaussian_se)) {
        results$effect_ci_lower[gaussian_se] <- results$coefficient[gaussian_se] - 1.96 * results$std_error[gaussian_se]
        results$effect_ci_upper[gaussian_se] <- results$coefficient[gaussian_se] + 1.96 * results$std_error[gaussian_se]
      }
      
      # For beta and negative binomial - calculate on log scale then exponentiate
      exp_models <- se_available & (results$distribution %in% c("beta", "negative_binomial"))
      if (any(exp_models)) {
        log_lower <- results$coefficient[exp_models] - 1.96 * results$std_error[exp_models]
        log_upper <- results$coefficient[exp_models] + 1.96 * results$std_error[exp_models]
        
        results$effect_ci_lower[exp_models] <- exp(log_lower)
        results$effect_ci_upper[exp_models] <- exp(log_upper)
      }
      
      # For log-normal - calculate percentage change
      log_normal_se <- se_available & results$distribution == "log_normal"
      if (any(log_normal_se)) {
        log_lower <- results$coefficient[log_normal_se] - 1.96 * results$std_error[log_normal_se]
        log_upper <- results$coefficient[log_normal_se] + 1.96 * results$std_error[log_normal_se]
        
        results$effect_ci_lower[log_normal_se] <- (exp(log_lower) - 1) * 100
        results$effect_ci_upper[log_normal_se] <- (exp(log_upper) - 1) * 100
      }
    }
  }
  
  # 6. Create formatted effect size with CI for reporting
  results$effect_with_ci <- NA_character_
  
  # Format based on effect unit
  for (i in 1:nrow(results)) {
    if (is.na(results$effect_size[i]) || is.na(results$effect_ci_lower[i]) || is.na(results$effect_ci_upper[i])) {
      results$effect_with_ci[i] <- "Not available"
      next
    }
    
    if (results$effect_unit[i] == "β") {
      results$effect_with_ci[i] <- sprintf("β=%.3f (95%% CI: %.3f to %.3f)", 
                                           results$effect_size[i], 
                                           results$effect_ci_lower[i], 
                                           results$effect_ci_upper[i])
    } else if (results$effect_unit[i] == "OR") {
      results$effect_with_ci[i] <- sprintf("OR: %.3f (95%% CI: %.3f to %.3f)", 
                                           results$effect_size[i], 
                                           results$effect_ci_lower[i], 
                                           results$effect_ci_upper[i])
    } else if (results$effect_unit[i] == "IRR") {
      results$effect_with_ci[i] <- sprintf("IRR: %.3f (95%% CI: %.3f to %.3f)", 
                                           results$effect_size[i], 
                                           results$effect_ci_lower[i], 
                                           results$effect_ci_upper[i])
    } else if (results$effect_unit[i] == "% change") {
      results$effect_with_ci[i] <- sprintf("%.1f%% (95%% CI: %.1f%% to %.1f%%)", 
                                           results$effect_size[i], 
                                           results$effect_ci_lower[i], 
                                           results$effect_ci_upper[i])
    }
  }
  
  return(results)
}

# Apply transformations
results <- transform_coefficients(results)



# Export results as an excel file
write.xlsx(results, file = "~/Desktop/RegionNTDs001/manuscript/Supplementary Material 3.xlsx", rowNames = FALSE)



