# Setting up ----

## packages ----
library(here)
library(tidyverse)
library(data.table)
library(knitr)
library(kableExtra)
library(patchwork)
library(parallel)
library(foreach)
library(doParallel)
library(xtable)
library(fixest)

## setting path ----
if(Sys.getenv("R_PLATFORM")=="aarch64-apple-darwin20"){
  setwd(here::here("PS2"))
  output_path <- here::here("PS2", "output"); if(!dir.exists(output_path)) dir.create(output_path)
} else{
  setwd("~/MGTECON603/PS2") 
  output_path <- "output"; if(!dir.exists(output_path)) dir.create(output_path)
}

## setting seed ----
set.seed(123)

## Parallelization - Use more cores ----
n_cores <- if(Sys.getenv("R_PLATFORM")=="aarch64-apple-darwin20"){
  parallel::detectCores() - 1
} else{
  min(32, parallel::detectCores()) # Use all available cores up to 32
}

cl <- makeCluster(n_cores)
registerDoParallel(cl)

## data ----
data <- data.table::fread(file.path("riverside_2025.txt"))
names(data) <- c("W", "earnings1yr", "earnings4yr", "hs_diploma", "female", "age", "child", "single")

# Pre-compute constants
n <- nrow(data)
M <- sum(data$W) # More efficient than nrow(data[W==1])


## Functions ----
### Optimized Function for Data Summary ----
display_summary_stats <- function(data) {
  # Vectorized computation of summary statistics
  compute_stats <- function(x) {
    x_clean <- x[!is.na(x)]
    zeros <- x_clean == 0
    
    c(numNA = sum(is.na(x)),
      numZeros = sum(zeros),
      fracZeros = mean(zeros),
      mean = mean(x_clean),
      sd = sd(x_clean),
      min = min(x_clean),
      max = max(x_clean),
      quantile(x_clean, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.9, 0.95, 0.99, 0.999)))
  }
  
  # Use lapply for better performance
  stats_list <- lapply(data, compute_stats)
  
  # Convert to data.table more efficiently
  result <- rbindlist(lapply(names(stats_list), function(var) {
    stats <- stats_list[[var]]
    dt <- as.data.table(t(stats))
    dt[, variable := var]
    setcolorder(dt, c("variable", setdiff(names(dt), "variable")))
    dt
  }))
  
  return(result)
}

# ### Optimized test statistic functions ----
# compute_test_stat_mean <- function(dt, outcome_col, treatment_col) {
#   # More efficient aggregation
#   treated_mean <- dt[get(treatment_col) == 1, mean(get(outcome_col))]
#   control_mean <- dt[get(treatment_col) == 0, mean(get(outcome_col))]
#   return(treated_mean - control_mean)
# }

# compute_test_stat_median <- function(dt, outcome_col, treatment_col) {
#   treated_median <- dt[get(treatment_col) == 1, median(get(outcome_col))]
#   control_median <- dt[get(treatment_col) == 0, median(get(outcome_col))]
#   return(treated_median - control_median)
# }

# # Pre-compute demeaned covariates once
# covariates <- c("hs_diploma", "female", "age", "single")
# for (cov in covariates) {
#   data[, paste0(cov, "_dm") := get(cov) - mean(get(cov))]
# }

# compute_test_stat_hurdle <- function(dt, outcome_col, treatment_col, covariates) {
#   # Use pre-computed demeaned variables
#   cov_dm <- paste0(covariates, "_dm")
  
#   # More efficient formula construction
#   interaction_terms <- paste(paste0(cov_dm, ":", treatment_col), collapse = " + ")
#   covariate_terms <- paste(covariates, collapse = " + ")
  
#   formula_zero <- as.formula(paste0("I(", outcome_col, " > 0) ~ ", interaction_terms, " + ", treatment_col, " + ", covariate_terms))
#   formula_positive <- as.formula(paste0("log(", outcome_col, ") ~ ", interaction_terms, " + ", treatment_col, " + ", covariate_terms))
  
#   # Fit models with faster options
#   model_zero <- tryCatch({
#     feglm(formula_zero, data = dt, family = binomial("logit"), se = "hetero")
#   }, error = function(e) return(list(coefficients = setNames(0, treatment_col))))
  
#   model_positive <- tryCatch({
#     feols(formula_positive, data = dt[get(outcome_col) > 0], se = "hetero")
#   }, error = function(e) return(list(coefficients = setNames(0, treatment_col))))
  
#   return(model_zero$coefficients[[treatment_col]] + model_positive$coefficients[[treatment_col]])
# }

# Generate output functions
create_table <- function(data, filename) {
  tab <- display_summary_stats(data) |> 
    kable(digits = 4, format = "latex", booktabs = TRUE, format.args = list(decimal.mark = ".", big.mark = ","))
  writeLines(tab, file.path(output_path, filename))
  return(tab)
}


### Sample Variance Function ----
sample_variance <- function(x) {
  # Remove missing values
  x <- x[!is.na(x)]
  n <- length(x)
  
  # Handle edge case of empty vector or single observation
  if (n <= 1) return(NA_real_)
  
  # Compute sample mean
  mean_val <- sum(x) / n
  
  # Compute sample variance using the formula: s² = Σ(xᵢ - x̄)² / (n-1)
  variance <- sum((x - mean_val)^2) / (n - 1)
  
  return(variance)
}





# Summary Statistics ----
cat("Computing summary statistics...\n")
create_table(data, "summary_stats.tex")
create_table(data[W==1], "summary_stats(treated_group).tex")
create_table(data[W==0], "summary_stats(control_group).tex")

## Create histogram
ggplot(data, aes(x=earnings1yr, fill=factor(W))) + 
  geom_histogram(position="identity", alpha=0.3, bins=30) +
  scale_fill_manual(values=c("blue", "orange"), labels=c("Control", "Treatment")) +
  labs(title="Histogram of earnings1yr by Treatment Arm", x="earnings1yr", y="Count", fill="Group") +
  theme_minimal()
ggsave(file.path(output_path, "histogram_earnings1yr_by_treatment_arm.png"), width=8, height=6)


## Create histogram
ggplot(data, aes(x=earnings4yr, fill=factor(W))) + 
  geom_histogram(position="identity", alpha=0.3, bins=30) +
  scale_fill_manual(values=c("blue", "orange"), labels=c("Control", "Treatment")) +
  labs(title="Histogram of earnings4yr by Treatment Arm", x="earnings4yr", y="Count", fill="Group") +
  theme_minimal()
ggsave(file.path(output_path, "histogram_earnings4yr_by_treatment_arm.png"), width=8, height=6)


# Problem 1 ----

data_treated <- data[W==1]
data_control <- data[W==0]

## (a) ----

### earnings1yr ----
dim_1yr <- data_treated[, .(mean = mean(earnings1yr))] - data_control[, .(mean = mean(earnings1yr))]
dim_1yr

neyman_var_dim_1yr <- data_treated[, .(var = sample_variance(earnings1yr))]/nrow(data_treated) + data_control[, .(var = sample_variance(earnings1yr))]/(nrow(data_control))
neyman_var_dim_1yr

# Assuming homoskedasticity, we can say S_0^2 = S_1^2
data_treated[, dev_sq := (earnings1yr - mean(earnings1yr))^2]
data_control[, dev_sq := (earnings1yr - mean(earnings1yr))^2]

homoskedastic_var_dim_1yr <- (sum(data_treated[, dev_sq]) + sum(data_control[, dev_sq]))/(nrow(data) - 2)
homoskedastic_var_dim_1yr <- homoskedastic_var_dim_1yr * (1/nrow(data_treated) + 1/nrow(data_control))

# Confidence Intervals by Normal Approximation
z_score <- qnorm(0.95)
ci_dim_1yr <- as.numeric(dim_1yr) + c(-1, 1) * z_score * sqrt(as.numeric(neyman_var_dim_1yr))
ci_dim_1yr

ci_dim_1yr_homoskedastic <- as.numeric(dim_1yr) + c(-1, 1) * z_score * sqrt(as.numeric(homoskedastic_var_dim_1yr))
ci_dim_1yr_homoskedastic

### earnings4yr ----
dim_4yr <- data_treated[, .(mean = mean(earnings4yr))] - data_control[, .(mean = mean(earnings4yr))]
dim_4yr

neyman_var_dim_4yr <- data_treated[, .(var = sample_variance(earnings4yr))]/nrow(data_treated) + data_control[, .(var = sample_variance(earnings4yr))]/(nrow(data_control))
neyman_var_dim_4yr

# Assuming homoskedasticity, we can say S_0^2 = S_1^2
data_treated[, dev_sq := (earnings4yr - mean(earnings4yr))^2]
data_control[, dev_sq := (earnings4yr - mean(earnings4yr))^2]

homoskedastic_var_dim_4yr <- (sum(data_treated[, dev_sq]) + sum(data_control[, dev_sq]))/(nrow(data) - 2)
homoskedastic_var_dim_4yr <- homoskedastic_var_dim_4yr * (1/nrow(data_treated) + 1/nrow(data_control))

# Confidence Intervals by Normal Approximation
ci_dim_4yr <- as.numeric(dim_4yr) + c(-1, 1) * z_score * sqrt(as.numeric(neyman_var_dim_4yr))
ci_dim_4yr

ci_dim_4yr_homoskedastic <- as.numeric(dim_4yr) + c(-1, 1) * z_score * sqrt(as.numeric(homoskedastic_var_dim_4yr))
ci_dim_4yr_homoskedastic



# Create a summary table for ATE estimation results
ate_results <- data.table(
  Statistic = c(
    "DIM estimate",
    "Neyman variance",
    "Homoskedastic variance",
    "Confidence interval lower bound",
    "Confidence interval upper bound",
    "Confidence interval lower bound (homoskedastic)",
    "Confidence interval upper bound (homoskedastic)"
  ),
  `earnings 1yr` = c(
    as.numeric(dim_1yr),
    as.numeric(neyman_var_dim_1yr),
    as.numeric(homoskedastic_var_dim_1yr),
    ci_dim_1yr[1],
    ci_dim_1yr[2],
    ci_dim_1yr_homoskedastic[1],
    ci_dim_1yr_homoskedastic[2]
  ),
  `earnings 4yr` = c(
    as.numeric(dim_4yr),
    as.numeric(neyman_var_dim_4yr),
    as.numeric(homoskedastic_var_dim_4yr),
    ci_dim_4yr[1],
    ci_dim_4yr[2],
    ci_dim_4yr_homoskedastic[1],
    ci_dim_4yr_homoskedastic[2]
  )
)

# Set row names for pretty printing (optional, as data.table doesn't use rownames)
# Print the table
print(ate_results)

ate_results_xtable <- xtable(ate_results, align = c("l", "l", "r", "r"))
print(
  ate_results_xtable,
  file = file.path(output_path, "ATE_results.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  tabular.environment = "tabular"
)

## (b) ----

dt_TT <- data[hs_diploma == 1 & child == 1]
dt_TF <- data[hs_diploma == 1 & child == 0]
dt_FT <- data[hs_diploma == 0 & child == 1]
dt_FF <- data[hs_diploma == 0 & child == 0]

nrow(data) == nrow(dt_TT) + nrow(dt_TF) + nrow(dt_FT) + nrow(dt_FF)

weights <- c(nrow(dt_TT), nrow(dt_TF), nrow(dt_FT), nrow(dt_FF))/ nrow(data)
weights
sum(weights) == 1

### earnings1yr ----
for(i in 1:4){
  dt <- get(paste0("dt_", c("TT", "TF", "FT", "FF")[i]))
  dt_treated <- dt[W==1]; dt_control <- dt[W==0]
  assign(paste0("dim_", c("TT", "TF", "FT", "FF")[i]), as.numeric(dt_treated[, .(mean = mean(earnings1yr))] - dt_control[, .(mean = mean(earnings1yr))]))
  assign(paste0("neyman_var_", c("TT", "TF", "FT", "FF")[i]), as.numeric(dt_treated[, .(var = sample_variance(earnings1yr))]/nrow(dt_treated) + dt_control[, .(var = sample_variance(earnings1yr))]/(nrow(dt_control))))
  assign(paste0("ci_", c("TT", "TF", "FT", "FF")[i]), as.numeric(get(paste0("dim_", c("TT", "TF", "FT", "FF")[i]))) + c(-1, 1) * z_score * sqrt(as.numeric(get(paste0("neyman_var_", c("TT", "TF", "FT", "FF")[i])))))
}

dim_TT
neyman_var_TT
ci_TT
ci_TF
ci_FT
ci_FF

stratified_results_1yr <- data.table(
  group = c("TT", "TF", "FT", "FF"),
  dim = c(dim_TT, dim_TF, dim_FT, dim_FF),
  neyman_var = c(neyman_var_TT, neyman_var_TF, neyman_var_FT, neyman_var_FF),
  lb = c(ci_TT[1], ci_TF[1], ci_FT[1], ci_FF[1]),
  ub = c(ci_TT[2], ci_TF[2], ci_FT[2], ci_FF[2])
)

stratified_results_1yr

ggplot(stratified_results_1yr, aes(x=group, y=dim, color=group)) + 
  geom_point() + 
  geom_hline(yintercept=0, color="black", linetype="dashed") + 
  geom_errorbar(aes(ymin=lb, ymax=ub)) + 
  labs(title="Stratified Results", x="Group", y="Difference in Mean Earnings(1yr)") + 
  theme_minimal()
ggsave(file.path(output_path, "stratified_results_1yr.png"), width=8, height=6)

names(stratified_results_1yr) <- c("Group", "DIM estimate", "Neyman variance", "Confidence interval lower bound", "Confidence interval upper bound")

stratified_results_xtable <- xtable(stratified_results_1yr, align = c("l", "l","r", "r", "r", "r"))
print(
  stratified_results_xtable,
  file = file.path(output_path, "stratified_results_1yr.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  tabular.environment = "tabular"
)

### earnings4yr ----
for(i in 1:4){
  dt <- get(paste0("dt_", c("TT", "TF", "FT", "FF")[i]))
  dt_treated <- dt[W==1]; dt_control <- dt[W==0]
  assign(paste0("dim_", c("TT", "TF", "FT", "FF")[i]), as.numeric(dt_treated[, .(mean = mean(earnings4yr))] - dt_control[, .(mean = mean(earnings4yr))]))
  assign(paste0("neyman_var_", c("TT", "TF", "FT", "FF")[i]), as.numeric(dt_treated[, .(var = sample_variance(earnings4yr))]/nrow(dt_treated) + dt_control[, .(var = sample_variance(earnings4yr))]/(nrow(dt_control))))
  assign(paste0("ci_", c("TT", "TF", "FT", "FF")[i]), as.numeric(get(paste0("dim_", c("TT", "TF", "FT", "FF")[i]))) + c(-1, 1) * z_score * sqrt(as.numeric(get(paste0("neyman_var_", c("TT", "TF", "FT", "FF")[i])))))
}

dim_TT
neyman_var_TT
ci_TT
ci_TF
ci_FT
ci_FF

stratified_results_4yr <- data.table(
  group = c("TT", "TF", "FT", "FF"),
  dim = c(dim_TT, dim_TF, dim_FT, dim_FF),
  neyman_var = c(neyman_var_TT, neyman_var_TF, neyman_var_FT, neyman_var_FF),
  lb = c(ci_TT[1], ci_TF[1], ci_FT[1], ci_FF[1]),
  ub = c(ci_TT[2], ci_TF[2], ci_FT[2], ci_FF[2])
)

stratified_results_4yr

ggplot(stratified_results_4yr, aes(x=group, y=dim, color=group)) + 
  geom_point() + 
  geom_hline(yintercept=0, color="black", linetype="dashed") + 
  geom_errorbar(aes(ymin=lb, ymax=ub)) + 
  labs(title="Stratified Results", x="Group", y="Difference in Mean Earnings(4yr)") + 
  theme_minimal()
ggsave(file.path(output_path, "stratified_results_4yr.png"), width=8, height=6)

names(stratified_results_4yr) <- c("Group", "DIM estimate", "Neyman variance", "Confidence interval lower bound", "Confidence interval upper bound")

stratified_results_xtable <- xtable(stratified_results_4yr, align = c("l", "l","r", "r", "r", "r"))
print(
  stratified_results_xtable,
  file = file.path(output_path, "stratified_results_4yr.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  tabular.environment = "tabular"
)


## (c) ----


### earnings1yr ----

# Aggregated DIM estimate
agg_dim <- sum(weights * stratified_results_1yr$`DIM estimate`)

# Aggregated Neyman variance
agg_var <- sum((weights^2) * stratified_results_1yr$`Neyman variance`)

# 90% confidence interval (z = 1.645)
agg_ci_lower <- agg_dim - 1.645 * sqrt(agg_var)
agg_ci_upper <- agg_dim + 1.645 * sqrt(agg_var)

# Store in a data.table for output
aggregate_results_1yr <- data.table(
  Statistic = c("Aggregated DIM estimate", "Aggregated Neyman variance", 
                "Aggregated CI lower bound (90%)", "Aggregated CI upper bound (90%)"),
  Value = c(agg_dim, agg_var, agg_ci_lower, agg_ci_upper)
)

print(aggregate_results_1yr)

# Output to LaTeX table
aggregate_results_1yr_xtable <- xtable(aggregate_results_1yr, align = c("l", "l", "r"))
print(
  aggregate_results_1yr_xtable,
  file = file.path(output_path, "aggregate_results_1yr.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  tabular.environment = "tabular"
)

### earnings4yr ----

# Aggregated DIM estimate
agg_dim <- sum(weights * stratified_results_4yr$`DIM estimate`)

# Aggregated Neyman variance
agg_var <- sum((weights^2) * stratified_results_4yr$`Neyman variance`)

# 90% confidence interval (z = 1.645)
agg_ci_lower <- agg_dim - 1.645 * sqrt(agg_var)
agg_ci_upper <- agg_dim + 1.645 * sqrt(agg_var)

# Store in a data.table for output
aggregate_results_4yr <- data.table(
  Statistic = c("Aggregated DIM estimate", "Aggregated Neyman variance", 
                "Aggregated CI lower bound (90%)", "Aggregated CI upper bound (90%)"),
  Value = c(agg_dim, agg_var, agg_ci_lower, agg_ci_upper)
)

print(aggregate_results_4yr)

# Output to LaTeX table
aggregate_results_4yr_xtable <- xtable(aggregate_results_4yr, align = c("l", "l", "r"))
print(
  aggregate_results_4yr_xtable,
  file = file.path(output_path, "aggregate_results_4yr.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  tabular.environment = "tabular"
)


balance <- data[order(hs_diploma, child, W), .N, .(W, hs_diploma, child)]
balance[, `:=`(W = ifelse(W == 1, "Treated", "Control"), hs_diploma = ifelse(hs_diploma == 1, "Yes", "No"), child = ifelse(child == 1, "Yes", "No"))]
names(balance) <- c("Treatment", "High School Degree", "Child", "Number of Obs.")
balance

balance_xtable <- xtable(balance, align = c("l", "l", "l", "l", "r"))
print(
  balance_xtable,
  file = file.path(output_path, "balance.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  tabular.environment = "tabular"
)
