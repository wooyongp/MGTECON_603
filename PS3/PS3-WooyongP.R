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
setwd(here::here("PS3"))
output_path <- here::here("PS3", "output"); if(!dir.exists(output_path)) dir.create(output_path)
log_file <- here::here("PS3", "output", "log.txt"); if(!file.exists(log_file)) file.create(log_file) else file.remove(log_file); file.create(log_file)

## setting seed ----
set.seed(123)

## Parallelization - Use more cores ----
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

## data ----
data <- data.table::fread(file.path("riverside_2025.txt"))
names(data) <- c("W", "earnings1yr", "earnings4yr", "hs_diploma", "female", "age", "child", "single")

# Pre-compute constants
n <- nrow(data)
M <- sum(data$W) # More efficient than nrow(data[W==1])
B <- 10000 # Number of bootstrap samples

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

# write a code log to a file
cat_to_file <- function(...) {
  cat(...)
  cat(..., file = log_file, append = TRUE)
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


## DIM for 1yr and 4yr earnings
DIM_1 <- as.numeric(data[W==1, mean(earnings1yr)] - data[W==0, mean(earnings1yr)])
DIM_4 <- as.numeric(data[W==1, mean(earnings4yr)] - data[W==0, mean(earnings4yr)])


cat_to_file("DIM_1: ", DIM_1, "\n")
cat_to_file("DIM_4: ", DIM_4, "\n")

# Problem 1 ----
cat_to_file("Problem 1 ----\n")
data_treated <- data[W==1]
data_control <- data[W==0]

## (a) ----
cat_to_file("(a) ----\n")
DIM_1_boot_vector <- c()
DIM_4_boot_vector <- c()

for (i in 1:B){
  dt <- data |> sample_n(size = n, replace = TRUE)
  DIM_1_boot <- as.numeric(dt[W==1, .(mean = mean(earnings1yr))] - dt[W==0, .(mean = mean(earnings1yr))])
  DIM_4_boot <- as.numeric(dt[W==1, .(mean = mean(earnings4yr))] - dt[W==0, .(mean = mean(earnings4yr))])
  DIM_1_boot_vector[i] <- DIM_1_boot
  DIM_4_boot_vector[i] <- DIM_4_boot
}

bootstrap_variance_1 <- sum((DIM_1_boot_vector - DIM_1)^2) / B
bootstrap_variance_4 <- sum((DIM_4_boot_vector - DIM_4)^2) / B

bootstrap_se_1 <- sqrt(bootstrap_variance_1)
bootstrap_se_4 <- sqrt(bootstrap_variance_4)

z_90 <- qnorm(0.95)  # 1.645

# Store vanilla bootstrap confidence intervals
ci_90_lb_1_vanilla <- DIM_1 - z_90 * bootstrap_se_1
ci_90_ub_1_vanilla <- DIM_1 + z_90 * bootstrap_se_1
ci_90_lb_4_vanilla <- DIM_4 - z_90 * bootstrap_se_4
ci_90_ub_4_vanilla <- DIM_4 + z_90 * bootstrap_se_4

cat_to_file("bootstrap_variance_1: ", bootstrap_variance_1, "\n")
cat_to_file("bootstrap_variance_4: ", bootstrap_variance_4, "\n")
cat_to_file("bootstrap_se_1: ", bootstrap_se_1, "\n")
cat_to_file("bootstrap_se_4: ", bootstrap_se_4, "\n")

cat_to_file("ci_90_lb_1_vanilla: ", ci_90_lb_1_vanilla, "\n")
cat_to_file("ci_90_ub_1_vanilla: ", ci_90_ub_1_vanilla, "\n")
cat_to_file("ci_90_lb_4_vanilla: ", ci_90_lb_4_vanilla, "\n")
cat_to_file("ci_90_ub_4_vanilla: ", ci_90_ub_4_vanilla, "\n")

## (b) ----
cat_to_file("(b) ----\n")
DIM_1_bboot_vector <- c()
DIM_4_bboot_vector <- c()

W <- data$W
earnings1yr <- data$earnings1yr
earnings4yr <- data$earnings4yr

for (i in 1:B){
  E_b <- rexp(n=n, rate=1)
  R_b <- E_b / sum(E_b)
  tau_b1 <- sum(R_b*W*earnings1yr) / sum(R_b*W) - sum(R_b*(1-W)*earnings1yr) / sum(R_b*(1-W))
  tau_b4 <- sum(R_b*W*earnings4yr) / sum(R_b*W) - sum(R_b*(1-W)*earnings4yr) / sum(R_b*(1-W))
  DIM_1_bboot_vector[i] <- tau_b1
  DIM_4_bboot_vector[i] <- tau_b4
}

bootstrap_variance_1_b <- sum((DIM_1_bboot_vector - DIM_1)^2) / B
bootstrap_variance_4_b <- sum((DIM_4_bboot_vector - DIM_4)^2) / B

bootstrap_se_1_b <- sqrt(bootstrap_variance_1_b)
bootstrap_se_4_b <- sqrt(bootstrap_variance_4_b)

# Store Bayesian bootstrap confidence intervals
ci_90_lb_1_bayesian <- DIM_1 - z_90 * bootstrap_se_1_b
ci_90_ub_1_bayesian <- DIM_1 + z_90 * bootstrap_se_1_b
ci_90_lb_4_bayesian <- DIM_4 - z_90 * bootstrap_se_4_b
ci_90_ub_4_bayesian <- DIM_4 + z_90 * bootstrap_se_4_b

cat_to_file("bootstrap_variance_1_b: ", bootstrap_variance_1_b, "\n")
cat_to_file("bootstrap_variance_4_b: ", bootstrap_variance_4_b, "\n")
cat_to_file("bootstrap_se_1_b: ", bootstrap_se_1_b, "\n")
cat_to_file("bootstrap_se_4_b: ", bootstrap_se_4_b, "\n")

cat_to_file("ci_90_lb_1_bayesian: ", ci_90_lb_1_bayesian, "\n")
cat_to_file("ci_90_ub_1_bayesian: ", ci_90_ub_1_bayesian, "\n")
cat_to_file("ci_90_lb_4_bayesian: ", ci_90_lb_4_bayesian, "\n")
cat_to_file("ci_90_ub_4_bayesian: ", ci_90_ub_4_bayesian, "\n")

## (c) ----
cat_to_file("(c) ----\n")


### compute the Neyman standard error
Neyman_se_1 <- sqrt(data_treated[, .(var = sample_variance(earnings1yr))] / nrow(data_treated) + data_control[, .(var = sample_variance(earnings1yr))] / nrow(data_control)) |> as.numeric()
Neyman_se_4 <- sqrt(data_treated[, .(var = sample_variance(earnings4yr))] / nrow(data_treated) + data_control[, .(var = sample_variance(earnings4yr))] / nrow(data_control)) |> as.numeric()
cat_to_file("Neyman_se_1: ", Neyman_se_1, "\n")
cat_to_file("Neyman_se_4: ", Neyman_se_4, "\n")


DIM_1_boot_vector_pivot <- c()
DIM_4_boot_vector_pivot <- c()

for (i in 1:B){
  dt <- data |> sample_n(size = n, replace = TRUE)
  temp1 <- as.numeric(dt[W==1, .(mean = mean(earnings1yr))] - dt[W==0, .(mean = mean(earnings1yr))])
  temp2 <- as.numeric(dt[W==1, .(mean = mean(earnings4yr))] - dt[W==0, .(mean = mean(earnings4yr))])
  
  # Compute Neyman standard errors for this bootstrap sample
  dt_treated <- dt[W==1]
  dt_control <- dt[W==0]
  se_1_boot <- sqrt(dt_treated[, .(var = sample_variance(earnings1yr))] / nrow(dt_treated) + dt_control[, .(var = sample_variance(earnings1yr))] / nrow(dt_control)) |> as.numeric()
  se_4_boot <- sqrt(dt_treated[, .(var = sample_variance(earnings4yr))] / nrow(dt_treated) + dt_control[, .(var = sample_variance(earnings4yr))] / nrow(dt_control)) |> as.numeric()

  temp1 <- (temp1 - DIM_1) / se_1_boot
  temp2 <- (temp2 - DIM_4) / se_4_boot
  DIM_1_boot_vector_pivot[i] <- temp1
  DIM_4_boot_vector_pivot[i] <- temp2
}

### quantiles
percentile_5th_1 <- quantile(DIM_1_boot_vector_pivot, probs = 0.05)
percentile_95th_1 <- quantile(DIM_1_boot_vector_pivot, probs = 0.95)
percentile_5th_4 <- quantile(DIM_4_boot_vector_pivot, probs = 0.05)
percentile_95th_4 <- quantile(DIM_4_boot_vector_pivot, probs = 0.95)
cat_to_file("5th percentile of DIM_1_boot_vector_pivot: ", percentile_5th_1, "\n")
cat_to_file("95th percentile of DIM_1_boot_vector_pivot: ", percentile_95th_1, "\n")
cat_to_file("5th percentile of DIM_4_boot_vector_pivot: ", percentile_5th_4, "\n")
cat_to_file("95th percentile of DIM_4_boot_vector_pivot: ", percentile_95th_4, "\n")


### Confidence intervals
ci_90_lb_1 <- DIM_1 - percentile_95th_1 * Neyman_se_1
ci_90_ub_1 <- DIM_1 - percentile_5th_1 * Neyman_se_1
ci_90_lb_4 <- DIM_4 - percentile_95th_4 * Neyman_se_4
ci_90_ub_4 <- DIM_4 - percentile_5th_4 * Neyman_se_4

cat_to_file("ci_90_lb_1: ", ci_90_lb_1, "\n")
cat_to_file("ci_90_ub_1: ", ci_90_ub_1, "\n")
cat_to_file("ci_90_lb_4: ", ci_90_lb_4, "\n")
cat_to_file("ci_90_ub_4: ", ci_90_ub_4, "\n")


## (d) ----
cat_to_file("(d) ----\n")
subsample_size <- round(sqrt(nrow(data)))

DIM_1_sboot_vector <- c()
DIM_4_sboot_vector <- c()

for (i in 1:B){
  subsample <- data |> sample_n(size = subsample_size, replace = FALSE)
  W <- subsample$W
  earnings1yr <- subsample$earnings1yr
  earnings4yr <- subsample$earnings4yr
  tau_b1 <- sum(W*earnings1yr) / sum(W) - sum((1-W)*earnings1yr) / sum(1-W)
  tau_b4 <- sum(W*earnings4yr) / sum(W) - sum((1-W)*earnings4yr) / sum(1-W)
  DIM_1_sboot_vector[i] <- tau_b1
  DIM_4_sboot_vector[i] <- tau_b4
}

bootstrap_variance_1_s <- sum((DIM_1_sboot_vector - DIM_1)^2) / B *(subsample_size/n)
bootstrap_variance_4_s <- sum((DIM_4_sboot_vector - DIM_4)^2) / B *(subsample_size/n)

bootstrap_se_1_s <- sqrt(bootstrap_variance_1_s)
bootstrap_se_4_s <- sqrt(bootstrap_variance_4_s)

# Store subsampling bootstrap confidence intervals
ci_90_lb_1_subsampling <- DIM_1 - z_90 * bootstrap_se_1_s
ci_90_ub_1_subsampling <- DIM_1 + z_90 * bootstrap_se_1_s
ci_90_lb_4_subsampling <- DIM_4 - z_90 * bootstrap_se_4_s
ci_90_ub_4_subsampling <- DIM_4 + z_90 * bootstrap_se_4_s

cat_to_file("bootstrap_variance_1_s: ", bootstrap_variance_1_s, "\n")
cat_to_file("bootstrap_variance_4_s: ", bootstrap_variance_4_s, "\n")
cat_to_file("bootstrap_se_1_s: ", bootstrap_se_1_s, "\n")
cat_to_file("bootstrap_se_4_s: ", bootstrap_se_4_s, "\n")
cat_to_file("ci_90_lb_1_subsampling: ", ci_90_lb_1_subsampling, "\n")
cat_to_file("ci_90_ub_1_subsampling: ", ci_90_ub_1_subsampling, "\n")
cat_to_file("ci_90_lb_4_subsampling: ", ci_90_lb_4_subsampling, "\n")
cat_to_file("ci_90_ub_4_subsampling: ", ci_90_ub_4_subsampling, "\n")


## (e) ----
cat_to_file("(e) ----\n")
# Estimator: OLS regression with interacting high school degree
# Reason: We can use the OLS regression with interacting high school degree to estimate the ATE because it can adjust for the heterogeneity in the treatment effect across different high school degree groups.

model_1 <- fixest::feols(earnings1yr ~ W * hs_diploma, data = data)
model_4 <- fixest::feols(earnings4yr ~ W * hs_diploma, data = data)

# Store the coefficients
model_1$coefficients
model_4$coefficients

OLS_1 <- model_1$coefficients[2]
OLS_4 <- model_4$coefficients[2]
OLS_1_hs_diploma <- model_1$coefficients[3]
OLS_4_hs_diploma <- model_4$coefficients[3]

# Store the residuals
residuals_1 <- model_1$residuals
residuals_4 <- model_4$residuals

# Store the W and hs_diploma
W <- data$W
hs_diploma <- data$hs_diploma

# Calculate the sample proportion with high school diploma
p_hs <- mean(data$hs_diploma)

# Store the ATE as linear combination of coefficients
ATE_1 <- model_1$coefficients[2] + model_1$coefficients[4] * p_hs  # β1 + β3 * p_hs
ATE_4 <- model_4$coefficients[2] + model_4$coefficients[4] * p_hs  # β1 + β3 * p_hs

cat_to_file("ATE_1: ", ATE_1, "\n")
cat_to_file("ATE_4: ", ATE_4, "\n")
cat_to_file("p_hs (proportion with hs diploma): ", p_hs, "\n")

# I use the parametric bootstrap to estimate the confidence interval of the ATE.

ATE_1_boot_vector <- c()
ATE_4_boot_vector <- c()

for (i in 1:B){
  residuals_1_b <- residuals_1[sample(1:n, size = n, replace = TRUE)]
  residuals_4_b <- residuals_4[sample(1:n, size = n, replace = TRUE)]
  earnings1yr_b_hat <- model_1$coefficients[1] + model_1$coefficients[2] * W + model_1$coefficients[3] * hs_diploma + model_1$coefficients[4] * hs_diploma * W + residuals_1_b
  earnings4yr_b_hat <- model_4$coefficients[1] + model_4$coefficients[2] * W + model_4$coefficients[3] * hs_diploma + model_4$coefficients[4] * hs_diploma * W + residuals_4_b
  model_1_b <- lm(earnings1yr_b_hat ~ W * hs_diploma)
  model_4_b <- lm(earnings4yr_b_hat ~ W * hs_diploma)
  
  # Calculate bootstrap ATE as linear combination of coefficients
  ATE_1_boot_vector[i] <- model_1_b$coefficients[2] + model_1_b$coefficients[4] * p_hs
  ATE_4_boot_vector[i] <- model_4_b$coefficients[2] + model_4_b$coefficients[4] * p_hs
}

# Calculate parametric bootstrap variance of the ATE
parametric_bootstrap_variance_ATE_1 <- sum((ATE_1_boot_vector - ATE_1)^2) / B
parametric_bootstrap_variance_ATE_4 <- sum((ATE_4_boot_vector - ATE_4)^2) / B

parametric_bootstrap_se_ATE_1 <- sqrt(parametric_bootstrap_variance_ATE_1)
parametric_bootstrap_se_ATE_4 <- sqrt(parametric_bootstrap_variance_ATE_4)

z_95 <- qnorm(0.975)

# 90% confidence intervals for ATE
ci_90_lb_ATE_1 <- ATE_1 - z_90 * parametric_bootstrap_se_ATE_1
ci_90_ub_ATE_1 <- ATE_1 + z_90 * parametric_bootstrap_se_ATE_1
ci_90_lb_ATE_4 <- ATE_4 - z_90 * parametric_bootstrap_se_ATE_4
ci_90_ub_ATE_4 <- ATE_4 + z_90 * parametric_bootstrap_se_ATE_4

# 95% confidence intervals for ATE
ci_95_lb_ATE_1 <- ATE_1 - z_95 * parametric_bootstrap_se_ATE_1
ci_95_ub_ATE_1 <- ATE_1 + z_95 * parametric_bootstrap_se_ATE_1
ci_95_lb_ATE_4 <- ATE_4 - z_95 * parametric_bootstrap_se_ATE_4
ci_95_ub_ATE_4 <- ATE_4 + z_95 * parametric_bootstrap_se_ATE_4

cat_to_file("parametric_bootstrap_variance_ATE_1: ", parametric_bootstrap_variance_ATE_1, "\n")
cat_to_file("parametric_bootstrap_variance_ATE_4: ", parametric_bootstrap_variance_ATE_4, "\n")
cat_to_file("parametric_bootstrap_se_ATE_1: ", parametric_bootstrap_se_ATE_1, "\n")
cat_to_file("parametric_bootstrap_se_ATE_4: ", parametric_bootstrap_se_ATE_4, "\n")
cat_to_file("ci_90_lb_ATE_1: ", ci_90_lb_ATE_1, "\n")
cat_to_file("ci_90_ub_ATE_1: ", ci_90_ub_ATE_1, "\n")
cat_to_file("ci_90_lb_ATE_4: ", ci_90_lb_ATE_4, "\n")
cat_to_file("ci_90_ub_ATE_4: ", ci_90_ub_ATE_4, "\n")
cat_to_file("ci_95_lb_ATE_1: ", ci_95_lb_ATE_1, "\n")
cat_to_file("ci_95_ub_ATE_1: ", ci_95_ub_ATE_1, "\n")
cat_to_file("ci_95_lb_ATE_4: ", ci_95_lb_ATE_4, "\n")
cat_to_file("ci_95_ub_ATE_4: ", ci_95_ub_ATE_4, "\n")

# Create comparison plots for bootstrap confidence intervals ----
cat_to_file("Creating comparison plots...\n")

# Extract results from log for plotting
# Note: Some variables get overwritten, so we need to recalculate or use the final values

# 1-year earnings results
methods_1yr <- c("Vanilla Bootstrap", "Bayesian Bootstrap", "Pivotal Bootstrap", "Subsampling Bootstrap", "OLS Parametric Bootstrap")
estimates_1yr <- c(DIM_1, DIM_1, DIM_1, DIM_1, ATE_1)  # Point estimates

# Now we can use the unique variable names that were properly stored
ci_lower_1yr <- c(ci_90_lb_1_vanilla, ci_90_lb_1_bayesian, ci_90_lb_1, ci_90_lb_1_subsampling, ci_90_lb_ATE_1)
ci_upper_1yr <- c(ci_90_ub_1_vanilla, ci_90_ub_1_bayesian, ci_90_ub_1, ci_90_ub_1_subsampling, ci_90_ub_ATE_1)

# 4-year earnings results
methods_4yr <- c("Vanilla Bootstrap", "Bayesian Bootstrap", "Pivotal Bootstrap", "Subsampling Bootstrap", "OLS Parametric Bootstrap")
estimates_4yr <- c(DIM_4, DIM_4, DIM_4, DIM_4, ATE_4)  # Point estimates
ci_lower_4yr <- c(ci_90_lb_4_vanilla, ci_90_lb_4_bayesian, ci_90_lb_4, ci_90_lb_4_subsampling, ci_90_lb_ATE_4)
ci_upper_4yr <- c(ci_90_ub_4_vanilla, ci_90_ub_4_bayesian, ci_90_ub_4, ci_90_ub_4_subsampling, ci_90_ub_ATE_4)

# Create data frames for plotting
plot_data_1yr <- data.frame(
  Method = factor(methods_1yr, levels = methods_1yr),
  Estimate = estimates_1yr,
  CI_Lower = ci_lower_1yr,
  CI_Upper = ci_upper_1yr,
  CI_Width = ci_upper_1yr - ci_lower_1yr
)

plot_data_4yr <- data.frame(
  Method = factor(methods_4yr, levels = methods_4yr),
  Estimate = estimates_4yr,
  CI_Lower = ci_lower_4yr,
  CI_Upper = ci_upper_4yr,
  CI_Width = ci_upper_4yr - ci_lower_4yr
)

# Plot for 1-year earnings
p1 <- ggplot(plot_data_1yr, aes(x = Method, y = Estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.3, color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = DIM_1, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "90% Confidence Intervals for Treatment Effect (1-Year Earnings)",
    subtitle = "Comparison of Different Bootstrap Methods",
    x = "Bootstrap Method",
    y = "Treatment Effect Estimate",
    caption = "Red dashed line shows the simple difference-in-means estimate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0.8, 1.5, 0.1))

# Plot for 4-year earnings
p2 <- ggplot(plot_data_4yr, aes(x = Method, y = Estimate)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.3, color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = DIM_4, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(
    title = "90% Confidence Intervals for Treatment Effect (4-Year Earnings)",
    subtitle = "Comparison of Different Bootstrap Methods",
    x = "Bootstrap Method",
    y = "Treatment Effect Estimate",
    caption = "Red dashed line shows the simple difference-in-means estimate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0.6, 1.8, 0.2))

# Save plots
ggsave(file.path(output_path, "bootstrap_comparison_1yr.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(output_path, "bootstrap_comparison_4yr.png"), p2, width = 12, height = 8, dpi = 300)

# Print summary statistics
cat_to_file("\n=== Bootstrap Method Comparison Summary ===\n")
cat_to_file("1-Year Earnings - Confidence Interval Widths:\n")
for(i in 1:nrow(plot_data_1yr)) {
  cat_to_file(sprintf("%s: %.4f\n", methods_1yr[i], plot_data_1yr$CI_Width[i]))
}

cat_to_file("\n4-Year Earnings - Confidence Interval Widths:\n")
for(i in 1:nrow(plot_data_4yr)) {
  cat_to_file(sprintf("%s: %.4f\n", methods_4yr[i], plot_data_4yr$CI_Width[i]))
}

cat_to_file("\nPlots saved to output directory.\n")
