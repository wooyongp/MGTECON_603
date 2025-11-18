# Setting up ----

## packages ----
library(here)
library(data.table)
library(ggplot2)
library(knitr)
library(kableExtra)
library(GGally)
library(ggcorrplot)
library(fixest)
library(foreach)
library(doParallel)

## setting path ----
setwd(here::here("PS6"))
output_path <- here::here("PS6", "output"); if(!dir.exists(output_path)) dir.create(output_path)
log_file <- here::here("PS6", "output", "log.txt")

## setting seed ----
set.seed(123)

## create log file if it doesn't exist
if(!file.exists(log_file)) {
  file.create(log_file)
} else {
  file.remove(log_file)
  file.create(log_file)
}


## Parallelization - Use more cores ----
n_cores <- parallel::detectCores() - 8
cl <- makeCluster(n_cores)
registerDoParallel(cl)

## Additional Options ----
theme_set(theme_minimal())
stars = c(0.01, 0.05, 0.1)


## data ----
data <- data.table::fread(file.path("jive.txt"))

# Pre-compute constants
n <- nrow(data)
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


### Generate output functions ----
create_table <- function(data, filename) {
  tab <- display_summary_stats(data) |> 
    kable(digits = 4, format = "latex", booktabs = TRUE, format.args = list(decimal.mark = ".", big.mark = ","))
  writeLines(tab, file.path(output_path, filename))
  return(tab)
}

### write a code log to a file ----
cat_to_file <- function(...) {
  cat(...)
  cat(..., file = log_file, append = TRUE)
}

# Data Preparation ----
## Create ID variable
data[, id := .I]


# Summary Statistics ----
cat("Computing summary statistics...\n")
create_table(data[, !c("id"), with = FALSE], "summary_stats.tex")

## Create scatterplot matrix
ggpairs(data[, !c("id"), with = FALSE], diag = list(continuous = 'barDiag'))
ggsave(file.path(output_path, "scatterplot_matrix.png"), width=8, height=6)

## Create correlation matrix
cor_matrix <- cor(data[, !c("id"), with = FALSE])
cor_matrix

## Create correlation matrix plot
ggcorrplot(cor_matrix, lab=TRUE, type = "upper", lab_size = 6)
ggsave(file.path(output_path, "correlation_matrix_plot.png"), width=8, height=6)


# Problem 1 ----
## Use data from jive.txt, observations on educ, lwage, sob, and yob

## (a) estimate lwage on educ and (b) calculate the OLS standard errors
model_homo <- feols(lwage ~ educ, data = data)
summary(model_homo)

## (c) estimate lwage on educ using cluster standard errors
model_cluster <- feols(lwage ~ educ, data = data, cluster = ~ sob)
summary(model_cluster)

models <- list("Model 1: homoskedastic s.e." = model_homo, "Model 2: cluster-robust s.e." = model_cluster)


if(file.exists(file.path(output_path, "regression_problem_1_a.tex"))) file.remove(file.path(output_path, "regression_problem_1_a.tex"))
etable(models, 
  file = file.path(output_path, "regression_problem_1_a.tex"),
  digits = 4,
  title = "Regression of lwage on educ",
  label = "tab:regression_problem_1_a"
)

## (d) caclulate both OLS and CRSE by bootstrapping

model_homo_coefficients <- model_homo$coefficients |> as.vector()
b0 <- model_homo_coefficients[1]
b1 <- model_homo_coefficients[2]
residuals <- model_homo$residuals
## For the regular OLS, we can use parametric bootstrap

OLS_b_vector <- foreach(bbb = 1:B, .combine = 'c') %dopar% {
  residuals_b <- residuals[sample(1:n, size = n, replace = TRUE)]
  lwage_b <- b0 + b1 * data$educ + residuals_b
  model_homo_b <- lm(lwage_b ~ data$educ)
  model_homo_coefficients_b <- model_homo_b$coefficients |> as.vector()
  b1_b <- model_homo_coefficients_b[2]
  return(b1_b)
}

OLS_b_vector

# Calculate the standard error of the OLS coefficients
OLS_b_vector_sd <- sd(OLS_b_vector)


## For the cluster-robust OLS, we
## 1. resample 51 clusters with replacement
## 2. resample individuals within each cluster with replacement
## 3. estimate the model
## 4. store the coefficient

cluster_ids <- unique(data$sob)

CRSE_b_vector <- foreach(
  bbb = 1:B, 
  .combine = 'c', 
  .packages = c("fixest", "data.table", "foreach")
) %dopar% {
  cluster_ids_b <- sample(cluster_ids, size = 51, replace = TRUE)
  data_b <- foreach(
    cluster_id = cluster_ids_b, 
    .combine = rbind
  ) %do% { # <- Change inner loop to %do%
    cb <- data[sob == cluster_id]
    cb <- cb[sample(1:nrow(cb), size = nrow(cb), replace = TRUE)]
    return(cb)
  }
  model_cluster_b <- feols(lwage ~ educ, data = data_b)
  model_cluster_coefficients_b <- model_cluster_b$coefficients |> as.vector()
  b1_b <- model_cluster_coefficients_b[2]
  return(b1_b)
}

CRSE_b_vector

# Calculate the standard error of the CRSE coefficients
CRSE_b_vector_sd <- sd(CRSE_b_vector)


# Print the results
cat_to_file("OLS_b_vector_sd: ", OLS_b_vector_sd, "\n")
cat_to_file("CRSE_b_vector_sd: ", CRSE_b_vector_sd, "\n")

# Create a data.table with meaningful column names
se_table <- data.table(
  `OLS Standard Error` = OLS_b_vector_sd,
  `Cluster-Robust Standard Error` = CRSE_b_vector_sd
)
# Save as .tex in the output folder
print(
  xtable::xtable(se_table, caption = "Comparison of OLS and Cluster-Robust Standard Errors(bootstrapped)", label = "tab:se_comparison_d", digits = 4),
  file = file.path(output_path, "se_comparison_d.tex"),
  include.rownames = FALSE
)

# Problem 1(continued) ----
## (e) estimate lwage on mean educ and (f) calculate the OLS standard errors
data[, mean_educ := mean(educ), by = sob]
model_homo <- feols(lwage ~ educ + mean_educ, data = data)
summary(model_homo)

## (g) estimate lwage on educ using cluster standard errors
model_cluster <- feols(lwage ~ educ + mean_educ, data = data, cluster = ~ sob)
summary(model_cluster)

models <- list("Model 1: homoskedastic s.e." = model_homo, "Model 2: cluster-robust s.e." = model_cluster)

if(file.exists(file.path(output_path, "regression_problem_1_e.tex"))) file.remove(file.path(output_path, "regression_problem_1_e.tex"))
etable(models, 
  file = file.path(output_path, "regression_problem_1_e.tex"),
  digits = 4,
  title = "Regression of lwage on mean educ",
  label = "tab:regression_problem_1_e"
)

## (h) caclulate both OLS and CRSE by bootstrapping

model_homo_coefficients <- model_homo$coefficients |> as.vector()
b0 <- model_homo_coefficients[1]
b1 <- model_homo_coefficients[2]
b2 <- model_homo_coefficients[3]
residuals <- model_homo$residuals
## For the regular OLS, we can use parametric bootstrap

OLS_b_vector <- foreach(bbb = 1:B, .combine = 'rbind') %dopar% {
  residuals_b <- residuals[sample(1:n, size = n, replace = TRUE)]
  lwage_b <- b0 + b1 * data$educ + b2 * data$mean_educ + residuals_b
  model_homo_b <- lm(lwage_b ~ data$educ + data$mean_educ)
  model_homo_coefficients_b <- model_homo_b$coefficients |> as.vector()
  b1_b <- model_homo_coefficients_b[2]
  b2_b <- model_homo_coefficients_b[3]
  return(c(b1_b, b2_b))
} |> as.data.table()

OLS_b_vector

# Calculate the standard error of the OLS coefficients
OLS_b_vector_sd <- OLS_b_vector[, .(sd_b1 = sd(V1), sd_b2 = sd(V2))]
OLS_b_vector_sd[, type := "OLS"]

## For the cluster-robust OLS, we
## 1. resample 51 clusters with replacement
## 2. resample individuals within each cluster with replacement
## 3. estimate the model
## 4. store the coefficient

cluster_ids <- unique(data$sob)

CRSE_b_vector <- foreach(
  bbb = 1:B, 
  .combine = 'rbind', 
  .packages = c("fixest", "data.table", "foreach") # <- Add this line
) %dopar% {
  cluster_ids_b <- sample(cluster_ids, size = 51, replace = TRUE)
  data_b <- foreach(
    cluster_id = cluster_ids_b, 
    .combine = rbind
  ) %do% { # <- Change inner loop to %do%
    cb <- data[sob == cluster_id]
    cb <- cb[sample(1:nrow(cb), size = nrow(cb), replace = TRUE)]
    return(cb)
  }
  model_cluster_b <- feols(lwage ~ educ + mean_educ, data = data_b)
  model_cluster_coefficients_b <- model_cluster_b$coefficients |> as.vector()
  b1_b <- model_cluster_coefficients_b[2]
  b2_b <- model_cluster_coefficients_b[3]
  return(c(b1_b, b2_b))
}

CRSE_b_vector <- CRSE_b_vector |> as.data.table()
names(CRSE_b_vector) <- c("b1", "b2")

# Calculate the standard error of the CRSE coefficients
CRSE_b_vector_sd <- CRSE_b_vector[, .(sd_b1 = sd(b1), sd_b2 = sd(b2))]
CRSE_b_vector_sd[, type := "CRSE"]
bootstrap_results <- rbind(OLS_b_vector_sd, CRSE_b_vector_sd)


# Print the results to log
cat_to_file("Bootstrapped Standard Errors for model with educ and mean_educ:\n")
capture.output(bootstrap_results) |> paste(collapse = "\n") |> cat_to_file()

# Reshape for the TeX table
# Rename columns for clarity
setnames(bootstrap_results, c("type", "sd_b1", "sd_b2"), c("Estimator", "SE(educ)", "SE(mean_educ)"))

# Reorder columns
setcolorder(bootstrap_results, c("Estimator", "SE(educ)", "SE(mean_educ)"))

# Save as .tex in the output folder
print(
  xtable::xtable(bootstrap_results, caption = "Comparison of Bootstrapped OLS and Cluster-Robust Standard Errors", label = "tab:se_comparison_h", digits = 4),
  file = file.path(output_path, "se_comparison_h.tex"),
  include.rownames = FALSE
)

# Stop the cluster
stopCluster(cl)


