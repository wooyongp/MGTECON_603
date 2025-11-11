# Setting up ----

## packages ----
library(here)
library(data.table)
library(texreg)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(fixest)
library(doParallel)
library(foreach)
library(knitr)


## setting path ----
setwd(here::here("PS5"))
output_path <- here::here("PS5", "output"); if(!dir.exists(output_path)) dir.create(output_path)
log_file <- here::here("PS5", "output", "log.txt")


## create log file if it doesn't exist
if(!file.exists(log_file)) {
  file.create(log_file)
} else {
  file.remove(log_file)
  file.create(log_file)
}


## Parallelization - Use more cores ----
n_cores <- parallel::detectCores() - 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)

## Additional Options ----
theme_set(theme_minimal())
stars = c(0.01, 0.05, 0.1)


## data ----
data <- data.table::fread(file.path("lalonde.txt"))

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


# Generate output functions
create_table <- function(data, filename) {
  tab <- display_summary_stats(data) |> 
    kable(digits = 4, format = "latex", booktabs = TRUE, format.args = list(decimal.mark = ".", big.mark = ","))
  writeLines(tab, file.path(output_path, filename))
  return(tab)
}


### Compute Leverage Matrix
leverage_matrix <- function(data, rhs){
  X <- model.matrix(as.formula(paste0("~ ", rhs)), data = data)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  return(H)
} # use the diag() function to extract the actual leverage value of each observation

# write a code log to a file
cat_to_file <- function(...) {
  cat(...)
  cat(..., file = log_file, append = TRUE)
}

# Data Preparation ----
## Create ID variable
data[, id := .I]


# Summary Statistics ----
cat("Computing summary statistics...\n")
create_table(data, "summary_stats.tex")

## Create scatterplot matrix
ggpairs(data, diag = list(continuous = 'barDiag'))
ggsave(file.path(output_path, "scatterplot_matrix.png"), width=8, height=6)

## Create correlation matrix
cor_matrix <- cor(data)
cor_matrix

## Create correlation matrix plot
ggcorrplot(cor_matrix)
ggsave(file.path(output_path, "correlation_matrix_plot.png"), width=8, height=6)


# Problem 1 ----

## Scatterplot of re75 and re78
mean_re75 <- mean(data$re75)
mean_re78 <- mean(data$re78)
ggplot(data, aes(x=re75, y=re78)) + 
  geom_point() + 
  geom_hline(yintercept = mean_re78, color = "red", linetype = "dashed", linewidth = 2) +
  geom_vline(xintercept = mean_re75, color = "red", linetype = "dashed", linewidth = 2) +
  geom_smooth(method = "lm") +
  labs(x="re75", y="re78")
ggsave(file.path(output_path, "scatterplot_re75_re78.png"), width=8, height=6)

ggplot(data, aes(x=re75)) + 
  geom_histogram() +
  labs(x="re75", y="Count")
ggsave(file.path(output_path, "histogram_re75.png"), width=8, height=6)

## Regression with feols
rhs_short <- "re75"
l <- fixest::feols(fml = as.formula(paste0("re78 ~ ", rhs_short)), data = data, vcov = "hetero")
summary(l)
texreg::texreg(l, 
  file = file.path(output_path, "regression_problem_1.tex"),
  stars = stars,
  digits = 4,
  caption = "Regression of re78 on re75",
  label = "tab:regression_re75",
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.projrsquared = FALSE,
  float.pos = "h")
H <- leverage_matrix(data, rhs_short)
h <- diag(H)

## From the leverage formula in the single covariate case, we know that the end points of the data are the most influential observations.
data[, leverage_re75 := h]
ggplot(data, aes(x=re75, y=leverage_re75)) + 
  geom_point() + 
  labs(x="re75", y="leverage_re75")
ggsave(file.path(output_path, "scatterplot_re75_leverage_re75.png"), width=8, height=6)

## values for earnings in 1975 for the five highest leverage observations
highest_leverage_data <- data[order(leverage_re75, decreasing = TRUE)][1:5, .(re75, leverage_re75)]
print(xtable::xtable(highest_leverage_data, caption = "Top 5 observations with the highest leverage(single regressor)", label = "tab:highest_leverage_re75", digits = 6), file = file.path(output_path, "highest_leverage_re75.tex"))
rm(highest_leverage_data)

# Problem 2 ----
rhs_long <- "re75 + re74 + education + black + hispanic + age"
K <- length(strsplit(rhs_long, "\\s*\\+\\s*")[[1]]) + 1
l <- fixest::feols(fml = as.formula(paste0("re78 ~ ", rhs_long)), data = data, vcov = "hetero")
summary(l)
texreg::texreg(l, 
  file = file.path(output_path, "regression_problem_2.tex"),
  stars = stars,
  digits = 4,
  caption = "Regression of re78 on re75, re74, education, black, hispanic, and age",
  label = "tab:regression_re75_re74_education_black_hispanic_age",
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.projrsquared = FALSE,
  float.pos = "h")

## Calculate the highest leverage observations
H <- leverage_matrix(data, rhs_long)
h <- diag(H)
data[, leverage_multivariate := h]
highest_leverage_data <- data[order(leverage_multivariate, decreasing = TRUE)][1:5, .(re75, re74, education, black, hispanic, age, leverage_multivariate)]
highest_leverage_data[, high_leverage := leverage_multivariate > 3*K/n]
print(xtable::xtable(highest_leverage_data[, .(re75, re74, leverage_multivariate, high_leverage)], caption = "Top 5 observations with the highest leverage(multiple regressors)", label = "tab:highest_leverage_re75_re74_education_black_hispanic_age", digits = 4, size = "small"), file = file.path(output_path, "highest_leverage_re75_re74_education_black_hispanic_age.tex"))
rm(highest_leverage_data)

# Problem 3 ----
data[, high_leverage := leverage_multivariate > 3*K/n]
data[, standardized_residuals := l$residuals/sd(l$residuals)]

ggplot(data, aes(x= standardized_residuals)) + 
  geom_histogram() +
  labs(x="standardized_residuals", y="Count")
ggsave(file.path(output_path, "histogram_standardized_residuals_problem_3.png"), width=8, height=6)

ggplot(data, aes(x=abs(standardized_residuals), y=leverage_multivariate)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x="standardized_residuals", y="leverage_multivariate")
ggsave(file.path(output_path, "scatterplot_standardized_residuals_leverage_multivariate.png"), width=8, height=6)

highest_standardized_residuals <- data[order(standardized_residuals, decreasing = TRUE)][1:5, .(id, standardized_residuals, leverage_multivariate, high_leverage)]
print(xtable::xtable(highest_standardized_residuals, caption = "Top 5 observations with the highest standardized residuals", label = "tab:highest_standardized_residuals", digits = 4, size = "small"), file = file.path(output_path, "highest_standardized_residuals.tex"))
rm(highest_standardized_residuals)

data[order(leverage_multivariate, decreasing = TRUE)][1:5, .(id, standardized_residuals, leverage_multivariate, high_leverage)]
highest_leverage_multivariate <- data[order(leverage_multivariate, decreasing = TRUE)][1:5, .(id, standardized_residuals, leverage_multivariate, high_leverage)]
print(xtable::xtable(highest_leverage_multivariate, caption = "Top 5 observations with the highest leverage", label = "tab:highest_leverage_multivariate", digits = 4, size = "small"), file = file.path(output_path, "highest_leverage_multivariate.tex"))
rm(highest_leverage_multivariate)

# Problem 4 ----
## 10-fold cross-validation
k_folds <- 10
data$fold <- sample(1:k_folds, n, replace = TRUE)
all_predictions_p4 <- data.table()

for (k in 1:k_folds) {
  train_data <- data[fold != k]
  test_data <- data[fold == k]
  
  ## Fit the model
  l <- fixest::feols(fml = as.formula(paste0("re78 ~ ", rhs_short)), data = train_data, vcov = "hetero")
  
  predictions <- predict(l, newdata = test_data)
  
  temp_data <- data.table(id = test_data$id, re78 = test_data$re78, predicted_re78 = predictions)
  all_predictions_p4 <- rbind(all_predictions_p4, temp_data)
}

all_predictions_p4[, error := re78 - predicted_re78]
RMSE_p4 <- sqrt(mean(all_predictions_p4$error^2))

cat_to_file("RMSE for Problem 4: ", RMSE_p4, "\n")

ggplot(all_predictions_p4, aes(x=predicted_re78, y=error)) + 
  geom_point() +
  labs(x="Predicted re78", y="Prediction Error", title = paste0("Problem 4: Predicted vs. Error (RMSE: ", round(RMSE_p4, 4), ")")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(output_path, "scatterplot_predicted_re78_error_p4_cv.png"), width=8, height=6)


# Problem 5 ----
all_predictions_p5 <- data.table()

for (k in 1:k_folds) {
  train_data <- data[fold != k]
  test_data <- data[fold == k]
  
  rhs <- "re75 + re74 + education + black + hispanic + age"
  
  ## Fit the model
  l <- fixest::feols(fml = as.formula(paste0("re78 ~ ", rhs)), data = train_data, vcov = "hetero")
  
  predictions <- predict(l, newdata = test_data)
  
  temp_data <- data.table(id = test_data$id, re78 = test_data$re78, predicted_re78 = predictions)
  all_predictions_p5 <- rbind(all_predictions_p5, temp_data)
}

all_predictions_p5[, error := re78 - predicted_re78]
RMSE_p5 <- sqrt(mean(all_predictions_p5$error^2))

cat_to_file("RMSE for Problem 5: ", RMSE_p5, "\n")

ggplot(all_predictions_p5, aes(x=predicted_re78, y=error)) + 
  geom_point() +
  labs(x="Predicted re78", y="Prediction Error", title = paste0("Problem 5: Predicted vs. Error (RMSE: ", round(RMSE_p5, 4), ")")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(output_path, "scatterplot_predicted_re78_error_p5_cv.png"), width=8, height=6)
