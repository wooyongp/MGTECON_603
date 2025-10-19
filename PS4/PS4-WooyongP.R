# Setting up ----

## packages ----
library(here)
library(tidyverse)
library(data.table)
library(knitr)
library(kableExtra)
# library(patchwork)
library(GGally)
library(parallel)
library(foreach)
library(doParallel)
library(xtable)
library(fixest)
library(ggcorrplot)
library(texreg)

## setting path ----
setwd(here::here("PS4"))
output_path <- here::here("PS4", "output"); if(!dir.exists(output_path)) dir.create(output_path)
log_file <- here::here("PS4", "output", "log.txt")


## create log file if it doesn't exist
if(!file.exists(log_file)) {
  file.create(log_file)
} else {
  file.remove(log_file)
  file.create(log_file)
}


## Parallelization - Use more cores ----
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

## Additional Options ----
theme_set(theme_minimal())
stars = c(0.01, 0.05, 0.1)


## data ----
data <- data.table::fread(file.path("jive.txt"))


## We subset education, lwage, and yob
data <- data[, .(educ, lwage, yob)]

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

## Construct the age variable as 1990 minus year of birth
data[, age := 1990 - yob]

## Construct the wage variable as exp(lwage)
data[, wage := exp(lwage)]

## Report the mean and standard deviation of both

### age
mean_age <- mean(data$age)
sd_age <- sd(data$age)

### wage
mean_wage <- mean(data$wage)
sd_wage <- sd(data$wage)

cat_to_file("mean_age: ", mean_age, "\n")
cat_to_file("sd_age: ", sd_age, "\n")
cat_to_file("mean_wage: ", mean_wage, "\n")
cat_to_file("sd_wage: ", sd_wage, "\n")

# Problem 2 ----

## Regress wage on years of education
model <- feols(wage ~ educ, data = data)
summary(model)

intercept_estimate <- model$coefficients[1]
educ_estimate <- model$coefficients[2]

## The question said that the purpose of this analysis is to investigate the properties of homoskedastic and heteroskedastic models.
## So let's try a hetero-robust SE as well

model_hetero <- fixest::feols(wage ~ educ, data = data, vcov = "hetero")
summary(model_hetero)

screenreg(list(model, model_hetero), stars = stars, digits = 4, custom.note = "The table shows two regression models: one with homoskedastic standard errors and one with Eicker-Huber-White robust standard errors.", caption = "Regression of wage on years of education", custom.model.names = c("Homoskedastic", "White-Robust"),
  include.rsquared = TRUE,
  include.adjrs = TRUE,
  include.projrsquared = FALSE)

texreg(list(model, model_hetero), 
  file = file.path(output_path, "problem_2_model.tex"), 
  stars = stars, 
  digits = 4, 
  custom.model.names = c("Homoskedastic", "White-Robust"),
  caption = "Regression of wage on years of education", 
  custom.note = "\\begin{minipage}{0.9\\linewidth}\\scriptsize\\textit{Notes:} 
  The table shows two regression models: one with homoskedastic standard errors 
  and one with Eicker–Huber–White robust standard errors.\\end{minipage}",
  label = "tab:problem_2_model",
  include.projrsq = FALSE,
  include.projadjrsq = FALSE,
  float.pos = "h")



# Problem 3 ----

## Compute the residuals
residuals <- model$residuals

## Compute the squared residuals
squared_residuals <- residuals^2

## Calculate the average squared residual for all 21 values of years of education

### Attach residuals to the data
data[, residuals := residuals]
data[, squared_residuals := squared_residuals]

### Calculate the average squared residual for all 21 values of years of education
avg_squared_residual <- data[, .(avg_squared_residual = mean(squared_residuals)), by = educ]
avg_squared_residual

kable(avg_squared_residual, format = "latex", booktabs = TRUE, digits = 4, file = file.path(output_path, "avg_squared_residuals.tex"), caption = "Average Squared Residuals by Years of Education")

### Plot the average squared residuals vs. years of education
ggplot(avg_squared_residual, aes(x = educ, y = avg_squared_residual)) +
  geom_line() +
  geom_point(color='red', size = 3) +
  labs(title = "Average Squared Residuals vs. Years of Education", x = "Years of Education", y = "Average Squared Residuals")
ggsave(file.path(output_path, "avg_squared_residuals_vs_educ.png"), width=8, height=6)


### Create a scatterplot of the residuals against the years of education
ggplot(data, aes(x = educ, y = residuals)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Residuals vs. Years of Education", x = "Years of Education", y = "Residuals")
ggsave(file.path(output_path, "residuals_vs_educ.png"), width=8, height=6)


### Create a scatterplot of the squared residuals against the years of education

ggplot(data, aes(x = educ, y = squared_residuals)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Squared Residuals vs. Years of Education", x = "Years of Education", y = "Squared Residuals")
ggsave(file.path(output_path, "squared_residuals_vs_educ.png"), width=8, height=6)


# Problem 4 ----

# Problem 5 ----
## Draw random sample
model_20_tibble <- tibble(estimate = numeric(), lb = numeric(), ub = numeric())
model_20_hetero_tibble <- tibble(estimate = numeric(), lb = numeric(), ub = numeric())
replacement <- TRUE
for(i in 1:B){
  DT_20 <- data[sample(1:nrow(data), size = 20, replace = replacement)]
  model_20 <- feols(wage ~ educ, data = DT_20)
  model_20_hetero <- fixest::feols(wage ~ educ, data = DT_20, vcov = "hetero")
  model_20_tibble <- add_row(model_20_tibble, estimate = model_20$coefficients[2], lb = confint(model_20)[2,1], ub = confint(model_20)[2,2])
  model_20_hetero_tibble <- add_row(model_20_hetero_tibble, estimate = model_20_hetero$coefficients[2], lb = confint(model_20_hetero)[2,1], ub = confint(model_20_hetero)[2,2])
}

coverage_model_20 <- model_20_tibble |> mutate(in_confint  = as.integer(lb < educ_estimate & ub > educ_estimate)) |> summarise(coverage = mean(in_confint)) |> as.numeric()
coverage_model_20_hetero <- model_20_hetero_tibble |> mutate(in_confint  = as.integer(lb < educ_estimate & ub > educ_estimate)) |> summarise(coverage =mean(in_confint)) |> as.numeric()

cat_to_file("coverage_model_20: ", coverage_model_20, "\n")
cat_to_file("coverage_model_20_hetero: ", coverage_model_20_hetero, "\n") 

# Problem 6 ----

## Same Exercise with size 200
model_200_tibble <- tibble(estimate = numeric(), lb = numeric(), ub = numeric())
model_200_hetero_tibble <- tibble(estimate = numeric(), lb = numeric(), ub = numeric())
replacement <- TRUE
for(i in 1:B){
  DT_200 <- data[sample(1:nrow(data), size = 200, replace = replacement)]
  model_200 <- feols(wage ~ educ, data = DT_200)
  model_200_hetero <- fixest::feols(wage ~ educ, data = DT_200, vcov = "hetero")
  model_200_tibble <- add_row(model_200_tibble, estimate = model_200$coefficients[2], lb = confint(model_200)[2,1], ub = confint(model_200)[2,2])
  model_200_hetero_tibble <- add_row(model_200_hetero_tibble, estimate = model_200_hetero$coefficients[2], lb = confint(model_200_hetero)[2,1], ub = confint(model_200_hetero)[2,2])
}

coverage_model_200 <- model_200_tibble |> mutate(in_confint  = as.integer(lb < educ_estimate & ub > educ_estimate)) |> summarise(coverage = mean(in_confint)) |> as.numeric()
coverage_model_200_hetero <- model_200_hetero_tibble |> mutate(in_confint  = as.integer(lb < educ_estimate & ub > educ_estimate)) |> summarise(coverage =mean(in_confint)) |> as.numeric()

coverage_model_200
coverage_model_200_hetero

cat_to_file("coverage_model_200: ", coverage_model_200, "\n")
cat_to_file("coverage_model_200_hetero: ", coverage_model_200_hetero, "\n") 

## Same Exercise with size 2000
model_2000_tibble <- tibble(estimate = numeric(), lb = numeric(), ub = numeric())
model_2000_hetero_tibble <- tibble(estimate = numeric(), lb = numeric(), ub = numeric())
replacement <- TRUE
for(i in 1:B){
  DT_2000 <- data[sample(1:nrow(data), size = 2000, replace = replacement)]
  model_2000 <- feols(wage ~ educ, data = DT_2000)
  model_2000_hetero <- fixest::feols(wage ~ educ, data = DT_2000, vcov = "hetero")
  model_2000_tibble <- add_row(model_2000_tibble, estimate = model_2000$coefficients[2], lb = confint(model_2000)[2,1], ub = confint(model_2000)[2,2])
  model_2000_hetero_tibble <- add_row(model_2000_hetero_tibble, estimate = model_2000_hetero$coefficients[2], lb = confint(model_2000_hetero)[2,1], ub = confint(model_2000_hetero)[2,2])
}

coverage_model_2000 <- model_2000_tibble |> mutate(in_confint  = as.integer(lb < educ_estimate & ub > educ_estimate)) |> summarise(coverage = mean(in_confint)) |> as.numeric()
coverage_model_2000_hetero <- model_2000_hetero_tibble |> mutate(in_confint  = as.integer(lb < educ_estimate & ub > educ_estimate)) |> summarise(coverage =mean(in_confint)) |> as.numeric()

coverage_model_2000
coverage_model_2000_hetero

cat_to_file("coverage_model_2000: ", coverage_model_2000, "\n")
cat_to_file("coverage_model_2000_hetero: ", coverage_model_2000_hetero, "\n") 
