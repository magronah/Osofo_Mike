# Load the package
library(nnet)

# Simulate some data
set.seed(123)

n <- 200  # number of observations

data <- data.frame(
  Gender     = factor(sample(c("Male", "Female"), n, replace = TRUE)),
  Education  = factor(sample(c("HighSchool", "University"), n, replace = TRUE)),
  Region     = factor(sample(c("North", "South"), n, replace = TRUE)),
  Employment = factor(sample(c("Employed", "Unemployed"), n, replace = TRUE)),
  Transport  = factor(sample(c("Car", "Bus", "Bike"), n, replace = TRUE))
)

# Check structure
str(data)

# Fit multinomial logistic regression model
# Note: multinom() automatically uses the first level as the baseline
model <- multinom(Transport ~ Gender + Education + Region + Employment, data = data)

# Summary of the model
summary(model)

# Get p-values
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- (1 - pnorm(abs(z))) * 2
print(p)
