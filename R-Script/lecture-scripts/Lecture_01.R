###############################################################################################
# Lecture: 01 (Dealing with Missing Data)                                                     #
# Email: adrahat.bht@gmail.com                                                                #
###############################################################################################


# Create a list with the given data
data <- c(3, 1, 0, 2, 0, 2, NA, 2, NA, 1, 5, NA, 0, 3, 4, NA, 0, 4, 4, NA)

# Print the list to check
print(data)



###############################################################################################
#Uni-variate Imputation-1: Mean Replacement


# calculate the mean
mean_d <- mean(data, na.rm = TRUE)

# copy the whole data
new_data <- data

# replace mean with null
new_data[is.na(new_data)] <- mean_d
new_data

# Create a density plot
plot(density(new_data), main = "Density Plot of Data Vector", xlab = "Data Values", col = "red")

# Plot the density of v1
plot(density(na.omit(data)),
     main = "Density Plot of Two Vectors",
     xlab = "Data Values",
     col = "red", 
     ylim = range(c(density(na.omit(data))$y, density(new_data)$y)))

lines(density(new_data), col = "blue")

# Add a legend
legend("topright", legend = c("original", "imputed"), col = c("red", "blue"), lwd = 2)

mean(data, na.rm = TRUE)
var(data, na.rm = TRUE)

mean(new_data, na.rm = TRUE)
var(new_data, na.rm = TRUE)

###############################################################################################

#Uni-variate Imputation-2: Mean/Variance Simulation

# Calculate mean and variance
mean_d <- mean(data, na.rm = TRUE)
var_d <- var(data, na.rm = TRUE)

# get number of null values
n <- sum(is.na(data))
n

# Compute the imputed values
imputed_values <- rnorm(n, mean = mean_d, sd = sqrt(var_d))
imputed_values

# Store in the original list
new_data <- data
new_data[is.na(new_data)] <- imputed_values
new_data

# Create a density plot
plot(density(new_data), main = "Density Plot of Data Vector", xlab = "Data Values", col = "red")

# Plot the density of v1
plot(density(na.omit(data)),
     main = "Density Plot of Two Vectors",
     xlab = "Data Values",
     col = "red", 
     ylim = range(c(density(na.omit(data))$y, density(new_data)$y)))

lines(density(new_data), col = "blue")

# Add a legend
legend("topright", legend = c("original", "imputed"), col = c("red", "blue"), lwd = 2)



###############################################################################################

#Uni-variate Imputation-3: Direct Random Sampling

# all non-null data
non_missing_data <- data[!is.na(data)]

# all null data
missing_data <- data[is.na(data)]
missing_data

# imputed data
imputed_values <- sample(non_missing_data, length(missing_data), replace = TRUE)
imputed_values

# Store in the original list
new_data <- data
new_data[is.na(new_data)] <- imputed_values
new_data

# Create a density plot
plot(density(new_data), main = "Density Plot of Data Vector", xlab = "Data Values", col = "red")

# Plot the density of v1
plot(density(na.omit(data)),
     main = "Density Plot of Two Vectors",
     xlab = "Data Values",
     col = "red", 
     ylim = range(c(density(na.omit(data))$y, density(new_data)$y)))

lines(density(new_data), col = "blue")

# Add a legend
legend("topright", legend = c("original", "imputed"), col = c("red", "blue"), lwd = 2)




###############################################################################################

# Bi-variate Imputation
# Data set creation

# Set seed for reproducibility
set.seed(123)

# Create synthetic data

# Number of observations
n <- 100 

# Normally distributed ages
Age <- rnorm(n, mean = 50, sd = 10)

# Normally distributed BMI
BMI <- rnorm(n, mean = 25, sd = 5)  
BloodPressure <- 120 + 0.3 * Age - 0.25 * BMI + rnorm(n, mean = 0, sd = 15)

# Pulse with some relation to Age and BMI
Pulse <- 70 + 0.5 * Age - 0.2 * BMI + rnorm(n, mean = 0, sd = 10)

# Introduce NA values randomly in BMI and Pulse
set.seed(321)

# Randomly choose 20 indices for Pulse
missing_indices_Pulse <- sample(1:n, 20) 

# sort the random indices
missing_indices_Pulse <- sort(unlist(missing_indices_Pulse))
missing_indices_Pulse

# Preserve original data for comparison
original_missing_pulse <- Pulse[missing_indices_Pulse]
original_missing_pulse

# Assign NA to these indices in Pulse
Pulse[missing_indices_Pulse] <- NA 

# Combine into a data frame
data_df <- data.frame(Age, BMI, BloodPressure, Pulse)

# View the first few rows of the data set
head(data_df)

summary(data_df)

#Bi-variate Imputation-1: Multiple Regression Model

# Fit the model using complete cases
fit <- lm(Pulse ~ Age + BMI + BloodPressure, 
          data = data_df, 
          subset = !is.na(data_df$Pulse))

predicted_values <- predict(fit, newdata = data_df[missing_indices_Pulse, ])

# Add an error term to the predicted values
residual_sd <- sd(residuals(fit))  # Standard deviation of the model's residuals
error_term <- rnorm(length(predicted_values), mean = 0, sd = residual_sd)
imputed_values <- predicted_values + error_term

# copy the original pulse data
imputed_pulse <- Pulse

# add the imputed ones
imputed_pulse[missing_indices_Pulse] <- imputed_values

# original data
Pulse[missing_indices_Pulse] <- original_missing_pulse

# Plot the density of v1
plot(density(na.omit(Pulse)),
     main = "Density Plot of Two Vectors",
     xlab = "Data Values",
     col = "red", 
     ylim = range(c(density(na.omit(Pulse))$y, density(imputed_pulse)$y)))

lines(density(imputed_pulse), col = "blue")

# Add a legend
legend("topright", legend = c("original", "imputed"), col = c("red", "blue"), lwd = 2)

