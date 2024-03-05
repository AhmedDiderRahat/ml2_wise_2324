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
