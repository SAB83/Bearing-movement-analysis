# Bearing-movement-analysis
Calculating bearing of movement of deployed motus tags on juveniles and adults of migratory birds and plotting their movements and their flight distance
data <- read.csv("/home/baniasss/164/movement/data.csv")

library(dplyr)
library(CircStats)
library(circular)
library(plotrix)

# Data Preprocessing
data <- data %>%
  select(-starts_with("X")) %>%              # Remove columns that start with "X"
  mutate(sex = recode(sex, "M" = "Adult", "F" = "Adult", "U" = "Juvenile")) %>%
  filter(!is.na(V))                          # Filter out missing values in column V

# Select data for site 1 or site 2
data_site1 <- filter(data, site == 1)
data_site2 <- filter(data, site == 2)

# Perform Circular ANOVA for site 1
group_site1 <- data_site1$sex
x_site1 <- data_site1$V

# Perform circular ANOVA (F-test)
aov_result_site1 <- aov.circular(x_site1, group_site1, method = "F.test", F.mod = TRUE)
print(aov_result_site1)

# Perform circular ANOVA (LRT - Likelihood Ratio Test)
aov_result_LRT_site1 <- aov.circular(x_site1, group_site1, method = "LRT")
print(aov_result_LRT_site1)

# Mean Resultant Length and Mean Direction for circular data
mean_rho_site1 <- rho.circular(x_site1, na.rm = TRUE)
mean_direction_site1 <- mean(circular(x_site1, type = "angles", units = "degrees"))
cat("Mean Resultant Length:", mean_rho_site1, "\n")
cat("Mean Direction:", mean_direction_site1, "\n")

# Watson's Two-Sample Test (comparing site 1 and site 2 data)
x_site2 <- data_site2$V
watson_test <- watson.two(x_site1, x_site2, alpha = 0.05, plot = TRUE)

# Visualization - Polar Plots
# Polar plot for adults at site 1
testlen_adults1 <- rep(1, length(x_site1))  # Example values for lengths
testpos_adults1 <- x_site1                  # Positions based on x values

par(cex.axis = 1.5, font.axis = 2)
polar.plot(testlen_adults1, testpos_adults1, radial.lim = c(0, 1), clockwise = TRUE, 
           start = 90, line.col = rainbow(length(x_site1)), boxed.radial = FALSE, 
           show.grid.labels = 0, lwd = 3)

# Polar plot for juveniles at site 1
juvenile_data <- filter(data_site1, sex == "Juvenile")
testlen_juveniles1 <- rep(1, nrow(juvenile_data))
testpos_juveniles1 <- juvenile_data$V

polar.plot(testlen_juveniles1, testpos_juveniles1, radial.lim = c(0, 8), clockwise = TRUE, 
           start = 90, line.col = rainbow(nrow(juvenile_data)), boxed.radial = FALSE, 
           show.grid.labels = 0, lwd = 3)

# Polar plot for adults at site 2
testlen_adults2 <- rep(1, length(x_site2))  # Example values for lengths
testpos_adults2 <- x_site2                  # Positions based on x values

polar.plot(testlen_adults2, testpos_adults2, radial.lim = c(0, 1), clockwise = TRUE, 
           start = 90, line.col = rainbow(length(x_site2)), boxed.radial = FALSE, 
           show.grid.labels = 0, lwd = 3)

