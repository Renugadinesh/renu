
library(gridExtra)
library(dplyr)
dat = read.table("Exercises/Dataset_BIOS/tephritis.txt", header=T)
dat$Patry = as.factor(dat$Patry)
dat$Hostplant = as.factor(dat$Hostplant)
dat$Sex = as.factor(dat$Sex)
dat$Baltic = as.factor(dat$Baltic)
head(dat)

mean_BL_matrix <- tapply(dat$BL, list(dat$Hostplant, dat$Patry), mean, na.rm = TRUE)

# Print the BL matrix
print(mean_BL_matrix)

# Create a matrix of mean OL, excluding NA values
mean_OL_matrix <- tapply(dat$OL, list(dat$Hostplant, dat$Patry), mean, na.rm = TRUE)

# Print the OL matrix
print(mean_OL_matrix)

# Convert matrices to dataframes
mean_BL_df <- as.data.frame(mean_BL_matrix)
mean_OL_df <- as.data.frame(mean_OL_matrix)

# Set row names as a new column
mean_BL_df$Hostplant <- rownames(mean_BL_df)
mean_OL_df$Hostplant <- rownames(mean_OL_df)

# Reshape the data for plotting
library(tidyr)
mean_BL_long <- gather(mean_BL_df, key = "Patry", value = "Mean_BL", -Hostplant)
mean_OL_long <- gather(mean_OL_df, key = "Patry", value = "Mean_OL", -Hostplant)

# Plot the line graphs for BL and OL
library(ggplot2)
plot_BL = ggplot(mean_BL_long, aes(x = Patry, y = Mean_BL, color = Hostplant, group = Hostplant)) +
  geom_line() +
  geom_point() +
  labs(x = "Patry",
       y = "Body Length",
       color = "Hostplant") +
  theme(legend.position = "top", legend.justification = "right")

plot_OL = ggplot(mean_OL_long, aes(x = Patry, y = Mean_OL, color = Hostplant, group = Hostplant)) +
  geom_line() +
  geom_point() +
  labs(x = "Patry",
       y = "Ovipositor Length",
       color = "Hostplant") +
  theme(legend.position = "top", legend.justification = "right")

# Display both plots side by side
grid.arrange(plot_BL, plot_OL, ncol = 2)

