mean_WL_matrix <- tapply(dat$Wing_length, list(dat$Hostplant, dat$Patry), mean)

# Print the matrix
print(mean_WL_matrix)

mean_WL_df <- as.data.frame(mean_WL_matrix)

# Set row names as a new column 
mean_WL_df$Hostplant <- rownames(mean_WL_df)

# Reshape the data for plotting
mean_WL_long <- gather(mean_WL_df, key = "Patry", value = "Mean_WL", -Hostplant)

# Plot the line graph
library(ggplot2)
plot1 = ggplot(mean_WL_long, aes(x = Patry, y = Mean_WL, color = Hostplant, group = Hostplant)) +
  geom_line() +
  geom_point() +
  labs(x = "Patry",
       y = "Wing length (mm)",
       color = "Hostplant")
plot1 + theme(legend.position = "top",
              legend.justification = "right", aspect.ratio = 1)
plot1 + theme(
  legend.position = c(0.75, 0.9),  # Adjust the values to place the legend
  legend.justification = c(0,0.8)
)

