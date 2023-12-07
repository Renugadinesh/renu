library(ggplot2)
library(tidyr)
library(patchwork)

dat = read.table("Exercises/Dataset_BIOS/tephritis.txt", header=T)
dat$Patry = as.factor(dat$Patry)
dat$Hostplant = as.factor(dat$Hostplant)
dat$Sex = as.factor(dat$Sex)
dat$Baltic = as.factor(dat$Baltic)
head(dat)

filtered_dat_wl = dat[complete.cases(dat$Wing_length), ]

# Create a box plot for WL for Heterophyllum and Oleraceum with sex as grouping variable
box_plot_wl = ggplot(filtered_dat_wl, aes(x = Hostplant, y = Wing_length, fill = Sex)) +
  geom_boxplot(width = 0.7, position = position_dodge(width = 0.8)) +
  labs(x = "Hostplant", y = "Wing Length (mm)") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +  # Adjust colors as needed
  theme_minimal() +
  theme(aspect.ratio = 1, legend.box.background = element_rect(color = "black", linewidth = 0.5))

# Display the box plot
print(box_plot_wl)

########################

# Filter out rows with NA in OL
filtered_dat = dat[complete.cases(dat$OL), ]

box_plot_ol = ggplot(filtered_dat, aes(x = Hostplant, y = OL, fill = Patry)) +
  geom_boxplot(width = 0.7, position = position_dodge(width = 0.8)) +
  labs(x = "Hostplant", y = "Ovipositor Length (mm)") +
  scale_fill_manual(values = c("Allopatry" = "blue", "Sympatry" = "red")) +  # Adjust colors as needed
  theme_minimal() +
  theme(aspect.ratio = 1, legend.text = element_text(size = 8), legend.box.background = element_rect(color = "black", linewidth = 0.5))
  
# Display the box plot
print(box_plot_ol)


########################

lm_model <- lm(OL ~ BL + Hostplant, data = filtered_dat)
summary(lm_model)

# Extract residuals from the model
residuals <- residuals(lm_model)

# Create a data frame with residuals
residual_df <- data.frame(Residuals = residuals)

# Create a histogram plot for residuals
hist_plot <- ggplot(residual_df, aes(x = Residuals)) +
  geom_histogram(fill = "green", alpha = 0.5, bins = 30) +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals") +
  theme_minimal()

# Display the histogram plot
print(hist_plot)
