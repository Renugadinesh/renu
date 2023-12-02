library(tidyr)
library(ggplot2)

dat = read.table("Exercises/Dataset_BIOS/tephritis.txt", header=T)
dat$Patry = as.factor(dat$Patry)
dat$Hostplant = as.factor(dat$Hostplant)
dat$Sex = as.factor(dat$Sex)
dat$Baltic = as.factor(dat$Baltic)
head(dat)

###### BOX PLOT - Wing length #####

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

###### BOX PLOT - OL ########

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


###### MODEL SELECTION (OL) ########

m11 = lm(OL ~ Patry * Hostplant, data = dat)
m12 = lm(OL ~ Patry + Hostplant, data = dat)
m13 = lm(OL ~ Wing_length + Hostplant, data = dat)

mlist = list(m11, m12, m13)
AICTab = AIC(m11, m12, m13)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab

###### MODEL SELECTION (Wing Length) ########

m31 = lm(Wing_length ~ Patry, data = dat)
m32 = lm(Wing_length ~ Patry + Hostplant, data = dat)
m33 = lm(Wing_length ~ Patry * Hostplant, data = dat)
m34 = lm(Wing_length ~ Patry + Sex, data = dat)
m35 = lm(Wing_length ~ Patry * Sex, data = dat)


mlist = list(m31, m32, m33, m34, m35)
AICTab = AIC(m31, m32, m33, m34, m35)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab


###### ANOVA ANALYSIS #########

allopatry_dat <- dat[dat$Patry == "Allopatry", ]
sympatry_dat <- dat[dat$Patry == "Sympatry", ]

Hetero_dat <- dat[dat$Hostplant == "Heterophyllum", ]
Olera_dat <- dat[dat$Hostplant == "Oleraceum", ]

male_dat <- dat[dat$Sex == "Male", ]
Female_dat <- dat[dat$Sex == "Female", ]

# Fit the model for OL in the Allopatry condition
ol_allopatry = lm(OL ~ Hostplant, data = na.omit(allopatry_dat))
summary(ol_allopatry)
anova(ol_allopatry)

# Fit the model for OL in the Sympatry condition
ol_sympatry = lm(OL ~ Hostplant, data = na.omit(sympatry_dat))
summary(ol_sympatry)
anova(ol_sympatry)

# Fit the model for Wing_length in the Allopatry condition
wl_allopatry = lm(Wing_length ~ Sex, data = allopatry_dat)
summary(wl_allopatry)
anova(wl_allopatry)

# Fit the model for OL in the Sympatry condition
wl_sympatry = lm(Wing_length ~ Sex, data = sympatry_dat)
summary(wl_sympatry)
anova(wl_sympatry)


############  OL PLOT - Hostplant/Patry ####################

# Create a matrix of mean OL, excluding NA values

mean_OL_matrix <- tapply(dat$OL, list(dat$Hostplant, dat$Patry), mean, na.rm = TRUE)

# Print the matrix
print(mean_OL_matrix)

mean_OL_df <- as.data.frame(mean_OL_matrix)

# Set row names as a new column 
mean_OL_df$Hostplant <- rownames(mean_OL_df)

# Reshape the data for plotting
mean_OL_long <- gather(mean_OL_df, key = "Patry", value = "Mean_OL", -Hostplant)

# Plot the line graph
plot1 = ggplot(mean_OL_long, aes(x = Patry, y = Mean_OL, color = Hostplant, group = Hostplant)) +
  geom_line() +
  geom_point() +
  labs(x = "Patry",
       y = "Ovipositor length (mm)",
       color = "Hostplant")

 plot1 + theme(
   legend.position = c(0.6, 0.9),  # Adjust the values to place the legend
   legend.justification = c(0,0.8), aspect.ratio = 1
 )

 #### Fit the model for OL in the Hostplant and patry condition ####

 OL_patry = lm(OL ~ Patry * Hostplant, data = dat)
 summary(OL_patry)
 anova(OL_patry)
 
 ######### Wing length plot - Hostplant/sex########
 mean_WL_matrix = tapply(dat$Wing_length, list(dat$Hostplant, dat$Sex), mean, na.rm = TRUE)
 
 # Print the matrix
 print(mean_WL_matrix)
 
 mean_WL_df = as.data.frame(mean_WL_matrix)
 
 # Set row names as a new column 
 mean_WL_df$Hostplant = rownames(mean_WL_df)
 
 # Reshape the data for plotting
 mean_WL_long = gather(mean_WL_df, key = "Sex", value = "Mean_WL", -Hostplant)
 
 # Plot the line graph
 plot2 = ggplot(mean_WL_long, aes(x = Sex, y = Mean_WL, color = Hostplant, group = Hostplant)) +
   geom_line() +
   geom_point() +
   labs(x = "Sex",
        y = "Wing length (mm)",
        color = "Hostplant")
 
 plot2 + theme(
   legend.position = c(0.6, 0.9),  # Adjust the values to place the legend
   legend.justification = c(0,0.8), aspect.ratio = 1
 )
 ####### Wing length plot - Patry/Hostplant ###########
 mean_WL1_matrix = tapply(dat$Wing_length, list(dat$Hostplant, dat$Patry), mean)
 
 # Print the matrix
 print(mean_WL1_matrix)
 
 mean_WL1_df = as.data.frame(mean_WL1_matrix)
 
 # Set row names as a new column 
 mean_WL1_df$Hostplant = rownames(mean_WL1_df)
 
 # Reshape the data for plotting
 mean_WL1_long = gather(mean_WL1_df, key = "Patry", value = "Mean_WL1", -Hostplant)
 
 # Plot the line graph
 
 plot3 = ggplot(mean_WL1_long, aes(x = Patry, y = Mean_WL1, color = Hostplant, group = Hostplant)) +
   geom_line() +
   geom_point() +
   labs(x = "Patry",
        y = "Wing length (mm)",
        color = "Hostplant")
 plot3 + theme(legend.position = c(0.6, 0.9),
               legend.justification = c(0,0.8), aspect.ratio = 1)
 
 wL_patry = lm(Wing_length ~ Patry * Hostplant, data = dat)
 summary(wL_patry)
 anova(wL_patry)
 ############################################################
 
