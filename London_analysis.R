########################################################
# London Real Estate Analysis
# Regression and Visualization of House Prices
# Author: Your Name
# Date: 2025-09-16
########################################################

# -------------------------
# 1. Setup
# -------------------------
# Install and load required packages
required_packages <- c("ggplot2", "scales")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(ggplot2)
library(scales)

# -------------------------
# 2. Load Data
# -------------------------
# Replace with relative path or instructions to download data
data_path <- "data/London.csv"  # Make sure your CSV is in a 'data/' folder
data <- read.csv(data_path)

# Quick overview
head(data)
str(data)
summary(data)

# -------------------------
# 3. Simple Linear Regression
# -------------------------
model_simple <- lm(Price ~ Area.in.sq.ft, data = data)
summary(model_simple)

# Plot simple regression
plot_simple <- ggplot(data, aes(x=Area.in.sq.ft, y=Price)) +
  geom_point(alpha=0.6, color="blue") +
  geom_smooth(method="lm", color="red", se=FALSE) +
  scale_y_continuous(labels=comma) +
  labs(title="Price vs Area", x="Area (sq ft)", y="Price (£)") +
  theme_minimal()

# Save plot
ggsave("figures/price_vs_area.png", plot_simple, width=7, height=5)

# -------------------------
# 4. Multiple Regression
# -------------------------
# Including relevant numeric and categorical predictors
model_multiple <- lm(Price ~ Area.in.sq.ft + No..of.Bedrooms + 
                       House.Type + Location, data = data)
summary(model_multiple)

# Add predicted prices
data$Predicted_Price <- predict(model_multiple, newdata=data)

# -------------------------
# 5. Visualizations
# -------------------------

# Price vs Bedrooms by House Type
plot_bedrooms <- ggplot(data, aes(x=No..of.Bedrooms, y=Price, color=House.Type)) +
  geom_point(alpha=0.6) +
  scale_y_continuous(labels=comma) +
  labs(title="Price vs Bedrooms by House Type", x="Number of Bedrooms", y="Price (£)") +
  theme_minimal()
ggsave("figures/price_vs_bedrooms.png", plot_bedrooms, width=7, height=5)

# Top Locations by Price
top_locations <- c("Chelsea", "Knightsbridge", "Mayfair", "Marylebone", "St James's", 
                   "Westminster", "Kensington", "Shad Thames", "City Of London", "Paddington")

plot_top_locations <- ggplot(subset(data, Location %in% top_locations), aes(x=Location, y=Price)) +
  geom_boxplot(fill="skyblue") +
  scale_y_continuous(labels=comma) +
  labs(title="Price Distribution in Top Locations", x="Location", y="Price (£)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("figures/top_locations.png", plot_top_locations, width=8, height=5)

# Effect of number of bedrooms
plot_bedrooms_effect <- ggplot(data, aes(x=No..of.Bedrooms, y=Price)) +
  geom_point(alpha=0.4, color="purple") +
  geom_smooth(method="lm", color="orange", se=FALSE) +
  scale_y_continuous(labels=comma) +
  labs(title="Effect of Bedrooms on Price (Partial Effect)", x="Number of Bedrooms", y="Price (£)") +
  theme_minimal()
ggsave("figures/bedrooms_effect.png", plot_bedrooms_effect, width=7, height=5)

# Predicted vs Actual Prices
plot_predicted <- ggplot(data, aes(x=Predicted_Price, y=Price)) +
  geom_point(alpha=0.5, color="darkgreen") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels=comma) +
  labs(title="Predicted vs Actual Prices", x="Predicted Price (£)", y="Actual Price (£)") +
  theme_minimal()
ggsave("figures/predicted_vs_actual.png", plot_predicted, width=7, height=5)

# -------------------------
# 6. Summary of Findings
# -------------------------
cat("Simple regression: Price = ", round(coef(model_simple)[1],0),
    "+", round(coef(model_simple)[2],0), "* Area (sq ft)\n")
cat("R-squared:", round(summary(model_simple)$r.squared, 3), "\n\n")

cat("Multiple regression:\n")
cat("Multiple R-squared:", round(summary(model_multiple)$r.squared, 3), "\n")
cat("Adjusted R-squared:", round(summary(model_multiple)$adj.r.squared, 3), "\n")
cat("Interpretation: Area and Location are key predictors; prioritize large apartments in top neighborhoods.\n")
