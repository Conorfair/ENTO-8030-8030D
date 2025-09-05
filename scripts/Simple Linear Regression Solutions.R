# Simple Linear Regression - lm()

# Linear regression is a means to test a relationship between one or more independent variables and a dependent variable that is continuous. Depending on the conditions of the independent variable(s), you may be fitting a model that is colloquially referred to as a "simple" linear regression, a "multiple" linear regression, t-test, ANOVA, or ANCOVA. As to which type of model you are fitting, it is determined by the characteristic and number of the independent variables included.

# We will start with the simplest case, a simple linear regression, which is a model with one continuous dependent variable and one continuous independent variable.

# Load the necessary libraries
library(tidyverse)
library(here)
library(janitor)
library(ggthemes)

# Load the data
data <- read_csv(here("data", "Simple_Linear_Regression.csv")) %>%
  clean_names()
colnames(data)

# Inspect the data
glimpse(data)

# We see that the data contains a categorical variable (loc) and two continuous variables (density and lbs_suc_a). We are focusing on the relationship between density and lbs_suc_a, so we will ignore loc for now.

# Visualize the data
ggplot(data, aes(x = density, y = lbs_suc_a)) +
  geom_point() +
  theme_classic() +
  labs(title = "Scatter plot of Density vs. Pounds of Sugar per Acre",
       x = "Density (plants per acre)",
       y = "Pounds of Sugar per Acre")

# There appears to be a negative linear relationship between density and lbs_suc_a. As density increases, lbs_suc_a also tends to decrease. It is a good idea to visualize the data before fitting a model to get a sense of the nature of the relationship - is the relationship linear or non-linear? Are there any outliers that may unduly influence the model?

# Fit the linear model
model <- lm(lbs_suc_a ~ density, data = data)
summary(model)

# The output of summary(model) provides several pieces of information. 

# The coefficients section shows the estimated intercept and slope of the regression line. 
# The intercept is 8677.6, which is the estimated value of lbs_suc_a when density is 0. 
# The slope is -20644.7, which indicates that for each additional one unit increase in density, lbs_suc_a decreases by an average of 20,644.7 units.
# Does this make sense in the context of the data? Do we capture the scale of a one unit change in the independent variable (density)?

# A 0.1 unit change in density results in a 2064.47 unit decrease in lbs_suc_a. This seems more reasonable given the context of the data.

# The Adjust R-squared value is 0.35, which means that approximately 35% of the variability in lbs_suc_a can be explained by the linear relationship with density. This indicates a poor to moderate fit of the model to the data.
# The F-statistic is 13.37 on 1 and 22 DF with a p-value of 0.0014, which indicates that the model is statistically significant. This means that there is a significant linear relationship between density and lbs_suc_a. More specifically, that this model is a better fit to the data than a model with no independent variables (null model).

# Check the assumptions of linear regression
par(mfrow = c(2, 2)) # changes plot parameters
plot(model)

# The residuals vs. fitted plot shows that the residuals are randomly scattered around 0, which suggests that the linearity assumption is met.
# The normal Q-Q plot shows that the residuals are approximately normally distributed, which suggests that the normality assumption is met.
# The scale-location plot shows that the residuals have constant variance, which suggests that the homoscedasticity assumption is met.
# The residuals vs. leverage plot shows that there are no influential outliers, which satisfies the requirement for lack of outliers or influential observations.

par(mfrow = c(1, 1)) # reverts back to original parameters


# Alternative methods to check assumptions

qqnorm(model$residuals)
qqline(model$residuals)
shapiro.test(model$residuals) # p-value = 0.511, fail to reject null hypothesis of normality - data can be assumed to be normally distributed

plot(residuals(model) ~ fitted(model))
abline(h = 0, col = "red") # residuals appear to be randomly scattered around 0, suggesting homoscedasticity
# Use Breusch-Pagan test for a model with no categorical variables
lmtest::bptest(model) # p-value = 0.716, fail to reject null hypothesis of equal variances - homoscedasticity can be assumed


# Predictions and Final Visualization using base plot function
tiff(here("output", "simple_linear_regression_model.tiff"), width = 6, height = 4, units = 'in', res = 300) # Save the plot as a tiff file in the output folder
par(mfrow = c(1, 1), mar = c(3.2, 3.2, 2, 0.5), mgp = c(2, 0.7, 0)) #Number of rows of figures, the margins of the figure, and margin line for the axis title labels and line
plot(data$lbs_suc_a ~ data$density, bty="l", #Y and X variables for the plot and the "L" box type for the plot area
     ylab = "Sugar yield (lbs/A)", xlab = "Volunteer corn density (plants/ft row)",
     main = "Lingle, WY 2009", ylim = c(0, 10000))
abline(model) # Add the regression line
# to add the regression equation into the plot:
int <- round(summary(model)$coefficients[[1]]) # get intercept
sl <- round(summary(model)$coefficients[[2]]) # get slope
reg.eq <- paste("Yield =", int, sl, "* Density") # create text regression equation
legend("bottomleft",reg.eq, bty = "n")
dev.off() # close the device to save the file in the output folder


# Example with Categorical Independent Variable - t-test
# Now we will fit a model with one continuous dependent variable and one categorical independent variable with only two levels. This is equivalent to a t-test.

# Load the data
data_t_test <- read_csv(here("data", "T-Test_Example.csv")) %>%
  clean_names()
colnames(data_t_test)

# Inspect the data
glimpse(data_t_test)
# We see that the data contains a categorical variable (mulch type) with two levels (light and dark) and one continuous variable (yield).

# Visualize the data
ggplot(data_t_test, aes(x = mulch_type, y = yield)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box plot of Yield by Mulch Type",
       x = "Mulch Type",
       y = "Yield (lbs/A)")
# There appears to be a difference in yield between the two mulch types, with the light mulch type having a higher mean yield than the dark mulch type.

# Fit the linear model (t-test)
model_t_test <- lm(yield ~ mulch_type, data = data_t_test)
summary(model_t_test)

t.test(yield ~ mulch_type, data = data_t_test, var.equal = TRUE) # var.equal = TRUE assumes equal variances between groups

# You can see how the output of the lm() function and the t.test() function are equivalent. The coefficients section of the lm() output shows the estimated mean yield for the dark mulch type (intercept) and the difference in mean yield between the light and dark mulch types (slope). The t-value and p-value from the lm() output are the same as those from the t.test() output.

# Check the assumptions of linear regression (t-test)
par(mfrow = c(2, 2)) # changes plot parameters
plot(model_t_test)
# The residuals vs. fitted plot shows that the residuals are randomly scattered around 0 but that the spread for each category isn't equal, which suggests that the homoscedasticity assumption is violated.
# The normal Q-Q plot shows that the residuals have some long tails but this could be due to the small sample size or heteroscedasticity.
par(mfrow = c(1, 1)) # reverts back to original parameters

# Try t-test without equal variance assumption - Welch's t-test
t.test(yield ~ mulch_type, data = data_t_test, var.equal = FALSE) # var.equal = FALSE does not assume equal variances between groups
# The p-value is still significant, indicating that there is a significant difference in mean yield between the two mulch types.

# Visualization using ggplot function

t_test_plot <- ggplot(data_t_test, aes(x = mulch_type, y = yield)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, color = "blue") + # adds individual data points with some jitter for visibility
  theme_classic() +
  labs(title = "Yield by Mulch Type",
       x = "Mulch Type",
       y = "Yield (lbs/A)") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") + # adds error bars for standard error
  annotate("text", x = 1.5, y = max(data_t_test$yield) - 10, label = "p < 0.05", size = 5) # adds significance annotation
t_test_plot


ggsave(here("output", "t_test_model_plot.tiff"), plot = t_test_plot, width = 6, height = 4, units = 'in', dpi = 300) # Save the plot as a tiff file in the output folder


# Example with Categorical Independent Variable with more than two levels - ANOVA
# Load the data
data_anova <- read_csv(here("data", "FlowerColourVisits.csv")) %>%
  clean_names()
colnames(data_anova)
# Inspect the data
glimpse(data_anova)
# We see that the data contains two categorical variabls (flower and colour) where colour has four levels (red, white, yellow, and orange) and one continuous variable (number of visits).

# Visualize the data
ggplot(data_anova, aes(x = colour, y = number_of_visits)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box plot of Number of Visits by Colour",
       x = "Colour",
       y = "Number of Visits")
# There appears to be a difference in number of visits between the four colours, with white having the highest mean number of visits and red having the lowest mean number of visits.

# Fit the linear model (ANOVA)
model_anova <- lm(number_of_visits ~ colour, data = data_anova)
summary(model_anova)
car::Anova(model_anova, type = "II") # Type II ANOVA table - Type III is used when there are interactions in the model

# The output of summary vs. Anova() function provides different pieces of informaiton.
# The coefficients section of the summary(model_anova) output shows the estimated mean number of visits for the red colour (intercept) and the difference in mean number of visits between each of the other colours and red (slopes). This does not compare all the colours to each other, only to red.

# The F-statistic from the Anova() output is 0.606 on 3 and 46 DF with a p-value of 0.6145, which indicates that there is not a significant difference in mean number of visits between at least two of the four colours. However, this could be obfuscated by the variation among the flower types, so pairwise comparisons can be used to further explore the differences between the colours.

# Check the assumptions of linear regression (ANOVA)
par(mfrow = c(2, 2))
plot(model_anova)
# Assumptions look to be met, although the normal Q-Q plot shows some deviation from normality in the tails.

par(mfrow = c(1, 1))

# Pairwise comparisons
library(emmeans)
library(multcomp)
emmeans_anova <- emmeans(model_anova, ~ colour) %>%
  cld(Letters = letters) %>%
  as_tibble()
emmeans_anova
# The emmeans() function calculates the estimated marginal means (EMMs) for each level of the colour variable. The cld() function adds letters to indicate which means are significantly different from each other based on Tukey's HSD test. Means that share a letter are not significantly different from each other.

# Visualization using ggplot function
anova_plot <- ggplot(data_anova, aes(x = colour, y = number_of_visits)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, color = "blue") + # adds individual data points with some jitter for visibility
  geom_text(data = emmeans_anova, aes(x = colour, y = emmean + 1, label = str_trim(.group)), position = position_nudge(x = 0.1), color = "black", size = 5) + # adds significance letters
  theme_classic() +
  labs(title = "Number of Visits by Colour",
       x = "Colour",
       y = "Number of Visits") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red")  # adds error bars for standard error
anova_plot            
ggsave(here("output", "anova_model_plot.tiff"), plot = anova_plot, width = 6, height = 4, units = 'in', dpi = 300) # Save the plot as a tiff file in the output folder

# Example with two Categorical Independent Variables - Two-way ANOVA
# Load the data
data_two_way <- read_csv(here("data", "growth.csv")) %>%
  clean_names()
colnames(data_two_way)
# Inspect the data
glimpse(data_two_way)
# We see that the data contains two categorical variables (supplement and diet) where supplement has four levels (control, agrimore, supergain, and supersup) and diet has three levels (barley, oats, and wheat) and one continuous variable (gain).

# Visualize the data
ggplot(data_two_way, aes(x = supplement, y = gain)) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_classic() +
  labs(title = "Box plot of Gain by Supplement and Diet",
       x = "Supplement",
       y = "Gain (g)") +
  scale_fill_brewer(palette = "Set1")

ggplot(data_two_way, aes(x = diet, y = gain)) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_classic() +
  labs(title = "Box plot of Gain by Supplement and Diet",
       x = "Diet",
       y = "Gain (g)") +
  scale_fill_brewer(palette = "Set1")

ggplot(data_two_way, aes(x = diet, y = gain, fill = supplement)) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_classic() +
  labs(title = "Box plot of Gain by Supplement and Diet",
       x = "Diet",
       y = "Gain (g)") +
  scale_fill_brewer(palette = "Set1")

# There appears to be a difference in gain between the four supplements and three diets, with agrimore having the highest mean gain and supergain having the lowest mean gain. Among the Three diets, the barley diet seems to have the higher mean gain compared to oats or wheat. There also appears to be an interaction between supplement and diet, with the effect of supplement on gain varying depending on the diet.

# Fit the linear model (Two-way ANOVA)
model_two_way_additive <- lm(gain ~ supplement + diet, data = data_two_way)
summary(model_two_way_additive)
car::Anova(model_two_way_additive, type = "II") # Type II ANOVA table - Type III is used when there are interactions in the model

model_two_way_interaction <- lm(gain ~ supplement * diet, data = data_two_way)
summary(model_two_way_interaction)
car::Anova(model_two_way_interaction, type = "II") # Type II ANOVA table - Type III is used when there are interactions in the model
car::Anova(model_two_way_interaction, type = "III") # Type III ANOVA table - Type III is used when there are interactions in the model

# If the interaction was a crucial part of your hypothesis and you wanted to show that it was not significant - you should report type III
# If the interaction was not a crucial part of your hypothesis and you wanted to show the main effects of each independent variable - you should report type II becuase the interaction is not significant
# If the interaction is ever significant - you should report type III

# Test model assumptions
par(mfrow = c(2, 2))
plot(model_two_way_interaction)

# Pairwise comparisons
emmeans_supplement <- emmeans(model_two_way_additive, ~ supplement) %>%
  cld(Letters = letters) %>%
  as_tibble()
emmeans_supplement
emmeans_diet <- emmeans(model_two_way_additive, ~ diet) %>%
  cld(Letters = letters) %>%
  as_tibble()
emmeans_diet

emmeans_interaction <- emmeans(model_two_way_interaction, ~ supplement * diet) %>%
  cld(Letters = letters) %>%
  as_tibble()
emmeans_interaction

# Visualization using ggplot function
two_way_plot_supplement <- ggplot(data_two_way, aes(x = supplement, y = gain)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, color = "blue") + # adds individual data points with some jitter for visibility
  geom_text(data = emmeans_supplement, aes(x = supplement, y = emmean + 1, label = str_trim(.group)), position = position_nudge(x = 0.1), color = "black", size = 5) + # adds significance letters
  theme_classic() +
  labs(title = "Gain by Supplement",
       x = "Supplement",
       y = "Gain (g)") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red")  # adds error bars for standard error
two_way_plot_supplement
ggsave(here("output", "two_way_anova_model_plot_supplement.tiff"), plot = two_way_plot_supplement, width = 6, height = 4, units = 'in', dpi = 300) # Save the plot as a tiff file in the output folder

two_way_plot_diet <- ggplot(data_two_way, aes(x = diet, y = gain)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, color = "blue") + # adds individual data points with some jitter for visibility
  geom_text(data = emmeans_diet, aes(x = diet, y = emmean + 1, label = str_trim(.group)), position = position_nudge(x = 0.1), color = "black", size = 5) + # adds significance letters
  theme_classic() +
  labs(title = "Gain by Diet",
       x = "Diet",
       y = "Gain (g)") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red")  # adds error bars for standard error
two_way_plot_diet
ggsave(here("output", "two_way_anova_model_plot_diet.tiff"), plot = two_way_plot_diet, width = 6, height = 4, units = 'in', dpi = 300) # Save the plot as a tiff file in the output folder

library(ggpp) # for position_dodgenudge function
two_way_plot_interaction <- ggplot(data_two_way, aes(x = diet, y = gain, fill = supplement)) +
  geom_boxplot(position = position_dodge(1), outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(alpha = 0.5, position = position_dodge(1)) + # adds individual data points with some jitter for visibility
  geom_text(data = emmeans_interaction, aes(x = diet, y = emmean + 1, label = str_trim(.group)), position = position_dodgenudge(width = 1, x = 0.1), color = "black", size = 5) + # adds significance letters
  theme_classic() +
  labs(title = "Gain by Supplement and Diet",
       x = "Diet",
       y = "Gain (g)") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(1)) + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red", position = position_dodge(1)) +  # adds error bars for standard error
  scale_fill_brewer(palette = "Set1")
two_way_plot_interaction
ggsave(here("output", "two_way_anova_model_plot_interaction.tiff"), plot = two_way_plot_interaction, width = 6, height = 4, units = 'in', dpi = 300) # Save the plot as a tiff file in the output folder


# Example with Continuous and Categorical Independent Variables - ANCOVA
# Load the data
data_ancova <- read_csv(here("data", "limpet.csv")) %>%
  clean_names()
colnames(data_ancova)
# Inspect the data
glimpse(data_ancova)
# We see that the data contains one categorical variable (season) with two levels (spring, and summer) and two continuous variables (density and eggs).

# Visualize the data
ggplot(data_ancova, aes(x = density, y = eggs, color = season)) +
  geom_point() +
  theme_classic() +
  labs(title = "Scatter plot of Eggs vs. Density by Season",
       x = "Density (limpets per m^2)",
       y = "Eggs (thousands)") +
  scale_color_brewer(palette = "Set1")
# There appears to be a negative linear relationship between density and eggs for both seasons, with spring having a higher mean number of eggs than spring at any given density. There also may be an interaction between density and season, with the effect of density on eggs varying depending on the season.

# Fit the linear model (ANCOVA)
model_ancova_additive <- lm(eggs ~ density + season, data = data_ancova)
summary(model_ancova_additive)
car::Anova(model_ancova_additive, type = "II") 

model_ancova_interaction <- lm(eggs ~ density * season, data = data_ancova)
summary(model_ancova_interaction)
car::Anova(model_ancova_interaction, type = "III") 

# Test model assumptions
par(mfrow = c(2, 2))
plot(model_ancova_interaction)
plot(model_ancova_additive)
par(mfrow = c(1, 1))

# Pairwise comparisons
emtrends_ancova <- emtrends(model_ancova_interaction, ~ season, var = "density") %>%
  cld(Letters = letters) %>%
  as_tibble()
emtrends_ancova

# Slopes are not significantly different

emmeans_ancova <- emmeans(model_ancova_interaction, ~ season) %>%
  cld(Letters = letters) %>%
  as_tibble()
emmeans_ancova

# Intercepts are significantly different, with spring having a higher mean number of eggs than summer at any given density.

# Visualization using ggplot function
ancova_plot <- ggplot(data_ancova, aes(x = density, y = eggs, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # adds regression lines
  theme_classic() +
  labs(title = "Eggs vs. Density by Season",
       x = "Density (limpets per m^2)",
       y = "Eggs (thousands)") +
  scale_color_brewer(palette = "Set1") 
ancova_plot

season_plot <- ggplot(data_ancova, aes(x = season, y = eggs, fill = season)) +
  geom_boxplot(width = 0.25) +
  geom_text(data = emmeans_ancova, aes(x = season, y = emmean, label = str_trim(.group)), position = position_nudge(x = 0.2), color = "black", size = 5) + # adds significance letters
  theme_classic() +
  labs(title = "Eggs vs. Season",
       x = "Season",
       y = "Eggs (thousands)") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "top")
season_plot

library(patchwork)

combined_plot <- ancova_plot + season_plot

ggsave(here("output", "ancova_model_plot.tiff"), plot = combined_plot, width = 10, height = 4, units = 'in', dpi = 300) 


# Practice Examples

# 1) T-test reviewing gardens.csv data. Are there differences in the ozone readings between the two gardens (A and B)?
# Load the data
data_gardens <- read_csv(here("data", "gardens.csv")) %>%
  clean_names()
colnames(data_gardens)

# Inspect the data
glimpse(data_gardens)

# Visualize the data
ggplot(data_gardens, aes(x = garden, y = ozone)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Box plot of Ozone by Garden",
       x = "Garden",
       y = "Ozone (ppb)")

# There appears to be a difference in ozone between the two gardens, with garden A having a lower mean ozone than garden B.

# Fit the linear model (t-test)
t.test(ozone ~ garden, data = data_gardens, var.equal = TRUE) # var.equal = TRUE assumes equal variances between groups

# Check the assumptions of linear regression (t-test)
shapiro.test(data_gardens$ozone) # p-value = 0.6495, fail to reject null hypothesis of normality - data is normally distributed
car::leveneTest(ozone ~ garden, data = data_gardens) # p-value > 0.05 , fail to reject null hypothesis of equal variances - homoscedasticity can be assumed
bartlett.test(ozone ~ garden, data = data_gardens) # p-value > 0.05 , fail to reject null hypothesis of equal variances - homoscedasticity can be assumed

# Visualization using ggplot function
t_test_gardens_plot <- ggplot(data_gardens, aes(x = garden, y = ozone)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, color = "blue") + # adds individual data points with some jitter for visibility
  theme_classic() +
  labs(title = "Ozone by Garden",
       x = "Garden",
       y = "Ozone (ppb)") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red") + # adds error bars for standard error
  annotate("text", x = 1.5, y = max(data_gardens$ozone) - 5, label = "p < 0.05", size = 5) # adds significance annotation


# 2) T-Way ANOVA reviewing ladybirds.csv data. Are there differences in the number of ladybird beetles counted in the black or red color morphs from either the rural or industrial habitats?
# Load the data
data_ladybirds <- read_csv(here("data", "ladybirds.csv")) %>%
  clean_names()
colnames(data_ladybirds)

# Inspect the data
glimpse(data_ladybirds)

# Visualize the data
ggplot(data_ladybirds, aes(x = colour, y = number, fill = habitat)) +
  geom_boxplot(position = position_dodge(0.8)) +
  theme_classic() +
  labs(title = "Box plot of Number of Ladybird Beetles by Color Morph and Habitat",
       x = "Color Morph",
       y = "Number of Ladybird Beetles",
       fill = "Habitat") +
  scale_fill_brewer(palette = "Set1")

# There appears to be a difference in number of ladybird beetles between the two color morphs and two habitats, with an interaction between color morph and habitat, with the effect of color morph on number of ladybird beetles varying depending on the habitat.

# Fit the linear model (Two-way ANOVA)
model_ladybirds_interaction <- lm(number ~ colour * habitat, data = data_ladybirds)
summary(model_ladybirds_interaction)
car::Anova(model_ladybirds_interaction, type = "III") # Type III ANOVA table - Type III is used when there are interactions in the model

# Test model assumptions
par(mfrow = c(2, 2))
plot(model_ladybirds_interaction)
# The normal Q-Q plot shows that the residuals have some long tails but this could be due to the small sample size or heteroscedasticity.
par(mfrow = c(1, 1))

# Pairwise comparisons
emmeans_ladybirds <- emmeans(model_ladybirds_interaction, ~ colour * habitat) %>%
  cld(Letters = letters) %>%
  as_tibble()
emmeans_ladybirds

# Visualization using ggplot function
ladybirds_plot <- ggplot(data_ladybirds, aes(x = colour, y = number, fill = habitat)) +
  geom_boxplot(position = position_dodge(1), outlier.shape = NA) + # outlier.shape = NA removes outliers from the boxplot
  geom_jitter(alpha = 0.5, position = position_dodge(1)) + # adds individual data points with some jitter for visibility
  geom_text(data = emmeans_ladybirds, aes(x = colour, y = emmean + 1, label = str_trim(.group)), position = position_dodgenudge(width = 1, x = 0.3), color = "black", size = 5) + # adds significance letters
  theme_classic() +
  labs(title = "Number of Ladybird Beetles by Color Morph and Habitat",
       x = "Color Morph",
       y = "Number of Ladybird Beetles",
       fill = "Habitat") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(1)) + # adds mean point
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "red", position = position_dodge(1)) +  # adds error bars for standard error
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "top")
ladybirds_plot
ggsave(here("output", "two_way_anova_ladybirds_model_plot_interaction.tiff"), plot = ladybirds_plot, width = 6, height = 4, units = 'in', dpi = 300) 


# 3) ANCOVA revisiting the penguins_raw.csv data. Is there a relationship between the flipper_length of the penguins and its body mass? Does this relationship vary by species?
# Load the data
data_penguins <- read_csv(here("data", "penguins_raw.csv")) %>%
  clean_names() %>%
  filter(!is.na(flipper_length_mm) & !is.na(body_mass_g) & !is.na(species)) # remove rows with missing values in the relevant columns
colnames(data_penguins)

# Inspect the data
glimpse(data_penguins)

# Clean the data
data_penguins <- data_penguins %>%
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap")) %>%
  dplyr::select(species, body_mass_g, flipper_length_mm)

# Visualize the data
ggplot(data_penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  theme_classic() +
  labs(title = "Scatter plot of Body Mass vs. Flipper Length by Species",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  scale_color_brewer(palette = "Set1")


# Fit the linear model (ANCOVA)
model_penguins_interaction <- lm(body_mass_g ~ flipper_length_mm * species, data = data_penguins)
summary(model_penguins_interaction)
car::Anova(model_penguins_interaction, type = "III") # Type III ANOVA table - Type III is used when there are interactions in the model

# Test model assumptions
par(mfrow = c(2, 2))
plot(model_penguins_interaction)
# Assumptions appear to have been met

par(mfrow = c(1, 1))


# Pairwise comparisons
emtrends_penguins <- emtrends(model_penguins_interaction, ~ species, var = "flipper_length_mm") %>%
  cld(Letters = letters) %>%
  as_tibble()
emtrends_penguins
# Slopes are significantly different, so we will interpret the interaction model

# Visualization using ggplot function
penguins_plot <- ggplot(data_penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + # adds regression lines
  theme_classic() +
  labs(title = "Body Mass vs. Flipper Length by Species",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  scale_color_brewer(palette = "Set1")
penguins_plot
ggsave(here("output", "ancova_penguins_model_plot.tiff"), plot = penguins_plot, width = 6, height = 4, units = 'in', dpi = 300)