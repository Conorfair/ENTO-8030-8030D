# Multiple Linear Regression - lm() with multiple independent variables

# Multiple linear regression is an extension of simple linear regression that allows for the inclusion of multiple independent variables to (hopefully) more accurately predict a single continuous dependent variable. This approach helps to understand the relationship between several predictors and the outcome, and it can also control for confounding variables. 

# We will use the same lm() function in R, but now we will include multiple independent variables in the model formula. Furthermore, we will explore how to interpret the results of a multiple linear regression analysis. This includes how to adjust the prediction of individual independent variables while holding others constant. 

# Lastly we will discuss the process of model selection methods to identify the best fitting model to represent the relationship among the predictors and the response variable.

# Load necessary libraries
library(tidyverse)
library(janitor)
library(here)
library(ggthemes)
library(emmeans)
library(patchwork)
library(multcomp)
library(MuMIn) # for model selection and dredge function

# We will revisit the penguins dataset
# Load the data and clean it up a bit
penguins <- read_csv(here("data", "penguins_raw.csv")) %>%
  clean_names() %>%
  select(c(species, island, culmen_length_mm, culmen_depth_mm,
           flipper_length_mm, body_mass_g, sex)) %>%
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
  ))

# Let's take a look at the data
glimpse(penguins) # looks good

# Let's visualize the realtionship between body mass and the other variables
Bill_Length_Plot <- ggplot(penguins, aes(x = culmen_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Body Mass vs Bill Length",
       x = "Bill Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Bill_Depth_Plot <- ggplot(penguins, aes(x = culmen_depth_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Body Mass vs Bill Depth",
       x = "Bill Depth (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Flipper_Length_Plot <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Body Mass vs Flipper Length",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Species_Plot <- ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species",
       x = "Species",
       y = "Body Mass (g)") +
  theme_classic()
Island_Plot <- ggplot(penguins, aes(x = island, y = body_mass_g)) +
  geom_boxplot() +
  labs(title = "Body Mass by Island",
       x = "Island",
       y = "Body Mass (g)") +
  theme_classic()
Sex_Plot <- ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  labs(title = "Body Mass by Sex",
       x = "Sex",
       y = "Body Mass (g)") +
  theme_classic()

# Combine the plots into a grid
combined_penguins_plot <- (Species_Plot + Island_Plot + Sex_Plot) / (Bill_Length_Plot + Bill_Depth_Plot + Flipper_Length_Plot) +
  plot_layout(nrow = 2) +
  plot_annotation(title = "Penguin Body Mass Relationships",
                  subtitle = "Exploring relationships between body mass and other variables",
                  caption = "Data: Palmer Station LTER")

# After reviewing the individual plots we can see some clear relationships between body mass and the other variables. However, there are some possible interactions between some of the independent variables - we see this especially with the Bill Depth data.

# Let's visualize the interaction plots
Bill_Length_Species_Plot <- ggplot(penguins, aes(x = culmen_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Length by Species",
       x = "Bill Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Bill_Length_Sex_Plot <- ggplot(penguins, aes(x = culmen_length_mm, y = body_mass_g, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Length by Sex",
       x = "Bill Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Bill_Length_Island_Plot <- ggplot(penguins, aes(x = culmen_length_mm, y = body_mass_g, color = island)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Length by Island",
       x = "Bill Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Bill_Depth_Species_Plot <- ggplot(penguins, aes(x = culmen_depth_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Depth by Species",
       x = "Bill Depth (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Bill_Depth_Sex_Plot <- ggplot(penguins, aes(x = culmen_depth_mm, y = body_mass_g, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Depth by Sex",
       x = "Bill Depth (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Bill_Depth_Island_Plot <- ggplot(penguins, aes(x = culmen_depth_mm, y = body_mass_g, color = island)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Depth by Island",
       x = "Bill Depth (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Flipper_Length_Species_Plot <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Flipper Length by Species",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Flipper_Length_Sex_Plot <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Flipper Length by Sex",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()
Flipper_Length_Island_Plot <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = island)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Flipper Length by Island",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_classic()

combined_penguins_interaction_plot <- (Bill_Length_Species_Plot + Bill_Length_Sex_Plot + Bill_Length_Island_Plot) / (Bill_Depth_Species_Plot + Bill_Depth_Sex_Plot + Bill_Depth_Island_Plot) / (Flipper_Length_Species_Plot + Flipper_Length_Sex_Plot + Flipper_Length_Island_Plot) +
  plot_layout(nrow = 3) +
  plot_annotation(title = "Penguin Body Mass Relationships with Species, Sex, and Island",
                  subtitle = "Exploring relationships between body mass and interaction between Species, Sex, and Island and continuous variables",
                  caption = "Data: Palmer Station LTER")


# Our understanding of the possible realtionship begins to take shape
# There is the interaction between the independent continuous variables and species and island - a three-way interaction is possible to estimate but notoriously complex to interpret.

# Let's consider an interaction between two continous variables
Bill_Length_Depth_Interaction_Plot <- ggplot(penguins, aes(x = culmen_length_mm, y = body_mass_g, color = culmen_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Length with Bill Depth Interaction",
       x = "Bill Length (mm)",
       y = "Body Mass (g)",
       color = "Bill Depth (mm)") +
  theme_classic()
Bill_Flipper_Length__Interaction_Plot <- ggplot(penguins, aes(x = culmen_length_mm, y = body_mass_g, color = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Bill Length with Flipper Length Interaction",
       x = "Bill Length (mm)",
       y = "Body Mass (g)",
       color = "Flipper Length (mm)") +
  theme_classic()
Flipper_Length_Bill_Depth_Interaction_Plot <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = culmen_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Mass vs Flipper Length with Bill Depth Interaction",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)",
       color = "Bill Depth (mm)") +
  theme_classic()
combined_penguins_continuous_interaction_plot <- (Bill_Length_Depth_Interaction_Plot + Bill_Flipper_Length__Interaction_Plot + Flipper_Length_Bill_Depth_Interaction_Plot) +
  plot_layout(nrow = 1) +
  plot_annotation(title = "Penguin Body Mass Relationships with Continuous Variable Interactions",
                  subtitle = "Exploring relationships between body mass and interaction between continuous variables",
                  caption = "Data: Palmer Station LTER")

# Some possible interactions here as well - difficult to interpret without strong theoretical backing to guide our understanding of the data

# You can see how this can get complex quickyl and how the visualizations can help us understand the possible relationships

# Let's fit a multiple linear regression model
# Start with a full model including all independent variables
model_full <- lm(body_mass_g ~ species + island + sex+ culmen_length_mm +
                   culmen_depth_mm + flipper_length_mm, data = penguins)
summary(model_full)

# The summary output provides coefficients for each independent variable, along with their significance levels.

car::Anova(model_full, type = "II")

# Effect of Island is not significant in this model
# Depending on your research question, you might consider removing it from the model
# Let's consider Island as a possible covariate and remove it from the model

model_no_island <- lm(body_mass_g ~ species + sex+ culmen_length_mm +
                        culmen_depth_mm + flipper_length_mm, data = penguins)
summary(model_no_island)

# Compare how the estimates have changed without the parameter for island

car::Anova(model_no_island, type = "II")

# Compare how the F statistics for each parameter have changed without the parameter for island


# Now we know that this is not the best model - while investigating the data we saw some possible interactions between the variables
# Let's fit a model with an interaction between Bill Depth and Species

model_bill_depth_by_species <- lm(body_mass_g ~ sex+ culmen_length_mm +
                                    culmen_depth_mm * species + flipper_length_mm, data = penguins)
summary(model_bill_depth_by_species)
car::Anova(model_bill_depth_by_species, type = "III")
# We require type III sums of square now that there is an interaction term in the model

# The interpretation of the main effect of species is ignored in the presence of the significant interaction term

# You can see how the total possible interactions can become overwhelming quickly
# There are forwards and backwards selection methods can be used to identify the best fitting model
# However there are some pitfalls to these methods and they should be used with caution
# We will use the dredge function from the MuMIn package to explore all possible models
penguins <- na.omit(penguins) # dredge does not work with NA values

# Deciding a priori what possible interactions should be considered (biologically and statistically) is important to avoid overfitting the model

# The following code considers all possible 2-way interactions between the categorical and continuous variables

fm <- lm(body_mass_g ~ (sex + island + species) * (culmen_length_mm + culmen_depth_mm + flipper_length_mm), data = penguins)
dredge_models <- dredge(fm, trace = TRUE, rank = "AIC")
# View the top models based on AICc
head(dredge_models)

# Consider the top models that have a delta AICc of less than 2
# We have three candidate models 
# Model 1
mod_1 <- lm(body_mass_g ~ sex + species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + culmen_depth_mm:sex + culmen_length_mm:sex, data = penguins)

mod_2 <- lm(body_mass_g ~ sex + species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + culmen_depth_mm:sex + culmen_length_mm:sex + culmen_length_mm:species, data = penguins)

mod_3 <- lm(body_mass_g ~ sex + species + culmen_length_mm + culmen_depth_mm + flipper_length_mm + culmen_depth_mm:sex + culmen_length_mm:sex + flipper_length_mm:sex, data = penguins)

# Compare the output from all three models
summary(mod_1)
summary(mod_2)
summary(mod_3)

car::Anova(mod_1, type = "III")
car::Anova(mod_2, type = "III")
car::Anova(mod_3, type = "III")

emtrends(mod_1, ~ sex, var = "culmen_depth_mm") %>%
  cld(Letters = letters)
emtrends(mod_1, ~ sex, var = "culmen_length_mm") %>%
  cld(Letters = letters)

emtrends(mod_2, ~ sex, var = "culmen_depth_mm") %>%
  cld(Letters = letters)
emtrends(mod_2, ~ sex, var = "culmen_length_mm") %>%
  cld(Letters = letters)
emtrends(mod_2, ~ sex, var = "flipper_length_mm") %>%
  cld(Letters = letters)

emtrends(mod_3, ~ sex, var = "culmen_depth_mm") %>%
  cld(Letters = letters)
emtrends(mod_3, ~ sex, var = "culmen_length_mm") %>%
  cld(Letters = letters)
emtrends(mod_3, ~ species, var = "culmen_length_mm") %>%
  cld(Letters = letters)

# Now that we have used the dredge function to determine which terms should be included in the model - we should not drop a non-significant term from the model if it is part of an interaction. The model with the lowest AIC is now the "best approximating model" from a candidate set of models.

# This is mixing approaches of AIC and null hypothesis significance testing (NHST) - this is not ideal but is unfortunatrly commonly done in practice. You should avoid doing this. Stick to one approach in model selection and interpretation. Sometimes you may have a clear theory driven approach to model selection based on NHST, and other times you may be innudated with possible interactions and covariates that may require AIC model selection approaches and the dredge function. 

# Let's visualize the model fit for the best approximating model

# First option - plot all species

# First we need to create a new data frame for predictions
New_Bill_Depth <- expand.grid(culmen_depth_mm = seq(min(penguins$culmen_depth_mm, na.rm = TRUE),
                                                    max(penguins$culmen_depth_mm, na.rm = TRUE),
                                                    length.out = 100),
                              sex = unique(penguins$sex), # levels of sex
                              species = unique(penguins$species), # levels of species
                              culmen_length_mm = mean(penguins$culmen_length_mm, na.rm = TRUE), # mean bill length
                              flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass <- predict(mod_1, newdata = New_Bill_Depth, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Depth <- cbind(New_Bill_Depth, Predicted_Body_Mass)
# View the new data frame
glimpse(New_Bill_Depth)
# Change the column names for the predicted values
New_Bill_Depth <- New_Bill_Depth %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

# Plot the predictions
Predicted_Body_Mass <- ggplot() +
  geom_point(New_Bill_Depth, mapping = aes(x = culmen_depth_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Depth, mapping = aes(x = culmen_depth_mm, y = body_mass_g, color = sex, linetype = species), linewidth = 1) +
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)") +
  theme_classic()


# Second option to select reference level for species
New_Bill_Depth <- expand.grid(culmen_depth_mm = seq(min(penguins$culmen_depth_mm, na.rm = TRUE),
                                                    max(penguins$culmen_depth_mm, na.rm = TRUE),
                                                    length.out = 100),
                              sex = unique(penguins$sex), # levels of sex
                              species = "Adelie", # select reference level for species
                              culmen_length_mm = mean(penguins$culmen_length_mm, na.rm = TRUE), # mean bill length
                              flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass <- predict(mod_1, newdata = New_Bill_Depth, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Depth <- cbind(New_Bill_Depth, Predicted_Body_Mass)
# View the new data frame
glimpse(New_Bill_Depth)
# Change the column names for the predicted values
New_Bill_Depth <- New_Bill_Depth %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

# Plot the predictions for the Adelie species only
Predicted_Body_Mass <- ggplot() +
  geom_point(New_Bill_Depth, mapping = aes(x = culmen_depth_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Depth, mapping = aes(x = culmen_depth_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)") +
  theme_classic()

# Third option - marginal average prediction across all species

# First we need to create a new data frame for predictions
New_Bill_Depth <- expand.grid(culmen_depth_mm = seq(min(penguins$culmen_depth_mm, na.rm = TRUE),
                                                    max(penguins$culmen_depth_mm, na.rm = TRUE),
                                                    length.out = 100),
                              sex = unique(penguins$sex), # levels of sex
                              species = unique(penguins$species), # levels of species
                              culmen_length_mm = mean(penguins$culmen_length_mm, na.rm = TRUE), # mean bill length
                              flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass <- predict(mod_1, newdata = New_Bill_Depth, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Depth <- cbind(New_Bill_Depth, Predicted_Body_Mass)
# View the new data frame
glimpse(New_Bill_Depth)
# Change the column names for the predicted values
New_Bill_Depth <- New_Bill_Depth %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

New_Bill_Depth <- New_Bill_Depth %>%
  group_by(culmen_depth_mm, sex) %>%
  summarise(body_mass_g = mean(body_mass_g),
            Lwr = mean(Lwr),
            Upr = mean(Upr),
            .groups = 'drop')
# Plot the predictions
Predicted_Body_Mass <- ggplot() +
  geom_point(New_Bill_Depth, mapping = aes(x = culmen_depth_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Depth, mapping = aes(x = culmen_depth_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Depth (mm)", y = "Body Mass (g)") +
  theme_classic()


# Exercise: Produce a figure that shows the relationship between bill length and body mass for each sex
# Compare the three options for visualizing the model fit

New_Bill_Length <- expand.grid(culmen_length_mm = seq(min(penguins$culmen_length_mm, na.rm = TRUE),
                                                    max(penguins$culmen_length_mm, na.rm = TRUE),
                                                    length.out = 100),
                              sex = unique(penguins$sex), # levels of sex
                              species = unique(penguins$species), # levels of species
                              culmen_depth_mm = mean(penguins$culmen_depth_mm, na.rm = TRUE), # mean bill depth
                              flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass <- predict(mod_1, newdata = New_Bill_Length, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Length <- cbind(New_Bill_Length, Predicted_Body_Mass)
# View the new data frame
glimpse(New_Bill_Length)
# Change the column names for the predicted values
New_Bill_Length <- New_Bill_Length %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

# Plot the predictions
Predicted_Body_Mass_Bill_Length <- ggplot() +
  geom_point(New_Bill_Length, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Length, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex, linetype = species), linewidth = 1) +
  labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
  theme_classic()


# Second option to select reference level for species
New_Bill_Length <- expand.grid(culmen_length_mm = seq(min(penguins$culmen_length_mm, na.rm = TRUE),
                                                    max(penguins$culmen_length_mm, na.rm = TRUE),
                                                    length.out = 100),
                              sex = unique(penguins$sex), # levels of sex
                              species = "Adelie", # select reference level for species
                              culmen_depth_mm = mean(penguins$culmen_depth_mm, na.rm = TRUE), # mean bill depth
                              flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass <- predict(mod_1, newdata = New_Bill_Length, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Length <- cbind(New_Bill_Length, Predicted_Body_Mass)
# View the new data frame
glimpse(New_Bill_Length)
# Change the column names for the predicted values
New_Bill_Length <- New_Bill_Length %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

# Plot the predictions for the Adelie species only
Predicted_Body_Mass_Bill_Length <- ggplot() +
  geom_point(New_Bill_Length, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Length, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
  theme_classic()

# Third option - marginal average prediction across all species

# First we need to create a new data frame for predictions
New_Bill_Length <- expand.grid(culmen_length_mm = seq(min(penguins$culmen_length_mm, na.rm = TRUE),
                                                    max(penguins$culmen_length_mm, na.rm = TRUE),
                                                    length.out = 100),
                              sex = unique(penguins$sex), # levels of sex
                              species = unique(penguins$species), # levels of species
                              culmen_depth_mm = mean(penguins$culmen_depth_mm, na.rm = TRUE), # mean bill depth
                              flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass <- predict(mod_1, newdata = New_Bill_Length, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Length <- cbind(New_Bill_Length, Predicted_Body_Mass)
# View the new data frame
glimpse(New_Bill_Length)
# Change the column names for the predicted values
New_Bill_Length <- New_Bill_Length %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

New_Bill_Length <- New_Bill_Length %>%
  group_by(culmen_length_mm, sex) %>%
  summarise(body_mass_g = mean(body_mass_g),
            Lwr = mean(Lwr),
            Upr = mean(Upr),
            .groups = 'drop')
# Plot the predictions
Predicted_Body_Mass_Bill_Length <- ggplot() +
  geom_point(New_Bill_Length, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Length, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
  theme_classic()

# Slopes are different for Bill length compared to Bill depth, but the interpretation is similar

# Compare the marginal predictions across the three best fitting models
New_Bill_Length_1 <- expand.grid(culmen_length_mm = seq(min(penguins$culmen_length_mm, na.rm = TRUE),
                                                      max(penguins$culmen_length_mm, na.rm = TRUE),
                                                      length.out = 100),
                               sex = unique(penguins$sex), # levels of sex
                               species = unique(penguins$species), # levels of species
                               culmen_depth_mm = mean(penguins$culmen_depth_mm, na.rm = TRUE), # mean bill depth
                               flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass_1 <- predict(mod_1, newdata = New_Bill_Length_1, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Length_1 <- cbind(New_Bill_Length_1, Predicted_Body_Mass_1)
# View the new data frame
glimpse(New_Bill_Length_1)
# Change the column names for the predicted values
New_Bill_Length_1 <- New_Bill_Length_1 %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

New_Bill_Length_1 <- New_Bill_Length_1 %>%
  group_by(culmen_length_mm, sex) %>%
  summarise(body_mass_g = mean(body_mass_g),
            Lwr = mean(Lwr),
            Upr = mean(Upr),
            .groups = 'drop')
# Plot the predictions
Predicted_Body_Mass_Bill_Length_1 <- ggplot() +
  geom_point(New_Bill_Length_1, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Length_1, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
  theme_classic()


New_Bill_Length_2 <- expand.grid(culmen_length_mm = seq(min(penguins$culmen_length_mm, na.rm = TRUE),
                                                        max(penguins$culmen_length_mm, na.rm = TRUE),
                                                        length.out = 100),
                                 sex = unique(penguins$sex), # levels of sex
                                 species = unique(penguins$species), # levels of species
                                 culmen_depth_mm = mean(penguins$culmen_depth_mm, na.rm = TRUE), # mean bill depth
                                 flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass_2 <- predict(mod_2, newdata = New_Bill_Length_2, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Length_2 <- cbind(New_Bill_Length_2, Predicted_Body_Mass_2)
# View the new data frame
glimpse(New_Bill_Length_2)
# Change the column names for the predicted values
New_Bill_Length_2 <- New_Bill_Length_2 %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

New_Bill_Length_2 <- New_Bill_Length_2 %>%
  group_by(culmen_length_mm, sex) %>%
  summarise(body_mass_g = mean(body_mass_g),
            Lwr = mean(Lwr),
            Upr = mean(Upr),
            .groups = 'drop')
# Plot the predictions
Predicted_Body_Mass_Bill_Length_2 <- ggplot() +
  geom_point(New_Bill_Length_2, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Length_2, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
  theme_classic()


New_Bill_Length_3 <- expand.grid(culmen_length_mm = seq(min(penguins$culmen_length_mm, na.rm = TRUE),
                                                        max(penguins$culmen_length_mm, na.rm = TRUE),
                                                        length.out = 100),
                                 sex = unique(penguins$sex), # levels of sex
                                 species = unique(penguins$species), # levels of species
                                 culmen_depth_mm = mean(penguins$culmen_depth_mm, na.rm = TRUE), # mean bill depth
                                 flipper_length_mm = mean(penguins$flipper_length_mm, na.rm = TRUE)) # mean flipper length

# Use the predict function to get the predicted values from the model
Predicted_Body_Mass_3 <- predict(mod_3, newdata = New_Bill_Length_3, interval = "confidence")
# Combine the predicted values with the new data frame
New_Bill_Length_3 <- cbind(New_Bill_Length_3, Predicted_Body_Mass_3)
# View the new data frame
glimpse(New_Bill_Length_3)
# Change the column names for the predicted values
New_Bill_Length_3 <- New_Bill_Length_3 %>%
  rename(body_mass_g = fit, Lwr = lwr, Upr = upr)

New_Bill_Length_3 <- New_Bill_Length_3 %>%
  group_by(culmen_length_mm, sex) %>%
  summarise(body_mass_g = mean(body_mass_g),
            Lwr = mean(Lwr),
            Upr = mean(Upr),
            .groups = 'drop')
# Plot the predictions
Predicted_Body_Mass_Bill_Length_3 <- ggplot() +
  geom_point(New_Bill_Length_3, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), alpha = 0.5) +
  geom_line(New_Bill_Length_3, mapping = aes(x = culmen_length_mm, y = body_mass_g, color = sex), linewidth = 1) +
  labs(x = "Bill Length (mm)", y = "Body Mass (g)") +
  theme_classic()

# Combine the three plots
Combined_Marginal_Plots <- Predicted_Body_Mass_Bill_Length_1 + Predicted_Body_Mass_Bill_Length_2 + Predicted_Body_Mass_Bill_Length_3 +
  plot_layout(nrow = 1) +
  plot_annotation(title = "Marginal Predictions of Body Mass vs Bill Length",
                  subtitle = "Comparing the three best fitting models",
                  caption = "Data: Palmer Station LTER")

# How does the statistical and biological interpretation compare among the three models?

emtrends(mod_1, ~ sex, var = "culmen_length_mm") %>%
  cld(Letters = letters)
emtrends(mod_2, ~ sex, var = "culmen_length_mm") %>%
  cld(Letters = letters)
emtrends(mod_3, ~ sex, var = "culmen_length_mm") %>%
  cld(Letters = letters)

# Slope for female penguins is always larger than males, but the magnitude for model 2 is steeper than the other models (31.49 > 30.00 > 26.55). The slope for males is steepest in model 3 (7.14 > 6.99 > 4.98). The slopes are always positive across all three models for both sexes. As bill length increases so does body mass. You can also report the 95% confidence intervals for the slopes as well.
