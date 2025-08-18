#Introduction to tidyverse and ggplot2 packages

# Where to begin
# You will need R and RStudio installed on your computer.
# Next you will need to install the tidyverse package and some additional packages.

# You can install the tidyverse package by running the following command in your R console:
install.packages("tidyverse")
# Or you can install it from the RStudio interface by going to the "Packages" tab and clicking on "Install".

# Once you have installed a package you will need to load it into your R session by running the following command:
library(tidyverse)

# This shows you the nine additional core packages that are included in the tidyverse package:
# dplyr: for data manipulation
# forcats: for working with factors
# ggplot2: for data visualization
# lubridate: for working with dates and times
# purrr: for functional programming
# readr: for data import
# stringr: for string manipulation
# tibble: for tibbles, a modern reimagining of data frames
# tidyr: for data tidying

# Other package to install outside of tidyverse include:
install.packages(
  c("arrow", "babynames", "curl", "duckdb", "gapminder", 
    "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
    "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
    "repurrrsive", "tidymodels", "writexl")
)
# You can install a list of packages by passing a character vector (concatenated list) to the install.packages() function as shown above.

# Loading the packages
# More complicated to load multiple packages at once - use lapply() function to apply a function over a list
multiple_packages <- c("arrow", "babynames", "curl", "duckdb", "gapminder", 
                       "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
                       "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
                       "repurrrsive", "tidymodels", "writexl")
lapply(multiple_packages, library, character.only = TRUE)

# You may have noticed that there are some conflicts between the packages. Some commonly named functions are found in multiple packages (e.g., filter(), select(), etc.). When there is a conflict between packages, the last package loaded will take precedence. You can specify which package you want to use by using the package name followed by two colons and the function name (e.g., dplyr::filter(), ggplot2::ggplot(), etc.).

# Data management basics
# Working directory
getwd() # Get the current working directory
# setwd("path/to/your/directory") # Set the working directory to a specific path - not needed with R project

# Following relative path names we want to load a data file from the data folder.
# You can use the read_csv() function from the readr package to read in a CSV file.

penguins <- read_csv("data/penguins_raw.csv")

# R is not looking in the right folder, but we can use another function from the here package to set the working directory to the root of the project.

library(here)
penguins <- read_csv(here("data/penguins_raw.csv"))

# View the data
head(penguins) # View the first six rows of the data
head(penguins, 10) # View the first ten rows of the data
glimpse(penguins) # View the structure of the data

# This will allow you to see the structure of the data to confirm the data is tidy - or long.
# Tidy data is a way of organizing data so that each variable is in a column, and each observation is in a row. This makes it easier to work with the data and perform analyses.

# Cleaning the data - column names with the janitor package
penguins <- janitor::clean_names(penguins) # Various methods to establish unified format to column names

colnames(penguins) # View the cleaned column names

penguins <- rename(penguins,
                   "delta_15n"="delta_15_n_o_oo",  # use rename from the dplyr package
                   "delta_13c"="delta_13_c_o_oo")

# use mutate and case_when for a statement that conditionally changes the names of the values in a variable
penguins <- penguins %>% 
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap"))

# Use the select function to select specific columns from the data
penguins <- penguins %>%
  select(species, sex, island, culmen_length_mm, culmen_depth_mm, flipper_length_mm, body_mass_g)

#penguins <- penguins %>% 
#   select(-study_name, -sample_number, -region, -stage, -individual_id, -clutch_completion, -date_egg, -delta_15n, -delta_13c, -comments) # gives the same result

# If you want to filter the data (observations/rows) based on the characteristics of a variable

penguins_Adelie <- penguins %>%
  filter(species == "Adelie") # filters the data to only include rows where the species is Adelie


# You can include multiple conditions in a filter statement
penguins_Adelie_fl190 <- penguins %>%
  filter(species == "Adelie", flipper_length_mm > 190)

# Notice that by naming a new object I am not naming over the original penguins object. This can be useful if you want to keep the original data intact, or it can be confusing if you name these new objects poorly.

# The arrange function sorts the rows by a specific column
# If the column is numeric it will sort in ascending order, if it is character it will sort alphabetically.

penguins_species <- penguins %>%
  arrange(species) 

penguins_flipper <- penguins %>%
  arrange(flipper_length_mm) 

penguins_body_mass <- penguins %>%
  arrange(desc(body_mass_g)) # add desc to sort in descending order

# The group_by function groups the data by a specific column, allowing you to perform operations on each group separately. Combined with the summarise function allows you to make summary statistics for each group.

penguins_summary <- penguins %>%
  group_by(species) %>%
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE), # na.rm = TRUE removes missing values
            mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            n = n()) # n() counts the number of rows in each group

# Introduction to ggplot2 and Data visualization
# We will be using both the tidyverse and ggplot2 packages to create visualizations of data.

# Load the data - many packages have data files included that you can use to practice with.
# Can call the plamerpenguin library in the Help tab to see the data sets included in the package.
library(palmerpenguins)
penguins

# View the data
glimpse(penguins)
summary(penguins)

# We see seven variables in the dataset. I want you to list what you think are the dependent (response/y) and independent (manipulated/x) variables.

# Dependent:

# Independent:


# Based on your answers above, what type of graph do you think would be best to visualize the data?
# What variables would you use for the x and y axes?
# What are the types of variables you are using (categorical, continuous, etc.)?
# How do you want to represent the data (points, lines, bars, etc.)?


# When we create a graph in ggplot you build layers of geometry to represent the data.
# The first layer is the coordinate space where you define the x and y axes.

ggplot(data = penguins)

# This creates a blank graph with the axes defined but no data points.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g))

# This defines the aesthetics of the axes for the graph and draws the minimum and maximum ranges and their labels.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point()

# "geom_point()" adds a layer of points to the graph to represent the data. The points are plotted based on the x and y values defined in the aesthetics. There are many other geometries you can use to represent different types of data, such as lines (geom_line()), bars (geom_bar()), histograms (geom_histogram()), etc. You can find a full list of geometries in the ggplot2 documentation.

# We also see a warning message in the console - "Removed 2 rows containing missing values (geom_point())." This means that there are two rows in the dataset that have missing values for either the flipper_length_mm or body_mass_g variables. ggplot2 automatically removes these rows from the graph. You can check for missing values in the dataset using the is.na() function or the complete.cases() function.

is.na(penguins$bill_length_mm)
is.na(penguins$body_mass_g)

# You can add additional aesthetic elements to the figure by adding more arguments to the aes() function. For example, you can add color to the points based on the species of the penguins.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point()

# This adds a color aesthetic to the points based on the species variable.
# This works because the species variable is a categorical variable with three levels: Adelie, Chinstrap, and Gentoo. ggplot2 automatically assigns different colors to each level of the variable.

# We can add a smooth curve showing the realtionship between body mass and flipper length.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(method = "lm")

# Here we have a line for each species, but what if we only wanted a single line for all the penguins?
# When aesthetics are defined in the ggplot() function, they apply to all layers of the graph. If you want to change the aesthetics for a specific layer, you can define them in that layer's function.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm", color = "black")

# This adds a black line for the smooth curve and keeps the color aesthetic for the points based on species.

# You can also change the shape of the points based on the species variable.

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm", color = "black")

# Now we will change the labels of the axes and the title of the graph.

ggplot(data = penguins, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(title = "Body mass and bill length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill length (mm)", y = "Body mass (g)",
       color = "Species", shape = "Species") +
  theme_classic() +
  scale_color_colorblind()

# This adds a title, subtitle, and labels for the x and y axes, as well as the color and shape legends.
# The scale_color_colorblind() function is used to make the colors more accessible for people with color blindness. You can also use other scale functions to change the colors, shapes, sizes, etc. of the aesthetics.


# Exercise

# Select another variable from the penguins dataset to plot against body mass.
# Consider the type of variable you are using (categorical, continuous, etc.) and how you want to represent the data (points, lines, bars, etc.).

penguins %>%
  filter(!is.na(island)) %>% # Remove missing values
  ggplot(aes(x = island, fill = species)) +
  geom_bar() +
  labs(title = "Number of Penguins by Island and Species",
       x = "Island", y = "Count", fill = "Species") +
  theme_classic()+
  scale_fill_colorblind()

# Now try plotting two continuous variables against each other and adding a third continuous variable as color.

penguins %>%
  filter(!is.na(body_mass_g)) %>% # Remove missing values
  ggplot() +
  geom_point(aes(x = flipper_length_mm, y = body_mass_g/1000, color = bill_depth_mm, shape = species)) +
  geom_smooth(aes(x = flipper_length_mm, y = body_mass_g/1000), se = T) +
  labs(title = "Body Mass, Bill Depth, and Flipper Length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Fipper Length (mm)", y = "Body Mass (Kg)", color = "Bill Depth (mm)", shape = "Species") +
  theme_classic()

# You can also facet the graph by a categorical variable to create multiple panels.

penguins %>%
  filter(!is.na(body_mass_g)) %>% # Remove missing values
  ggplot(aes(x = flipper_length_mm, y = body_mass_g/1000)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island) + # Multiple panels by island
  labs(title = "Body Mass and Flipper Length by Island and Species",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Fipper Length (mm)", y = "Body Mass (Kg)", color = "Species", shape = "Species") +
  theme_classic()



