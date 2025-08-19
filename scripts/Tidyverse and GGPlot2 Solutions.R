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
# tibble: for tibbles, a modern re-imagining of data frames
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

penguins_data <- read_csv("data/penguins_raw.csv")

# We can use another function from the here package to set the working directory to the root of the project if you have not connected the R project to the GitHub repository. The here function will set the working directory to the root of the project and allow you to use relative paths to access files in the project.

library(here)
penguins_data <- read_csv(here("data/penguins_raw.csv"))

# View the data
head(penguins_data) # View the first six rows of the data
head(penguins_data, 10) # View the first ten rows of the data
glimpse(penguins_data) # View the structure of the data

# This will allow you to see the structure of the data to confirm the data is tidy - or long.
# Tidy data is a way of organizing data so that each variable is in a column, and each observation is in a row. This makes it easier to work with the data and perform analyses.

# Cleaning the data - column names with the janitor package
penguins_data <- janitor::clean_names(penguins_data) # Various methods to establish unified format to column names

colnames(penguins_data) # View the cleaned column names

penguins_data <- rename(penguins_data,
                        "delta_15n"="delta_15_n_o_oo",  # use rename from the dplyr package
                        "delta_13c"="delta_13_c_o_oo")

# use mutate and case_when for a statement that conditionally changes the names of the values in a variable
penguins_data <- penguins_data %>% 
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap"))

# Use the select function to select specific columns from the data
penguins_data <- penguins_data %>%
  select(species, sex, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

#penguins <- penguins %>% 
#   select(-study_name, -sample_number, -region, -stage, -individual_id, -clutch_completion, -date_egg, -delta_15n, -delta_13c, -comments) # gives the same result

# If you want to filter the data (observations/rows) based on the characteristics of a variable

penguins_Adelie <- penguins_data %>%
  filter(species == "Adelie") # filters the data to only include rows where the species is Adelie


# You can include multiple conditions in a filter statement
penguins_Adelie_fl190 <- penguins_data %>%
  filter(species == "Adelie", flipper_length_mm > 190)

# Notice that by naming a new object I am not naming over the original penguins object. This can be useful if you want to keep the original data intact, or it can be confusing if you name these new objects poorly.

# The arrange function sorts the rows by a specific column
# If the column is numeric it will sort in ascending order, if it is character it will sort alphabetically.

penguins_species <- penguins_data %>%
  arrange(species) 

penguins_flipper <- penguins_data %>%
  arrange(flipper_length_mm) 

penguins_body_mass <- penguins_data %>%
  arrange(desc(body_mass_g)) # add desc to sort in descending order

# The group_by function groups the data by a specific column, allowing you to perform operations on each group separately. Combined with the summarise function allows you to make summary statistics for each group.

penguins_summary <- penguins_data %>%
  group_by(species) %>%
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE), # na.rm = TRUE ignores missing values
            mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            n = n()) # n() counts the number of rows in each group

# You can also group by multiple columns
penguins_summary_species_island <- penguins_data %>%
  group_by(species, island) %>%
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE), # na.rm = TRUE removes missing values
            mean_body_mass = mean(body_mass_g, na.rm = TRUE),
            n = n()) # n() counts the number of rows in each group

# You can combine multiple functions in a single pipeline using the pipe operator (%>%)
centered_penguins <- penguins_data %>% 
  group_by(sex, species) %>% 
  mutate(flipper_centered = flipper_length_mm-mean(flipper_length_mm, na.rm=TRUE))
# Centering a variable means subtracting the mean of that variable from each value, which allows you to see how each value deviates from the mean.

centered_penguins %>% 
  select(flipper_centered)
# Here you have tried to use the select function to select the centered variable, but it is not working because the centered_penguins is still grouped_by sex and species. You need to use the ungroup() function.

centered_penguins %>% 
  ungroup() %>% # Ungroup the data
  select(flipper_centered) # Now you see the centered variable

# Methods to check for distinct values in a variable (most useful for categorical variables)
distinct(penguins_data, species) 

# Count the number of missing values in the dataframe
penguins_data %>%
  is.na() %>%
  sum()
sum(is.na(penguins_data)) # same result - you can use the pipe or nested functions

# The type of variable is important to know when working with data. You can use the str() function to see the structure of the data and the type of each variable.
str(penguins_data) # View the structure of the data
# You see this when using glimpse() too

# Numerical variables can be changed to factors in certain ways, but factors cannot be changed to numbers
# Character variables can be changed to factors, and they are different in that factors can be ordered or unordered
# Use the mutate function to achieve this

# We are converting the variable flipper length (continuous) to a categorical variable (character) with three levels: small, medium, and large.
penguins_data <- penguins_data %>% 
  mutate(flipper_range = case_when(flipper_length_mm <= 190 ~ "small",
                                   flipper_length_mm >190 & flipper_length_mm < 213 ~ "medium",
                                   flipper_length_mm >= 213 ~ "large"))

# The function case_when() allows you to create a new variable based on conditions. In this case, we are creating a new variable called flipper_range that categorizes the flipper length into three ranges: small, medium, and large.

# Now we will convert the flipper_range variable to a factor variable with ordered levels
penguins_data <- penguins_data %>% 
  mutate(flipper_range = fct_relevel(flipper_range, "small", "medium", "large"))

# The function fct_relevel() allows you to change the order of the levels in a factor variable. In this case, we are reordering the flipper_range variable so that small is first, medium is second, and large is third. Otherwise they would be ordered alphabetically.

### Exercise ###
# 1) Using the data set from the course repo "surveys_complete_77_89.csv" in the data folder, select all the variables in the data set except record_id and species_id. How many observations and variables are in the new data set?
surveys <- read_csv(here("data/surveys_complete_77_89.csv"))

surveys_1 <- surveys %>%
  select(-record_id, -species_id)
str(surveys_1)

# 16,878 observations and 11 variables without record_id and species_id

# 2) Using the pipes and dplyr functions, subset the data to include animals collected before 1995 and retain the columns year, sex, and weight. Then what was the average weight (kg) by sex?

surveys_2 <- surveys %>%
  filter(year < 1995) %>%
  select(year, sex, weight) %>%
  group_by(sex) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE),
            n = n())

# Female: 53.1, 53.2 (kg)

# 3) Then create a new data frame from the survey data that only contains the species_id column and a new column called hind_foot_cm created from the hindfoot_length values (currently in mm). This new data frame should include entries that are less than 3 cm and no missing values. Then calculate the mean hind_foot_cm for each species_id and arrange the data frame in ascending order. What were the species with the smallest and largest mean hind_foot_cm?

surveys_3 <- surveys %>%
  mutate(hind_foot_cm = hindfoot_length/10) %>%
  select(species_id, hind_foot_cm) %>%
  filter(hind_foot_cm < 3, !is.na(hind_foot_cm)) %>%
  group_by(species_id) %>%
  summarise(mean_hind_foot_cm = mean(hind_foot_cm, na.rm = TRUE),
            n = n()) %>%
  arrange(mean_hind_foot_cm)

# Smallest: BA, 1.95 (cm)
# Largest: DO, 2.85 (cm)


# Introduction to ggplot2 and Data visualization
# We will be using both the tidyverse and ggplot2 packages to create visualizations of data.

# Load the data - many packages have data files included that you can use to practice with.
# Can call the plamerpenguin library in the Help tab to see the data sets included in the package.
library(palmerpenguins)
penguins

# This data should look familiar - it is already cleaned version from the penguins data set we used above

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

# We can add a smooth curve showing the relationship between body mass and flipper length.

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


# The last thing to note is that you can save the graph to a file using the ggsave() function. This function saves the last plot created in the current R session to a file. Using there here() function to direct the ggsave() function to the output folder in the project directory helps keep the project organized.

Bill_Length <- ggplot(data = penguins, aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(title = "Body mass and bill length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill length (mm)", y = "Body mass (g)",
       color = "Species", shape = "Species") +
  theme_classic() +
  scale_color_colorblind()

ggsave(here("output","Bill_Length.tiff"), plot = Bill_Length, width = 8, height = 6, dpi = 300)
# You can specify the file type (e.g., .tiff, .png, .pdf, etc.) and the dimensions of the plot in inches. The dpi argument specifies the resolution of the image in dots per inch.

# The file type .svg is another powerful option where you can upload the file into powerpoint and move the elements around, change the colors, and edit the text. This is a vector file type that allows for more flexibility in editing the graph after it has been created.
ggsave(here("output","Bill_Length.svg"), plot = Bill_Length, width = 8, height = 6, dpi = 300)


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



