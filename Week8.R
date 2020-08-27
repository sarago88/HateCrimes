################################################################################
###
### Data analysis in R: Week 8
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

# Article: https://fivethirtyeight.com/features/higher-rates-of-hate-crimes-are-tied-to-income-inequality/

# Read in data

crimes <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/hate-crimes/hate_crimes.csv")
View(crimes)

# Load packages
install.packages('maps')

library(tidyverse)
library(MASS)
library(maps)

## Data tidying ##

# Rename variables so they aren't so long

crimes_tidy <- crimes %>%
  rename(crimes_after = hate_crimes_per_100k_splc,
         crimes_before = avg_hatecrimes_per_100k_fbi,
         hs_degree = share_population_with_high_school_degree,
         pop_metro = share_population_in_metro_areas,
         income = median_household_income, 
         unemployment = share_unemployed_seasonal,
         white_poverty = share_white_poverty,
         income_inequality = gini_index,
         voted_trump = share_voters_voted_trump)

View(crimes_tidy)

## Analyses ##

# Linear regression

model1 <- lm(crimes_before ~ hs_degree + share_non_white + share_non_citizen +
               pop_metro + income + unemployment + white_poverty + income_inequality +
               voted_trump, data = crimes_tidy)

summary(model1)

model2 <- lm(crimes_after ~ hs_degree + share_non_white + share_non_citizen +
               pop_metro + income + unemployment + white_poverty + income_inequality +
               voted_trump, data = crimes_tidy)

summary(model2)

# Stepwise elimination (backward) using the "stepAIC" command (MASS package)

# Resource: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

elimination1 <- stepAIC(model1, direction = "backward", trace = FALSE)

# Why the error? Remove NAs for this to work

model1_no_na <- lm(crimes_before ~ hs_degree + share_non_white + share_non_citizen +
               pop_metro + income + unemployment + white_poverty + income_inequality +
               voted_trump, data = na.omit(crimes_tidy))

elimination1 <- stepAIC(model1_no_na, direction = "backward", trace = FALSE)
summary(elimination1)

model2_no_na <- lm(crimes_after ~ hs_degree + share_non_white + share_non_citizen +
                     pop_metro + income + unemployment + white_poverty + income_inequality +
                     voted_trump, data = na.omit(crimes_tidy))

elimination2 <- stepAIC(model2_no_na, direction = "backward", trace = FALSE)
summary(elimination2)

## Plotting map data ##

# Load longitude/latitude data for the United States

usa <- map_data("usa")
states <- map_data("state")

head(usa)
head(states)

# We are going to create a map of the US, and color states according to how many hate crimes
# (per 100k) they had before the election.

# Transform the "crimes_before" variable so that it is a factor variable, with five possible
# levels: 0, 1, 2, 3, 4

# Also rename "state" as "region," so that we can join it with the mapping data, and make
# these state names lower case.

crimes_tidyest <- crimes_tidy %>%
  mutate(crimes_before_factor = case_when(crimes_before < 1 ~ 0,
                                          crimes_before >=1 & crimes_before < 2 ~ 1,
                                          crimes_before >= 2 & crimes_before < 3 ~ 2,
                                          crimes_before >= 3 & crimes_before < 4 ~ 3,
                                          crimes_before >= 4 ~ 4),
         region = tolower(state),
         crimes_before_factor = factor(crimes_before_factor))

# Join these data frame with the "states" data drame

state_crimes <- states %>%
  left_join(crimes_tidyest, by = "region")

# Now we can begin plotting!

# Map the US:

ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3)

# Map the states:

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group)) +
  coord_fixed(ratio = 1.3) +
  guides(fill = FALSE)

# Create a plot called "usa_base," to which we will later add on. This
# should just be an outline of the USA with lines demarcating the states.
# All the states should be one color.




# Fill the states according to the crimes_before_factor variable. This tutorial
# will be helpful: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# Hint: reference the part where they plot population density by county of California

# Challenge yourself to remove axes/grid lines, and to change the color to more closely
# mimic the fivethirtyeight figure.





