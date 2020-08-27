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

library(tidyverse)
library(MASS)
library(maps)
library(cowplot)

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

## Analyses ##

# Linear regression

hatecrimes_before <- lm(crimes_before ~ hs_degree + share_non_white + share_non_citizen + 
                          pop_metro + income + unemployment + white_poverty +
                          income_inequality + voted_trump, data = crimes_tidy)

summary(hatecrimes_before)

hatecrimes_after <- lm(crimes_after ~ hs_degree + share_non_white + share_non_citizen + 
                          pop_metro + income + unemployment + white_poverty +
                          income_inequality + voted_trump, data = crimes_tidy)

summary(hatecrimes_after)
lm.beta(hatecrimes_after)

# Stepwise elimination (backward) using the "stepAIC" command (MASS package)

# Resource: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/

elimination1 <- stepAIC(hatecrimes_before, direction = "backward", trace = FALSE)

# Why the error? Remove NA's for this to work

elimination_mod1 <- lm(crimes_before ~ hs_degree + share_non_white + share_non_citizen + 
                         pop_metro + income + unemployment + white_poverty +
                         income_inequality + voted_trump, data = na.omit(crimes_tidy))

elimination1 <- stepAIC(elimination_mod1, direction = "backward", trace = FALSE)

summary(elimination1)

elimination_mod2 <- lm(crimes_after ~ hs_degree + share_non_white + share_non_citizen + 
                         pop_metro + income + unemployment + white_poverty +
                         income_inequality + voted_trump, data = na.omit(crimes_tidy))

elimination2 <- stepAIC(elimination_mod2, direction = "backward", trace = FALSE)

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
  rename(region = state) %>%
  mutate(region = tolower(region)) %>%
  mutate(crimes_before_factor = case_when(crimes_before < 1 ~ 0,
                                          crimes_before <= 2 ~ 1,
                                          crimes_before <= 3 ~ 2,
                                          crimes_before <= 4 ~ 3,
                                          crimes_before > 4 ~ 4),
         crimes_before_factor = factor(crimes_before_factor))

# Join these data frame with the "states" data drame

state_crimes <- states %>%
  left_join(crimes_tidyest, by = "region")

# Now we can begin plotting!

# As an example, map the US:

ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3)

# Mapping the states draws the outline of the US, including states:

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group)) +
  coord_fixed(ratio = 1.3) +
  guides(fill = FALSE)

# The last line ("guides") gets rid of the legend, which shows which color
# corresponds to which state.

# fill = region colors each state a different color; you would remove that
# to make each state the same color. This should provide a hint as to how
# we can later color the states according to the number of hate crimes.

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3) 

# Create a plot called "usa_base," to which we will later add on:

usa_base <- ggplot(data = usa, aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3) +
  geom_polygon(color = "black", fill = "grey") 

# Fill the states according to the crimes_before_factor variable. This tutorial
# will be helpful: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# Hint: reference the part where they plot population density by county of California

# Challenge yourself to remove axes/grid lines, and to change the color to more closely
# mimic the fivethirtyeight figure.

usa_base +
  geom_polygon(data = state_crimes, fill = NA, color = "white", size = .2) +
  geom_polygon(color = "black", fill = NA) +
  geom_polygon(data = state_crimes, aes(fill = crimes_before_factor)) +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill = "Average number of hate crimes") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank())

# Everything that is part of the "theme" layer is intended to remove
# axes, grid lines, etc. You can try to remove them one by one to understand
# which features they remove.

# You can also use theme_nothing() from the cowplot package to remove everything,
# but then we need to manually add back in the legend.

usa_base +
  geom_polygon(data = state_crimes, fill = NA, color = "white", size = .2) +
  geom_polygon(color = "black", fill = NA) +
  geom_polygon(data = state_crimes, aes(fill = crimes_before_factor)) +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill = "Average number of hate crimes") +
  theme_nothing() +
  theme(legend.position = "right")
