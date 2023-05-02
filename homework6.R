# Danielle Owens
# PLAN 372 HW 6

install.packages("stringr")
library(stringr)
library(ggplot2)
library(dplyr)

# Set workspace
setwd("/Users/danielleowens/Downloads")

# Upcload dataset
raw_tree <- read.csv("raw_tree_data.csv")
raw_tree

# Task 1
# Use a regular expression to create separate columns for 
# the city name and state abbreviation

# Creating new columns
raw_tree$city_name <- str_extract(raw_tree$City, "^[A-Za-z ]+")
raw_tree$state_abbr <- str_extract(raw_tree$City, "[A-Z]{2}$")
raw_tree

# Count records in each state
state_counts <- table(raw_tree$state_abbr)
state_counts

# Bar plot of state counts
state_counts <- raw_tree %>% 
  group_by(state_abbr) %>% 
  summarize(count = n())

ggplot(state_counts, aes(x = state_abbr, y = count, fill = state_abbr)) + 
  geom_bar(stat = "identity") +
  ggtitle("Number of Records by State") +
  xlab("State") +
  ylab("Count") 

# Task 2
# Filter the dataset to only these NC and SC, what cities did they collect data from?

# Filtered datset
carolina_tree <- raw_tree %>% 
  filter(state_abbr %in% c("NC", "SC"))
carolina_tree

# Cities in NC and SC that have data
unique(carolina_tree$city_name)

# Task 3
# What genus of trees has the largest crown diameter in North and South Carolina?
# The crown size is in the column AvgCdia (m), in meters.
# scientific names contain both a genus (plural genera) and species
# This requires you to write a regular expression to extract the genus.

# Extract genus from scientific name column
carolina_tree$genus <- str_extract(carolina_tree$ScientificName, "^\\w+")
carolina_tree

# Maximum crown diameter of each genus
biggest_crown <- carolina_tree %>%
  group_by(genus) %>% 
  summarize(max_cdia = max(AvgCdia..m.))  

# Average crown diameter of each genus
avg_crown <- carolina_tree %>%
  group_by(genus) %>% 
  summarize(avg_cdia = mean(AvgCdia..m.))

# Extra Credit: Tree Age 1
avg_age <- carolina_tree %>%
  group_by(genus) %>% 
  summarize(avg_age = mean(Age))

# Extra Credit: Tree Age 2

# Create scatterplot of Age vs. AvgCdia
ggplot(carolina_tree, aes(x = Age, y = AvgCdia..m.)) +
  geom_point() +
  facet_wrap(~genus) +
  labs(x = "Average Crown Diameter (inches)", y = "Age (years)")

# Regression age x diameter for each genus
reg_results <- carolina_tree %>% 
  group_by(genus) %>% 
  summarize(Age_Coefficient = coef(lm(AvgCdia..m. ~ Age))[2])
reg_results

# Extra Credit: Species 
# Extract genus from scientific name column
carolina_tree$species <- str_extract(carolina_tree$ScientificName, "(?<= )[[:alpha:]]+$")

# Within each genus of tree, how many species are recorded in the dataset?
species_count <- carolina_tree %>%
  group_by(genus) %>%
  summarize(species_count = length(unique(species)))