# Load required libraries
library(tidyverse)

# Read in the CSV files
df_2015 <- read_csv("2015.csv")
df_2016 <- read_csv("2016.csv") 
df_2017 <- read_csv("2017.csv")
df_2018 <- read_csv("2018.csv")
df_2019 <- read_csv("2019.csv")

#there are six predictors: economy, health, family, freedom, generosity and trust. Some need to be renamed so that they are common across all data sets before they can be merged.

# Combine the data frames into a single data set
world_happiness <- bind_rows(
  df_2015 %>% mutate(year = 2015),
  df_2016 %>% mutate(year = 2016), 
  df_2017 %>% mutate(year = 2017),
  df_2018 %>% mutate(year = 2018),
  df_2019 %>% mutate(year = 2019)
)

# Preview the merged data set
head(world_happiness)

# Discretize the happiness variable
world_happiness <- world_happiness %>%
  mutate(happiness_ordinal = case_when(
    happiness < 3 ~ 1,
    happiness >= 3 & happiness < 5 ~ 2, 
    happiness >= 5 & happiness < 7 ~ 3,
    happiness >= 7 & happiness < 9 ~ 4,
    happiness >= 9 ~ 5
  ))

# Preview the updated data frame
head(world_happiness)

# Create the six scatter plots with color-coding
ggplot(world_happiness, aes(x = happiness_ordinal, y = economy, color = as.factor(happiness_ordinal))) +
  geom_point() +
  labs(title = "Happiness vs Economy", color = "Happiness Score")

ggplot(world_happiness, aes(x = happiness_ordinal, y = health, color = as.factor(happiness_ordinal))) +
  geom_point() +
  labs(title = "Happiness vs Health", color = "Happiness Score")

ggplot(world_happiness, aes(x = happiness_ordinal, y = family, color = as.factor(happiness_ordinal))) +
  geom_point() +
  labs(title = "Happiness vs Family", color = "Happiness Score")

ggplot(world_happiness, aes(x = happiness_ordinal, y = freedom, color = as.factor(happiness_ordinal))) +
  geom_point() +
  labs(title = "Happiness vs Freedom", color = "Happiness Score")

ggplot(world_happiness, aes(x = happiness_ordinal, y = generosity, color = as.factor(happiness_ordinal))) +
  geom_point() +
  labs(title = "Happiness vs Generosity", color = "Happiness Score")

ggplot(world_happiness, aes(x = happiness_ordinal, y = trust, color = as.factor(happiness_ordinal))) +
  geom_point() +
  labs(title = "Happiness vs Trust", color = "Happiness Score")
