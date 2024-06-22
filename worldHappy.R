# Load required libraries
library(tidyverse)
library(ggcorrplot)
library(MASS)
library(markovchain)

# Read in the CSV files
df_2015 <- read_csv("2015.csv")
df_2016 <- read_csv("2016.csv") 
df_2017 <- read_csv("2017.csv")
df_2018 <- read_csv("2018.csv")
df_2019 <- read_csv("2019.csv")

# Subset and rename the variables for each yearly data frame
df_2015 <- df_2015 %>%
  dplyr::select(Country, `Happiness Score`, `Economy (GDP per Capita)`, 'Health (Life Expectancy)', Family, Freedom, Generosity, 'Trust (Government Corruption)') %>%
  rename(country = Country,
        happiness = `Happiness Score`,
        economy = 'Economy (GDP per Capita)',
        health = 'Health (Life Expectancy)',
      family = Family,
      freedom = Freedom,
      generosity = Generosity,
      trust = 'Trust (Government Corruption)')

df_2016 <- df_2016 %>%
  dplyr::select(Country, `Happiness Score`, `Economy (GDP per Capita)`, 'Health (Life Expectancy)', Family, Freedom, Generosity, 'Trust (Government Corruption)') %>%
  rename(country = Country,
         happiness = `Happiness Score`,
         economy = 'Economy (GDP per Capita)',
         health = 'Health (Life Expectancy)',
         family = Family,
         freedom = Freedom,
         generosity = Generosity,
         trust = 'Trust (Government Corruption)')

df_2017 <- df_2017 %>%
  dplyr::select(Country, Happiness.Score, Economy..GDP.per.Capita., Health..Life.Expectancy., Family, Freedom, Generosity, Trust..Government.Corruption.) %>%
  rename(country = Country,
         happiness = Happiness.Score,
         economy = Economy..GDP.per.Capita.,
         health = Health..Life.Expectancy.,
         family = Family,
         freedom = Freedom,
         generosity = Generosity,
         trust = Trust..Government.Corruption.)

df_2018 <- df_2018 %>%
  dplyr::select('Country or region', Score, 'GDP per capita','Healthy life expectancy', 'Social support', 'Freedom to make life choices', Generosity, 'Perceptions of corruption') %>%
  rename(country = 'Country or region',
         happiness = Score,
         economy = 'GDP per capita',
         health = 'Healthy life expectancy',
         family = 'Social support',
         freedom = 'Freedom to make life choices',
         generosity = Generosity,
         trust = 'Perceptions of corruption')
    df_2018 <- df_2018[-c(20), ]
    df_2018$trust <- as.numeric(df_2018$trust)


 df_2019 <- df_2019 %>%
   dplyr::select('Country or region', Score, 'GDP per capita','Healthy life expectancy', 'Social support', 'Freedom to make life choices', Generosity, 'Perceptions of corruption') %>%
  rename(country = 'Country or region',
         happiness = Score,
         economy = 'GDP per capita',
         health = 'Healthy life expectancy',
         family = 'Social support',
         freedom = 'Freedom to make life choices',
         generosity = Generosity,
         trust = 'Perceptions of corruption')

# Combine the data frames into a single data set
world_happiness <- rbind(
  df_2015 %>% mutate(year = 2015),
  df_2016 %>% mutate(year = 2016), 
  df_2017 %>% mutate(year = 2017),
  df_2018 %>% mutate(year = 2018),
  df_2019 %>% mutate(year = 2019)
)

# Preview the merged data set
head(world_happiness)
world_happiness$trust <- as.numeric(world_happiness$trust)

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

# Calculate the correlation matrix
cor_matrix <- cor(world_happiness[, c("economy", "health", "family", "freedom", "generosity", "trust")])

# Create the correlation matrix plot
ggcorrplot(cor_matrix,
           hc.order = TRUE,
           type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray(),
           lab = TRUE)
# Fit the ordinal logistic regression model
model <- polr(as.factor(happiness_ordinal) ~ economy + health + family + freedom + generosity + trust, 
             data = world_happiness)
# Summary of the model
summary(model)
