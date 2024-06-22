# Load required libraries
library(tidyverse)
library(ggcorrplot)
library(ordinal)
library(markovchain)

# Read in the CSV files
df_2015 <- read_csv("2015.csv")
df_2016 <- read_csv("2016.csv") 
df_2017 <- read_csv("2017.csv")
df_2018 <- read_csv("2018.csv")
df_2019 <- read_csv("2019.csv")

# Subset and rename the variables for each yearly data frame
df_2015 <- df_2015 %>%
  select(country, happiness_score, economy, health, family, freedom, generosity, trust) %>%
  rename(happiness_score = happiness)

df_2016 <- df_2016 %>%
  select(country, happiness.score, economy_gdp_per_capita, health_life_expectancy, family, freedom, generosity, trust) %>%
  rename(happiness = Happiness.Score, 
         economy = economy_gdp_per_capita,
         health = health_life_expectancy)

df_2017 <- df_2017 %>%
  select(country, happiness_score, economy_gdp_per_capita, health_life_expectancy, family, freedom, generosity, trust) %>%
  rename(happiness = score,
         economy = economy_gdp_per_capita,
         health = health_life_expectancy)

df_2018 <- df_2018 %>%
  select(country, happiness_score, economy_gdp_per_capita, health_life_expectancy, family, freedom, generosity, trust) %>%
  rename(economy = economy_gdp_per_capita,
         health = health_life_expectancy)

df_2019 <- df_2019 %>%
  select(country, happiness_score, economy_gdp_per_capita, health_life_expectancy, family, freedom, generosity, trust) %>%
  rename(economy = economy_gdp_per_capita,
         health = health_life_expectancy)

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
model <- clm(happiness_ordianal ~ economy + health + family + freedom + generosity + trust, 
              data = world_happiness)
# Summary of the model
summary(model)

# Generate predicted probabilities for each level of happiness
probabilities <- predict(model, type = "prob")

# Combine the predicted probabilities with the year variable
data <- cbind(world_happiness, probabilities)

# Fit the Markov model
mc <- normalize(as.matrix(data[, c("year", "prob1", "prob2", "prob3", "prob4", "prob5")]))
markov_model <- new("markovchain", transitionMatrix = mc, states = colnames(mc))

# Extract the transition matrix from the Markov chain model
P <- as.matrix(markov_model@transitionMatrix)

# Create a function to apply the Markov chain to each country
project_happiness <- function(data, years) {
  # Get the initial probabilities for each country
  initial_probs <- data[1, c("prob1", "prob2", "prob3", "prob4", "prob5")]
  
  # Apply the Markov chain to project happiness over time
  probs <- t(vapply(years, function(y) {
    initial_probs %*% matrix(P, nrow = 5, ncol = 5, byrow = TRUE)^y
  }, numeric(4)))
  
  # Combine the projected probabilities with the country and year
  projected_happiness <- data.frame(
    country = data$country[1],
    year = years,
    prob1 = probs[, 1],
    prob2 = probs[, 2],
    prob3 = probs[, 3],
    prob4 = probs[, 4],
    prob5 = probs[, 5]
  )
  
  return(projected_happiness)
}

# Apply the function to each country and combine the results
projected_happiness <- world_happiness %>%
  group_by(country) %>%
  do(project_happiness(., 1:10))

# Create the visualization
ggplot(projected_happiness, aes(x = year)) +
  geom_area(aes(y = prob1, fill = "Happiness Level 1")) +
  geom_area(aes(y = prob2, fill = "Happiness Level 2")) +
  geom_area(aes(y = prob3, fill = "Happiness Level 3")) +
  geom_area(aes(y = prob4, fill = "Happiness Level 4")) +
  geom_area(aes(y = prob5, fill = "Happiness Level 5")) +
  facet_wrap(~country, ncol = 4) +
  scale_fill_manual(values = c("#4B0082", "#8B008B", "#9370DB", "#E6E6FA")) +
  labs(title = "Projected Happiness Levels Over Time", x = "Year", y = "Probability", fill = "Happiness Level")
