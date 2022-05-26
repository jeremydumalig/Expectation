setwd("~/Downloads/Most Improved Player")

library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(gt)
rm(list=ls())

# Load datasets
predictions <- read_csv(file="nba_predictions_2022.csv")
nba <- read_csv(file="historical-data/nba2000s.csv")

##################################################

# Create function to standardize data
standardize <- function(x){
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return( (x - mu)/sigma )
}

# Standardize data and pull out columns for predictor variables
nba <- select(nba, AGE, MIN, PPG, `FG%`, `3P%`, `FT%`, `USG%`, PPG2)

standard_nba <- nba %>%
  mutate(AGE = round(standardize(AGE), 1),
         MIN = round(standardize(MIN), 1),
         PPG = round(standardize(PPG), 1),
         `FG%` = round(standardize(`FG%`), 1),
         `3P%` = round(standardize(`3P%`), 1),
         `FT%` = round(standardize(`FT%`), 1),
         `USG%` = round(standardize(`USG%`), 1)) %>%
  select(AGE, MIN, PPG, `FG%`, `3P%`, `FT%`, `USG%`, PPG2)

##################################################

# Create ggplot theme
theme_borders <- 
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.background = element_rect(color="black"),
    title = element_text(size=20),
    legend.title = element_text(size=10),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

##################################################

# Create plot for model residuals
scatter_residuals <- ggplot(data=predictions) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_point(aes(x=PRED_RF, y=RES_RF),
             color='#C9082A',
             shape=1,
             size=3) +
  xlim(0, 32) +
  ylim(-8, 8) +
  labs(title="Model Residuals",
       x="Predicted Points Per Game",
       y="Residuals") +
  theme_borders

# Create plot for model residuals
histogram_residuals <- ggplot(data=predictions) +
  geom_histogram(aes(x=RES_RF), 
                 color='#17408B',
                 fill='#6495ED',
                 bins=32) +
  xlim(-8, 8) +
  coord_flip() +
  labs(title="", x="", y="") +
  theme_borders


# Create function to make differentials more readable
differential_sign <- function(number) {
  if (substr( format(number, nsmall=1), 1, 1 ) != "-") {
    return( paste("+", format(number, nsmall=1), sep="") )
  } else {
    return( number )
  }
}

# Display training and test data
raw_data <-
  head(nba, 5) %>%
  gt() %>%
  tab_spanner(
    label = "Training Set",
    columns = c(AGE, MIN, PPG, `FG%`, `3P%`, `FT%`, `USG%`)
  ) %>%
  tab_spanner(
    label = "Test Set",
    columns = c(PPG2)
  ) %>%
  tab_footnote(
    footnote = "2020-21 Regular Season",
    locations = cells_column_labels(columns = PPG)
  ) %>%
  tab_footnote(
    footnote = "2021-22 Regular Season",
    locations = cells_column_labels(columns = PPG2)
  ) %>%
  cols_label(
    PPG2 = "PPG"
  )

std_data <-
  head(standard_nba, 5) %>%
  gt() %>%
  tab_spanner(
    label = "Standardized Training Set",
    columns = c(AGE, MIN, PPG, `FG%`, `3P%`, `FT%`, `USG%`)
  ) %>%
  tab_spanner(
    label = "Test Set",
    columns = c(PPG2)
  ) %>%
  tab_footnote(
    footnote = "2020-21 Regular Season",
    locations = cells_column_labels(columns = PPG)
  ) %>%
  tab_footnote(
    footnote = "2021-22 Regular Season",
    locations = cells_column_labels(columns = PPG2)
  ) %>%
  cols_label(
    PPG2 = "PPG"
  )

# Display model predictions/residuals
predictions <- mutate(predictions,
                      RES_RF = map(RES_RF, differential_sign))

over_performers <- select(head(predictions, 10),
                      RANK, PLAYER, PRED_RF, RES_RF, PPG2) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic", size="small")),
    locations = cells_body(columns = c(RES_RF))
  ) %>%
  cols_label(
    PRED_RF = "Prediction",
    PPG2 = "Actual",
    RES_RF = ""
  ) %>%
  tab_header( 
    title = md("**2021-22 Regular Season: Over-Performers**"),
    subtitle = "Random Forest Model Results")

under_performers <- select(tail(predictions, 10),
                      RANK, PLAYER, PRED_RF, RES_RF, PPG2) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic", size="small")),
    locations = cells_body(columns = c(RES_RF))
  ) %>%
  cols_label(
    PRED_RF = "Prediction",
    PPG2 = "Actual",
    RES_RF = ""
  ) %>%
  tab_header( 
    title = md("**2021-22 Regular Season: Under-Performers**"), # Under-Performers
    subtitle = "Random Forest Model Results")

##################################################

# Display plots
plot_grid(scatter_residuals, histogram_residuals, ncol=2, rel_widths=c(3/4, 1/4))
raw_data
std_data
over_performers
under_performers
