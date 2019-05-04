
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(assertthat)
library(assertr)
# hyperparameter for plots
cex_factor = 1.6

# Load and clean the data selecting for the desired fields
assertthat::assert_that(file.exists("./compas-scores-two-years.csv"))
assertthat::is.readable("./compas-scores-two-years.csv")
raw_data <- read.csv("./compas-scores-two-years.csv")

assertr::verify(raw_data, has_all_names("raw_data", "age", "c_charge_degree", "race", "age_cat", "score_text", "sex", "priors_count"))

