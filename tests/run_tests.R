library(testthat) 


source("../src/compas_equalize_gender.R")
source("../src/compas_two_threshold_race.R")
source("../src/compas_equalize_race.R")

source("../src/compas_two_threshold_gender.R")
test_results <- test_dir("./", reporter="summary")
