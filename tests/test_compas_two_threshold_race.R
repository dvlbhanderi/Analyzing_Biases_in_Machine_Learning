library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

# hyperparameter for plots
cex_factor = 1.6

# Load and clean the data selecting for the desired fields
raw_data <- read.csv("./compas-scores-two-years.csv")
cat(predictiveValue(5, positive=T,r="Male"))
# all members with score >= threshold are marked will recidivate for Positive
# no one is marked will recidivate => PPV = 1
test_that("Test > Threshold",{
  expect_equal(predictiveValue(11), 1)
})
# all members with score < threshold are marked will recidivate for Positive
# no one is marked will recidivate => PPV = 1
test_that("Test < Threshold",{
  expect_equal(predictiveValue(0), 1)
})
# all members with score < threshold are marked will recidivate for Positive
# no one is marked will recidivate => PPV = 1
test_that("Test < Threshold",{
  expect_equal(round(predictiveValue(5, positive=T,r="Male"), 3),  0.686)
})

