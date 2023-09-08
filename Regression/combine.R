library(tidymodels)
library(tidyverse)
library(rpart)
library(xgboost)
library(stacks)

set.seed(123)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

cleaned_train <- train %>% 
  select(-name)

recipe_1 <- recipe(percent_dem ~ ., data = cleaned_train) %>%
  # drop variable id
  step_rm(id) %>%
  
  step_impute_mean(income_per_cap_2016, income_per_cap_2017, 
                   income_per_cap_2018, income_per_cap_2019, 
                   income_per_cap_2020, gdp_2016, gdp_2017, 
                   gdp_2018, gdp_2019, gdp_2020) %>%
  
  # drop duplicate variable
  step_rm(x0033e, x0036e, x0058e, x0025e, x0029e) %>%
  
  step_mutate(x0002e = replace_na(x0002e / x0001e, 0),
              x0003e = replace_na(x0003e / x0001e, 0)) %>%
  
  # drop redundant variable
  step_rm(x0005e, x0006e, x0007e, x0008e, x0009e,
          x0010e, x0011e, x0012e, x0013e, x0014e,
          x0015e, x0016e, x0017e) %>%
  
  step_mutate(x0026e = replace_na(x0026e / x0021e, 0), 
              x0027e = replace_na(x0027e / x0021e, 0)) %>%
  
  step_mutate(x0030e = replace_na(x0030e / x0024e, 0), 
              x0031e = replace_na(x0031e / x0024e, 0)) %>%
  
  step_mutate(x0019e = replace_na(x0019e / x0001e, 0),
              x0020e = replace_na(x0020e / x0001e, 0),
              x0021e = replace_na(x0021e / x0001e, 0),
              x0022e = replace_na(x0022e / x0001e, 0),
              x0023e = replace_na(x0023e / x0001e, 0),
              x0024e = replace_na(x0024e / x0001e, 0)) %>%
  
  step_mutate(x0040e = replace_na(x0040e / x0039e, 0),
              x0041e = replace_na(x0041e / x0039e, 0),
              x0042e = replace_na(x0042e / x0039e, 0),
              x0043e = replace_na(x0043e / x0039e, 0)) %>%
  
  step_mutate(x0045e = replace_na(x0045e / x0044e, 0),
              x0046e = replace_na(x0046e / x0044e, 0),
              x0047e = replace_na(x0047e / x0044e, 0),
              x0048e = replace_na(x0048e / x0044e, 0),
              x0049e = replace_na(x0049e / x0044e, 0),
              x0050e = replace_na(x0050e / x0044e, 0),
              x0051e = replace_na(x0051e / x0044e, 0)) %>%
  
  step_mutate(x0053e = replace_na(x0053e / x0052e, 0),
              x0054e = replace_na(x0054e / x0052e, 0),
              x0055e = replace_na(x0055e / x0052e, 0),
              x0056e = replace_na(x0056e / x0052e, 0)) %>%
  
  step_mutate(x0037e = replace_na(x0037e / x0034e, 0),
              x0038e = replace_na(x0038e / x0034e, 0),
              x0039e = replace_na(x0039e / x0034e, 0),
              x0044e = replace_na(x0044e / x0034e, 0),
              x0052e = replace_na(x0052e / x0034e, 0),
              x0057e = replace_na(x0057e / x0034e, 0)) %>%
  
  step_mutate(x0059e = replace_na(x0059e / x0035e, 0),
              x0060e = replace_na(x0060e / x0035e, 0),
              x0061e = replace_na(x0061e / x0035e, 0),
              x0062e = replace_na(x0062e / x0035e, 0)) %>%
  
  step_mutate(x0034e = replace_na(x0034e / x0001e, 0),
              x0035e = replace_na(x0035e / x0001e, 0)) %>%
  
  step_mutate(x0064e = replace_na(x0064e / x0001e, 0),
              x0065e = replace_na(x0065e / x0001e, 0),
              x0066e = replace_na(x0066e / x0001e, 0),
              x0067e = replace_na(x0067e / x0001e, 0),
              x0068e = replace_na(x0068e / x0001e, 0),
              x0069e = replace_na(x0069e / x0001e, 0)) %>%
  
  step_mutate(x0072e = replace_na(x0072e / x0071e, 0),
              x0073e = replace_na(x0073e / x0071e, 0),
              x0074e = replace_na(x0074e / x0071e, 0),
              x0075e = replace_na(x0075e / x0071e, 0)) %>%
  
  step_mutate(x0084e = replace_na(x0084e / x0083e, 0), 
              x0085e = replace_na(x0085e / x0083e, 0)) %>%
  
  step_mutate(x0077e = replace_na(x0077e / x0076e, 0),
              x0078e = replace_na(x0078e / x0076e, 0),
              x0079e = replace_na(x0079e / x0076e, 0),
              x0080e = replace_na(x0080e / x0076e, 0),
              x0081e = replace_na(x0081e / x0076e, 0),
              x0082e = replace_na(x0082e / x0076e, 0),
              x0083e = replace_na(x0083e / x0076e, 0)) %>%
  
  step_mutate(x0071e = replace_na(x0071e / x0001e, 0),
              x0076e = replace_na(x0076e / x0001e, 0)) %>%
  
  step_mutate(x0088e = replace_na(x0088e / x0087e, 0),
              x0089e = replace_na(x0089e / x0087e, 0)) %>%
  
  step_mutate(c01_002e = replace_na(c01_002e / c01_001e, 0),
              c01_003e = replace_na(c01_003e / c01_001e, 0),
              c01_004e = replace_na(c01_004e / c01_001e, 0),
              c01_005e = replace_na(c01_005e / c01_001e, 0)) %>%
  
  step_mutate(c01_007e = replace_na(c01_007e / c01_006e, 0),
              c01_008e = replace_na(c01_008e / c01_006e, 0),
              c01_009e = replace_na(c01_009e / c01_006e, 0),
              c01_010e = replace_na(c01_010e / c01_006e, 0),
              c01_011e = replace_na(c01_011e / c01_006e, 0),
              c01_012e = replace_na(c01_012e / c01_006e, 0),
              c01_013e = replace_na(c01_013e / c01_006e, 0),
              c01_014e = replace_na(c01_014e / c01_006e, 0),
              c01_015e = replace_na(c01_015e / c01_006e, 0)) %>%
  
  step_mutate(c01_017e = replace_na(c01_017e / c01_016e, 0),
              c01_018e = replace_na(c01_018e / c01_016e, 0)) %>%
  
  step_mutate(c01_020e = replace_na(c01_020e / c01_019e, 0),
              c01_021e = replace_na(c01_021e / c01_019e, 0)) %>%
  
  step_mutate(c01_023e = replace_na(c01_023e / c01_022e, 0),
              c01_024e = replace_na(c01_024e / c01_022e, 0)) %>%
  
  step_mutate(c01_026e = replace_na(c01_026e / c01_026e, 0),
              c01_027e = replace_na(c01_027e / c01_026e, 0)) %>%
  
  step_mutate(c01_001e = replace_na(c01_001e / x0001e, 0),
              c01_006e = replace_na(c01_001e / x0001e, 0),
              c01_016e = replace_na(c01_001e / x0001e, 0),
              c01_019e = replace_na(c01_001e / x0001e, 0),
              c01_022e = replace_na(c01_001e / x0001e, 0),
              c01_025e = replace_na(c01_001e / x0001e, 0)) %>%
  
  step_normalize(income_per_cap_2016, income_per_cap_2017, income_per_cap_2018, 
                 income_per_cap_2019, income_per_cap_2020) %>%
  
  step_normalize(gdp_2016, gdp_2017, gdp_2018, gdp_2019, gdp_2020)

recipe_2 <- recipe(percent_dem ~ ., data = cleaned_train) %>%
  # drop variable id
  step_rm(id) %>%
  
  step_impute_linear(income_per_cap_2016, income_per_cap_2017, 
                     income_per_cap_2018, income_per_cap_2019, 
                     income_per_cap_2020, gdp_2016, gdp_2017, 
                     gdp_2018, gdp_2019, gdp_2020,
                     impute_with = imp_vars(x0001e, x2013_code)) %>%
  
  # drop duplicate variable
  step_rm(x0033e, x0036e, x0058e, x0025e, x0029e) %>%
  
  step_mutate(x0002e = replace_na(x0002e / x0001e, 0),
              x0003e = replace_na(x0003e / x0001e, 0)) %>%
  
  # drop redundant variable
  step_rm(x0005e, x0006e, x0007e, x0008e, x0009e,
          x0010e, x0011e, x0012e, x0013e, x0014e,
          x0015e, x0016e, x0017e) %>%
  
  step_mutate(x0026e = replace_na(x0026e / x0021e, 0), 
              x0027e = replace_na(x0027e / x0021e, 0)) %>%
  
  step_mutate(x0030e = replace_na(x0030e / x0024e, 0), 
              x0031e = replace_na(x0031e / x0024e, 0)) %>%
  
  step_mutate(x0019e = replace_na(x0019e / x0001e, 0),
              x0020e = replace_na(x0020e / x0001e, 0),
              x0021e = replace_na(x0021e / x0001e, 0),
              x0022e = replace_na(x0022e / x0001e, 0),
              x0023e = replace_na(x0023e / x0001e, 0),
              x0024e = replace_na(x0024e / x0001e, 0)) %>%
  
  step_mutate(x0040e = replace_na(x0040e / x0039e, 0),
              x0041e = replace_na(x0041e / x0039e, 0),
              x0042e = replace_na(x0042e / x0039e, 0),
              x0043e = replace_na(x0043e / x0039e, 0)) %>%
  
  step_mutate(x0045e = replace_na(x0045e / x0044e, 0),
              x0046e = replace_na(x0046e / x0044e, 0),
              x0047e = replace_na(x0047e / x0044e, 0),
              x0048e = replace_na(x0048e / x0044e, 0),
              x0049e = replace_na(x0049e / x0044e, 0),
              x0050e = replace_na(x0050e / x0044e, 0),
              x0051e = replace_na(x0051e / x0044e, 0)) %>%
  
  step_mutate(x0053e = replace_na(x0053e / x0052e, 0),
              x0054e = replace_na(x0054e / x0052e, 0),
              x0055e = replace_na(x0055e / x0052e, 0),
              x0056e = replace_na(x0056e / x0052e, 0)) %>%
  
  step_mutate(x0037e = replace_na(x0037e / x0034e, 0),
              x0038e = replace_na(x0038e / x0034e, 0),
              x0039e = replace_na(x0039e / x0034e, 0),
              x0044e = replace_na(x0044e / x0034e, 0),
              x0052e = replace_na(x0052e / x0034e, 0),
              x0057e = replace_na(x0057e / x0034e, 0)) %>%
  
  step_mutate(x0059e = replace_na(x0059e / x0035e, 0),
              x0060e = replace_na(x0060e / x0035e, 0),
              x0061e = replace_na(x0061e / x0035e, 0),
              x0062e = replace_na(x0062e / x0035e, 0)) %>%
  
  step_mutate(x0034e = replace_na(x0034e / x0001e, 0),
              x0035e = replace_na(x0035e / x0001e, 0)) %>%
  
  step_mutate(x0064e = replace_na(x0064e / x0001e, 0),
              x0065e = replace_na(x0065e / x0001e, 0),
              x0066e = replace_na(x0066e / x0001e, 0),
              x0067e = replace_na(x0067e / x0001e, 0),
              x0068e = replace_na(x0068e / x0001e, 0),
              x0069e = replace_na(x0069e / x0001e, 0)) %>%
  
  step_mutate(x0072e = replace_na(x0072e / x0071e, 0),
              x0073e = replace_na(x0073e / x0071e, 0),
              x0074e = replace_na(x0074e / x0071e, 0),
              x0075e = replace_na(x0075e / x0071e, 0)) %>%
  
  step_mutate(x0084e = replace_na(x0084e / x0083e, 0), 
              x0085e = replace_na(x0085e / x0083e, 0)) %>%
  
  step_mutate(x0077e = replace_na(x0077e / x0076e, 0),
              x0078e = replace_na(x0078e / x0076e, 0),
              x0079e = replace_na(x0079e / x0076e, 0),
              x0080e = replace_na(x0080e / x0076e, 0),
              x0081e = replace_na(x0081e / x0076e, 0),
              x0082e = replace_na(x0082e / x0076e, 0),
              x0083e = replace_na(x0083e / x0076e, 0)) %>%
  
  step_mutate(x0071e = replace_na(x0071e / x0001e, 0),
              x0076e = replace_na(x0076e / x0001e, 0)) %>%
  
  step_mutate(x0088e = replace_na(x0088e / x0087e, 0),
              x0089e = replace_na(x0089e / x0087e, 0)) %>%
  
  step_mutate(c01_002e = replace_na(c01_002e / c01_001e, 0),
              c01_003e = replace_na(c01_003e / c01_001e, 0),
              c01_004e = replace_na(c01_004e / c01_001e, 0),
              c01_005e = replace_na(c01_005e / c01_001e, 0)) %>%
  
  step_mutate(c01_007e = replace_na(c01_007e / c01_006e, 0),
              c01_008e = replace_na(c01_008e / c01_006e, 0),
              c01_009e = replace_na(c01_009e / c01_006e, 0),
              c01_010e = replace_na(c01_010e / c01_006e, 0),
              c01_011e = replace_na(c01_011e / c01_006e, 0),
              c01_012e = replace_na(c01_012e / c01_006e, 0),
              c01_013e = replace_na(c01_013e / c01_006e, 0),
              c01_014e = replace_na(c01_014e / c01_006e, 0),
              c01_015e = replace_na(c01_015e / c01_006e, 0)) %>%
  
  step_mutate(c01_017e = replace_na(c01_017e / c01_016e, 0),
              c01_018e = replace_na(c01_018e / c01_016e, 0)) %>%
  
  step_mutate(c01_020e = replace_na(c01_020e / c01_019e, 0),
              c01_021e = replace_na(c01_021e / c01_019e, 0)) %>%
  
  step_mutate(c01_023e = replace_na(c01_023e / c01_022e, 0),
              c01_024e = replace_na(c01_024e / c01_022e, 0)) %>%
  
  step_mutate(c01_026e = replace_na(c01_026e / c01_026e, 0),
              c01_027e = replace_na(c01_027e / c01_026e, 0)) %>%
  
  step_mutate(c01_001e = replace_na(c01_001e / x0001e, 0),
              c01_006e = replace_na(c01_001e / x0001e, 0),
              c01_016e = replace_na(c01_001e / x0001e, 0),
              c01_019e = replace_na(c01_001e / x0001e, 0),
              c01_022e = replace_na(c01_001e / x0001e, 0),
              c01_025e = replace_na(c01_001e / x0001e, 0)) %>%
  
  step_normalize(income_per_cap_2016, income_per_cap_2017, income_per_cap_2018, 
                 income_per_cap_2019, income_per_cap_2020) %>%
  
  step_normalize(gdp_2016, gdp_2017, gdp_2018, gdp_2019, gdp_2020)

recipe_3 <- recipe(percent_dem ~ ., data = cleaned_train) %>%
  # drop variable id
  step_rm(id) %>%
  
  # impute NA values
  step_impute_mean(income_per_cap_2016, income_per_cap_2017, 
                   income_per_cap_2018, income_per_cap_2019, 
                   income_per_cap_2020, gdp_2016, gdp_2017, 
                   gdp_2018, gdp_2019, gdp_2020) %>%
  
  # modify variable total_votes
  step_mutate(total_votes = total_votes / x0001e) %>%
  
  # modify variable x0002E percent_male
  step_mutate(x0002e = x0002e / x0001e) %>%
  
  # modify variable x0003E percent_female
  step_mutate(x0003e = x0003e / x0001e) %>%
  
  # drop variable x0005E, x0006E, x0007E, x0008E, x0009E, x0010E, 
  # x0011E, x0012E, x0013E, x0014E, x0015E, x0016E, x0017E
  step_rm(x0005e, x0006e, x0007e, x0008e, x0009e, x0010e,
          x0011e, x0012e, x0013e, x0014e, x0015e, x0016e, x0017e) %>%
  
  # modify variable x0019E percent_ineligible_to_vote
  step_mutate(x0019e = x0019e / x0001e) %>%
  
  # drop variable x0020E
  step_rm(x0020e) %>%
  
  # modify variable x0021E percent_eligible_to_vote
  step_mutate(x0021e = x0021e / x0001e) %>%
  
  # modify variable to percetage.
  step_mutate(x0022e = x0022e / x0001e, 
              x0023e = x0023e / x0001e,
              x0024e = x0024e / x0001e) %>%
  
  # modify variable to percetage.
  step_mutate(x0026e = x0026e / x0025e,
              x0027e = x0027e / x0025e) %>%
  
  # drop variable x0025E, repreat of variable x0021E
  step_rm(x0025e) %>%
  
  # modify variable to percetage.
  step_mutate(x0030e = x0030e / x0029e,
              x0031e = x0031e / x0029e) %>%
  
  # drop variable x0029E, repreat of variable x0024E
  step_rm(x0029e) %>%
  
  # modify variable to percetage.
  step_mutate(x0034e = x0034e / x0033e,
              x0035e = x0035e / x0033e) %>%
  
  # drop variable x0033E, repreat of variable x0001E
  step_rm(x0033e) %>%
  
  # modify variable to percetage.
  step_mutate(x0037e = x0037e / x0036e, 
              x0038e = x0038e / x0036e, 
              x0039e = x0039e / x0036e, 
              x0040e = x0040e / x0036e, 
              x0041e = x0041e / x0036e, 
              x0042e = x0042e / x0036e, 
              x0043e = x0043e / x0036e) %>%
  
  # modify variable to percetage.
  step_mutate(x0045e = x0045e / x0044e, 
              x0046e = x0046e / x0044e, 
              x0047e = x0047e / x0044e, 
              x0048e = x0048e / x0044e, 
              x0049e = x0049e / x0044e, 
              x0050e = x0050e / x0044e, 
              x0051e = x0051e / x0044e) %>%
  
  step_mutate(x0044e = x0044e / x0036e) %>%
  
  step_mutate(x0053E = x0053e / x0052e, 
              x0054e = x0054e / x0052e, 
              x0055e = x0055e / x0052e, 
              x0056e = x0056e / x0052e) %>%
  
  step_mutate(x0052e = x0052e / x0036e) %>%
  
  step_mutate(x0057e = x0057e / x0036e) %>%
  
  # drop variable x0036e, repreat of variable x0034e
  step_rm(x0036e) %>%
  
  step_mutate(x0059e = x0059e / x0058e, 
              x0060e = x0060e / x0058e, 
              x0061e = x0061e / x0058e, 
              x0062e = x0062e / x0058e) %>%
  
  # drop variable x0058e, repeat of variable x0035e
  step_rm(x0058e) %>%
  
  step_rm(x0064e, x0065e, x0066e, x0067e, x0068e, x0069e) %>%
  
  step_mutate(x0072e = x0072e / x0071e, 
              x0073e = x0073e / x0071e, 
              x0074e = x0074e / x0071e, 
              x0075e = x0075e / x0071e) %>%
  
  step_mutate(x0071e = x0071e / x0001e) %>%
  
  step_mutate(x0077e = x0077e / x0076e, 
              x0078e = x0078e / x0076e, 
              x0079e = x0079e / x0076e, 
              x0080e = x0080e / x0076e, 
              x0081e = x0081e / x0076e, 
              x0082e = x0082e / x0076e) %>%
  
  step_mutate(x0084e = x0084e / x0083e, 
              x0085e = x0085e / x0083e) %>%
  
  step_mutate(x0083e = x0083e / x0076e) %>%
  
  step_mutate(x0076e =x0076e / x0001e) %>%
  
  step_mutate(x0088e = x0088e / x0087e, 
              x0089e = x0089e / x0087e) %>%
  
  step_mutate(c01_002e = c01_002e / c01_001e, 
              c01_003e = c01_003e / c01_001e, 
              c01_004e = c01_004e / c01_001e, 
              c01_005e = c01_005e / c01_001e) %>%
  
  step_mutate(c01_007e = c01_007e / c01_006e, 
              c01_008e = c01_008e / c01_006e, 
              c01_009e = c01_009e / c01_006e, 
              c01_010e = c01_010e / c01_006e, 
              c01_011e = c01_011e / c01_006e, 
              c01_012e = c01_012e / c01_006e, 
              c01_013e = c01_013e / c01_006e,
              c01_014e = c01_014e / c01_006e, 
              c01_015e = c01_015e / c01_006e) %>%
  
  step_mutate(c01_017e = c01_017e / c01_016e,
              c01_018e = c01_018e / c01_016e) %>%
  
  step_mutate(c01_020e, c01_021e, by = c01_019e) %>%
  
  step_mutate(c01_023e = c01_023e / c01_022e, 
              c01_024e = c01_024e / c01_022e) %>%
  
  step_mutate(c01_026e = c01_026e / c01_025e, 
              c01_027e = c01_027e / c01_025e) %>%
  
  step_rm(c01_016e, c01_019e, c01_022e, c01_025e) %>%
  
  step_normalize(income_per_cap_2016, income_per_cap_2017, income_per_cap_2018, 
                 income_per_cap_2019, income_per_cap_2020) %>%
  
  step_normalize(gdp_2016, gdp_2017, gdp_2018, gdp_2019, gdp_2020)

boost_model_1 <- boost_tree(
  trees = 1507L,
  min_n = 14L,
  tree_depth = 15L,
  learn_rate = 0.020866287,
  loss_reduction = 5.443530e-5,
  sample_size = 0.4569762,
  stop_iter = 15L
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

boost_model_2 <- boost_tree(
  trees = 1891,
  tree_depth = 7,
  learn_rate = 0.01,
  loss_reduction = 2.78e-9,
  sample_size = 0.264
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

poly_model <- svm_poly(
  cost = 0.08630027,
  degree = 2,
  scale_factor = 0.01268114,
  margin = 0.1418384
) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

models = workflow_set(
  preproc = list(mean = recipe_1, regre1 = recipe_2, regre2 = recipe_3),
  models = list(xgboost_1 = boost_model_1, xgboost_2 = boost_model_2, poly_model = poly_model),
  cross = TRUE
)

train_folds <- vfold_cv(cleaned_train, v=10, strata = 'percent_dem')

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

models <- models %>%
  workflow_map(resamples = train_folds,
               control = grid_ctrl)


concrete_stack <- stacks() %>% 
  add_candidates(models)

########################

ens <- blend_predictions(concrete_stack, penalty = 10^seq(-2, -0.5, length = 20))

autoplot(ens)

ens <- fit_members(ens)

reg_metrics <- metric_set(rmse, rsq)

ens_test_pred <- predict(ens, cleaned_train)


ens_test_pred %>% 
  mutate(a = train$percent_dem) %>% 
  summarise(sum((.pred-a)^2)/2331)

ens_test_pred %>% reg_metrics(percent_dem, .pred)

##############################

result <- ens %>% 
  predict(test)

result <- result %>% 
  mutate(id = test$id)

result
write_csv(result, "combine_result.csv")

result
