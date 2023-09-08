
library(dplyr)
library(tidymodels)
library(tidyverse)
library(rpart)
library(xgboost)

set.seed(251637)

test <- read.csv("test.csv")
train <- read.csv("train.csv")

cleaned_train <- train %>% 
  select(-name)

boost_recipe <- recipe(percent_dem ~ ., data = cleaned_train) %>%
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

boost_model<- boost_tree(
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

boost_workflow <- workflow() %>% 
  add_model(boost_model) %>% 
  add_recipe(boost_recipe)

boost_workflow_fit <- boost_workflow %>%
  fit(train)

boost_predict <- boost_workflow_fit %>%
  predict(test)

boost_predict_with_id <- test %>%
  select(id) %>%
  bind_cols(boost_predict)

boost_predict_with_id <- boost_predict_with_id %>%
  select(id, percent_dem = .pred)


write_csv(boost_predict_with_id, "final.csv")