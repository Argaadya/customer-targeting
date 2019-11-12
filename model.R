library(tidyverse)
library(caret)
library(tidymodels)
library(rpart)
library(e1071)
library(rattle)
library(rpart.plot)
options(scipen = 100)
df <- fread("sales_data.csv")

df <-  df %>% 
  mutate(education = substr(education, 4, nchar(education)),
         age = substr(age, 3, nchar(age)),
         mortgage = substr(mortgage, 2, nchar(mortgage)))  %>% 
  mutate(flag = if_else(flag == "N", "No", "Yes"),
         online = if_else(online == "N", "No", "Yes"),
         child = if_else(child == "Y", "Yes", "No"),
         age = if_else(age == "Unk", "", age),
         gender = case_when(gender == "F" ~ "Female",
                            gender == "M" ~ "Male",
                            TRUE ~ "")) %>% 
  mutate_if(.predicate = is.character, .funs = funs(na_if(.,""))) %>% 
  mutate_if(.predicate = is.character, .funs = as.factor) %>% 
  select(-c(customer_psy,fam_income,car_prob)) %>% 
  mutate(flag = factor(flag, levels = c("Yes","No"))) %>% 
  na.omit()

# split data
set.seed(100)
intrain <- initial_split(data = df, prop = 0.8, strata = "flag")

# Recipes
rec <- recipe(flag~., training(intrain)) %>% 
               step_downsample(flag, ratio = 1/1, seed = 123) %>% 
               prep(strings_as_factors = F)

# juice and bake
data_train <- juice(rec)
data_test <- bake(rec, testing(intrain))

# decision tree -------------------
set.seed(123)
control = rpart.control(minsplit = 1)
tic()
dtree <- rpart(flag ~ ., data = data_train, method = "class", control = control)
toc()

# Predict
pred_tree <- select(data_test, flag) %>% 
  bind_cols(pred_class = predict(dtree, newdata = data_test %>% select(-flag), type = "class"))

# confusion matrix
pred_tree %>% conf_mat(flag, pred_class)

# performance
perf_dt <- pred_tree %>% 
  summarise(accuracy = accuracy_vec(flag, pred_class), 
            sensitivity = sens_vec(flag, pred_class), 
            specificity = spec_vec(flag, pred_class), 
            precision = precision_vec(flag, pred_class))

# Random Forest --------------------------

# define model spec
model_spec <- rand_forest(mode = "classification", 
                          mtry = 1, 
                          trees = 500, 
                          min_n = 1)

# define model engine
model_spec <- set_engine(model_spec, 
                         engine = "ranger", 
                         seed = 123, 
                         num.threads = parallel::detectCores(), 
                         importance = "impurity")

# model fitting
set.seed(123)
tic()
model <- fit_xy(object = model_spec, 
                x = select(data_train, -flag), 
                y = select(data_train, flag))
toc()

# Predict
pred_rf <- select(data_test, flag) %>% 
  bind_cols(pred_class = predict(model, new_data = data_test %>% select(-flag), type = "class"))

# confusion matrix
pred_rf %>% conf_mat(flag, .pred_class)

# performance
perf_rf <- pred_rf %>% 
  summarise(accuracy = accuracy_vec(flag, .pred_class), 
            sensitivity = sens_vec(flag, .pred_class), 
            specificity = spec_vec(flag, .pred_class), 
            precision = precision_vec(flag, .pred_class))


# Naive Bayes ----------------

df_naive <- df %>% 
  mutate(house_val = as.factor(if_else(house_val >= mean(house_val), "above_average", "below_average")))

# split data
set.seed(100)
intrain2 <- initial_split(data = df_naive, prop = 0.8, strata = "flag")

# Recipes
rec2 <- recipe(flag~., training(intrain2)) %>% 
  step_downsample(flag, ratio = 1/1, seed = 123) %>% 
  prep(strings_as_factors = F)

# juice and bake
data_train_naive <- as.data.frame(juice(rec2))
data_test_naive <- as.data.frame(bake(rec2, testing(intrain2)))

# model
tic()
bayes_mod <- naiveBayes(data_train_naive[,-12], 
                        data_train_naive[,12],
                        laplace = 1)
toc()

# predict
pred_bayes <- select(data_test, flag) %>% 
  bind_cols(pred_class = predict(bayes_mod,
                                 newdata = data_test_naive[,-1],
                                 type = "class"))

# confusion
pred_bayes %>% conf_mat(flag, pred_class)

# performance
perf_bayes <- pred_bayes %>% 
  summarise(accuracy = accuracy_vec(flag, pred_class), 
            sensitivity = sens_vec(flag, pred_class), 
            specificity = spec_vec(flag, pred_class), 
            precision = precision_vec(flag, pred_class))

# decision tree
perf_dt

# naive bayes
perf_bayes

# random forest
perf_rf


save.image("modeling.RData")


# NN

