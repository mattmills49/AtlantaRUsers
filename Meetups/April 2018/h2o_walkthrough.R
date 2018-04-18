library(h2o)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(expappr)
h2o.init(nthreads = 6, max_mem_size = "8g", min_mem_size = "1g")
# Load in basic data frame
df <- iris
df_h2o <- as.h2o(df, "df_h2o")
class(df_h2o)
str(df_h2o)

# From the Kaggle Loan Data Set
clean_loan_data <- readRDS("~/Documents/Presentations/h2o_pres/clean_loan_data.RDS")

##########################
#         GLM            #
##########################
h2o.clusterInfo()

loan_h2o <- as.h2o(clean_loan_data, destination_frame = "loan_h2o")
dim(loan_h2o)

default_glm_plain <- h2o.glm(x = 1:46,
                             y = "bad_loan",
                             training_frame = "loan_h2o",
                             family = "binomial",
                             lambda = 0) # must do this to avoid regularized regression

class(default_glm_plain)
str(default_glm_plain@parameters)
default_glm_plain@model$training_metrics@metrics[c("MSE", "RMSE", "AUC", "logloss", "Gini")]

default_glm_lasso <- h2o.glm(x = 1:46,
                             y = "bad_loan",
                             training_frame = "loan_h2o",
                             model_id = "default_glm_lasso",
                             nfolds = 4,
                             family = "binomial",
                             lambda_search = T,
                             nlambdas = 50,
                             alpha = 1) # 1 is lasso, 0 is ridge

mean(default_glm_lasso@model$coefficients == 0)
# [1] 0.9092559
##########################
#         GBM            #
##########################

## Splitting Data into Train / Val

train <- runif(n = nrow(loan_h2o)) < .75
train_rows <- seq_len(nrow(loan_h2o))[train]
test_rows <- seq_len(nrow(loan_h2o))[!train]
loan_h2o_train <- loan_h2o[train_rows, ]
loan_h2o_test <- loan_h2o[test_rows, ]

h2o.getId(loan_h2o_test)

default_gbm <- h2o.gbm(x = 1:46, 
                       y = "bad_loan",
                       training_frame = loan_h2o_train, # can use R reference
                       validation_frame = "RTMP_sid_9cfc_4", # or h2o ID
                       score_each_iteration = T,
                       distribution = "bernoulli",
                       ntrees = 500, 
                       max_depth = 3,
                       learn_rate = .1,
                       sample_rate = 2/3,
                       col_sample_rate_per_tree = 2/3,
                       categorical_encoding = "Enum")

training_accuracy <- default_gbm@model$scoring_history
training_accuracy <- h2o.scoreHistory(default_gbm)

training_accuracy %>%
  select(number_of_trees, training_logloss, validation_logloss) %>%
  tidyr::gather(key = "source", value = "logloss", -number_of_trees) %>%
  mutate(source = stringr::str_replace_all(source, "_logloss", "")) %>%
  ggplot(aes(x = number_of_trees, y = logloss, color = source)) +
  geom_point() +
  geom_line() +
  xlab("Tree Number") +
  ylab("Log Loss") +
  ggtitle("GBM Training Performance") +
  scale_color_expapp(discrete = T, n = 4, name = "Source") +
  theme_expapp()
ggsave("~/Documents/Presentations/h2o_pres/traininghistory.png", width = 6, height = 4.5, units = "in")

default_gbm@model$run_time / (1000 * 60)

##########################
#      Data Splits       #
##########################

data_splits <- h2o.splitFrame(loan_h2o, 
                              ratios = c(.6, .3), # .1 for the last
                              destination_frames = c("loan_split_train", # 60%
                                                     "loan_split_test",  # 30%
                                                     "loan_split_val"))  # 10%

default_gbm_train <- h2o.gbm(x = 1:46, 
                       y = "bad_loan",
                       training_frame = "loan_split_train",
                       validation_frame = "loan_split_test", 
                       score_each_iteration = T,
                       distribution = "bernoulli",
                       ntrees = 500, 
                       max_depth = 3,
                       learn_rate = .1,
                       sample_rate = 2/3,
                       col_sample_rate_per_tree = 2/3,
                       categorical_encoding = "Enum",
                       stopping_rounds = 10, ## defaults to LogLoss
                       stopping_tolerance = .01)

default_gbm_train@model$model_summary
default_gbm_train@model$run_time / ( 1000 * 60)

default_gbm_val_preds <- predict(default_gbm, newdata = "loan_split_val")
default_gbm_val_preds <- predict(default_gbm, newdata = h2o.getFrame("loan_split_val"))
default_gbm_val_preds <- predict(default_gbm, newdata = data_splits[[3]])

head(default_gbm_val_preds)
class(default_gbm_val_preds)
# as.data.frame()
##########################
#      Grid Search       #
##########################

gbm_hyper_params <- list(ntrees = c(500, 1000, 1500), learn_rate = c(.01, .1))
# 3 * 2 * 4 = 24 models

default_gbm_grid <- h2o.grid(algorithm = "gbm",
                             x = 1:46,
                             y = "bad_loan",
                             training_frame = loan_h2o,
                             hyper_params = gbm_hyper_params, 
                             is_supervised = T,
                             nfolds = 4,
                             max_depth = 4,
                             sample_rate = 2/3,
                             col_sample_rate_per_tree = 2/3,
                             categorical_encoding = "Enum",
                             stopping_rounds = 10,
                             stopping_tolerance = .01)

class(default_gbm_grid)
View(default_gbm_grid@summary_table)
best_model_id <- default_gbm_grid@summary_table$model_ids[1]

##########################
#  Variable Importance   #
##########################

var_importance <- h2o.varimp(h2o.getModel(best_model_id))

var_importance %>%
  mutate(variable = reorder(variable, percentage)) %>%
  ggplot(aes(x = variable, y = percentage)) +
  geom_col() +
  xlab("") +
  scale_y_continuous(name = "Error Reduction Percentage", labels = function(x) paste0(x * 100, "%")) +
  ggtitle("Most Important Variables") +
  theme_bw() +
  coord_flip()
ggsave(filename = "~/Documents/Presentations/h2o_pres/varimp.png", width = 8, height = 8, units = "in")

##########################
#     Partial Plot       #
##########################

# h2o.partialPlot(h2o.getModel(best_model_id), data = loan_h2o, cols = "recoveries")



