library(tidyverse)
library(tidymodels)
library(AmesHousing)
library(broom)
#install.packages('iml')
library(iml)
ames_df <- make_ames()
ames_df <- ames_df %>% 
  select_if(is.numeric) %>% 
  select(Sale_Price, everything())

head(ames_df)
str(ames_df)

linear_model <- lm(Sale_Price~., data = ames_df)
#Coef plot
linear_model %>% 
  tidy(conf.int = TRUE) %>% 
  slice(-1) %>% 
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_point() + 
  coord_flip() + 
  scale_y_log10()


#Create an effect plot
ames_df %>% 
  mutate(pk = row_number()) %>% 
  gather(key = "key", value = "value", -Sale_Price, -pk) %>% 
  left_join(linear_model %>% 
              tidy() %>% 
              slice(-1) %>% 
              select(term, estimate),
            by = c("key" = "term")) %>% 
  mutate(effect = value*estimate,
         pos_neg = if_else(effect <= 0, "Negative", "Positive"),
         effect = abs(effect)) %>% 
  ggplot(aes(x = key, y = effect, color = pos_neg)) + 
  geom_boxplot() + 
  coord_flip() + 
  scale_y_log10()


# Start making some non-interpretable models 
random_forest_model <- rand_forest() %>% 
  set_mode("regression") %>% 
  set_engine("randomForest")
xgb_model <- boost_tree() %>% 
  set_mode("regression") %>% 
  set_engine("xgboost")


#Train models
set.seed(42)
random_forest_model <- random_forest_model %>% 
  fit(Sale_Price~., data = ames_df)
xgb_model <- xgb_model %>% 
  fit(Sale_Price~., data = ames_df)


x_data <- ames_df %>% select(-Sale_Price) %>% as.data.frame()
rf_predictor <- Predictor$new(random_forest_model, data = x_data, y = ames_df$Sale_Price)
xgb_predictor <- Predictor$new(xgb_model, data = x_data, y = ames_df$Sale_Price)
rf_feature_imp <- FeatureImp$new(rf_predictor, loss = "mae")
xgb_feature_imp <- FeatureImp$new(xgb_predictor, loss = "mae")


rf_feature_imp %>% plot()


xgb_feature_imp %>% plot()

rf_ale <- FeatureEffects$new(rf_predictor)
xgb_ale <- FeatureEffects$new(xgb_predictor)

#install.packages('patchwork')
#library(patchwork)
rf_ale %>% plot()

#Lime model 
#For analyzing how models make individual predictions 
rf_lime <- LocalModel$new(rf_predictor, x.interest = x_data[1,])
rf_lime %>% plot()

xgb_lime <- LocalModel$new(xgb_predictor, x.interest = x_data[1,])
xgb_lime %>% plot()
