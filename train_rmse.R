## Rmse

#### Only Fixed effect #####

lm_mod3
lm_predict_3 = predict(lm_mod3, test)
lm_mse_3 = mean((test$Sales - exp(lm_predict_3))^2)
lm_mse_3 = sqrt(lm_mse_3)
lm_mse_3

##### Lasso #####

fit10
best_lasso_coef = as.numeric(coef(fit10, s = fit10$lambda.min))[-1]
coef(fit10, s = fit10$lambda.min)
sort(abs(best_lasso_coef), decreasing =  T)
lasso_coef = ifelse(abs(best_lasso_coef)>0, 300, -400)


#### Mixed effect model ####
mix_pred = fitted(mix_fit)
mix_rmse = sqrt(mean((test$Sales - exp(mix_pred))^2))
mix_rmse

#### glm #####

glm_pred_1
glm_rmse = sqrt(mean((test$Sales - glm_pred_1)^2))
glm_rmse

lm_mse_3 # lm
mix_rmse # lmer
glm_rmse # glm fixed effect
rf_rmse_1 # random forest

