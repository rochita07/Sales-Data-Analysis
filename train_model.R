# Modeling train data
library(DAAG)

##### Linear Regression #####


##### Modeling ######

summary(Sales)
hist(Sales)
qqnorm(Sales)
ggplot(cleaned_data) + geom_density(aes(x = Sales)) + labs(title = "Desnsity of Sales")

summary(log(Sales))
hist(log(Sales))
qqnorm(log(Sales))
ggplot(cleaned_data) + geom_density(aes(x = log(Sales))) + labs(title = "Desnsity of log(Sales)")


# train and test data

set.seed(1234)
ind = sample(2, nrow(cleaned_data), replace = T, prob = c(0.75, 0.25))
train = cleaned_data[ind == 1, ]
test = cleaned_data[ind == 2, ]
names(train)

#### Only Fixed effect #####

cat_list = c(4, 6, 8, 9, 10, 11, 15, 16, 17) # big model
marginal_pval = NULL
names_cat_list = names(train)[cat_list]

for(i in cat_list)
{
  f = summary(lm(train$log_sales ~ train[, i]))$fstat
  pval = pf(f[1], f[2], f[3], lower.tail = F)
  marginal_pval = c(marginal_pval, pval)
}
round(marginal_pval, 5)
names_cat_list

lm_mod1 = lm(log_sales ~ Ship.Mode + Segment + State + Region + Category + Sub.Category + mon + +wkday + yr, data = train)
anova(stepAIC(lm_mod1)) #

summary(lm_mod1)
lm_mod2 = lm(log_sales ~  State + Region + Category + Sub.Category, data = train)
summary(lm_mod2)
lm_mod3 = stepAIC(lm_mod1)
lm_mod3
anova(lm_mod3) # sub category, State


par(mfrow = c(2, 2))
plot(lm_mod2)
plot(lm_mod3) # Not much diff, can go with lm_mod3
AIC(lm_mod2)
AIC(lm_mod3)

lm_mod4 = lm(log_sales ~ Region + Category, data = train) 
# tried to simplify, but plots are not good
summary(lm_mod4)
plot(lm_mod4)

## Final model = lm_mod3

lm_predict_3 = predict(lm_mod3, test)
lm_predict_3
cbind(test$log_sales, lm_predict_3)
dev.off()
plot(test$log_sales, lm_predict_3, xlab = "log(Sale)", ylab = "log(Predicted Sale)", main="Linear Fixed Effect Model")
cor(test$log_sales, lm_predict_3) # good fit


# Model 2 for comparison with lasso etc

lm_predict_2 = predict(lm_mod2, test)
lm_predict_2
cbind(test$log_sales, lm_predict_2)
plot(test$log_sales, lm_predict_2)
cor(test$log_sales, lm_predict_2) # good fit
lm_mse_2 = mean((test$log_sales - lm_predict_2)^2)
lm_mse_2  # comparable with Lasso etc


##### Lasso, Ridge, Elastic Net  #####


lm_matrix = model.matrix.lm(lm_mod2) # starting from sig marginal p-val
dim(lm_matrix)

fit.lasso <- glmnet(lm_matrix, train$log_sale, family="gaussian", alpha=1, data = train)
#length(abs(coef(fit.lasso)))
fit.ridge <- glmnet(lm_matrix, train$log_sale, family="gaussian", alpha=0, data = train)

fit.elnet <- glmnet(lm_matrix, train$log_sale, family="gaussian", alpha=0.5, data = train) 


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(lm_matrix, train$log_sale, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
x11()
par(mfrow=c(1,2))

# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

best_lasso_coef = as.numeric(coef(fit10, s = fit10$lambda.min))[-1]


fit.lasso.adaptive = glmnet(lm_matrix, train$log_sale, family="gaussian", alpha = 1, penalty.factor = 1 / abs(best_lasso_coef), data = train)
plot(fit.lasso.adaptive, xvar = "lambda")

x.test = model.matrix.lm(lm_mod2, data = test)
dim(x.test)


yhat.lasso <- predict(fit10, s=fit10$lambda.1se, newx=x.test)
yhat.elnet <- predict(fit5, s=fit5$lambda.1se, newx=x.test)
yhat.ridge <- predict(fit0, s=fit0$lambda.1se, newx=x.test)
yhat.lasso.adaptive = predict(fit.lasso.adaptive, newx=x.test)

mse.lasso <- sqrt(mean((test$Sales - exp(yhat.lasso))^2))
mse.elnet <- mean((test$log_sales - yhat.elnet)^2)
mse.ridge <- mean((test$log_sales - yhat.ridge)^2)
mse.lasso.adaptive = mean((test$log_sales - yhat.lasso.adaptive)^2)
mse.lasso
mse.elnet
mse.ridge
mse.lasso.adaptive



dev.off()
x11()
par(mfrow = c(3,1))
plot(test$log_sales, yhat.lasso, main = "Lasso")
plot(test$log_sales, yhat.elnet, main = "Elastic Net")
plot(test$log_sales, yhat.ridge, main = "Ridge")
cbind(test$log_sales, yhat.lasso)

## LAsso is better marginally
#LASSO is the winner! LASSO is good at picking up a small signal through lots of noise.



#### Mixed effect model ####

# start with same fixed effect, and include diff variation of random variables

mod1 = lmer(log_sales ~ State + Sub.Category +  (1|Customer.ID) + (1| State:City), data = train)
mod2 = lmer(log_sales ~ State + Sub.Category + (1| State:City), data = train)
mod3 = lm(log_sales ~ State + Sub.Category, data = train)
mod4 = lmer(log_sales ~ State + Sub.Category +  (1|Customer.ID) + (1|City), data = train)
mod5 = lmer(log_sales ~ State + Sub.Category  + (1| State:City), data = train)
mod6 = lmer(log_sales ~ State + Sub.Category +  (1|Customer.ID), data = train)
mod7 = lmer(log_sales ~ State + Sub.Category  +  (1|City), data = train)

# considering 4 marginally sig variables (bigger models)

# mod11 = lmer(log_sales ~ State + Region + Sub.Category + (1|Customer.ID) + (1| State:City), data = train)
# mod12 = lme4::lmer(log_sales ~ State + Region + Category + Sub.Category  + (1| State:City), data = train)
# mod13 = lm(log_sales ~ State + Region + Category + Sub.Category , data = train)
# mod14 = lmer(log_sales ~ State + Region + Category + Sub.Category  +  (1|Customer.ID) + (1|City), data = train)
# mod15 = lmer(log_sales ~ State + Region + Category + Sub.Category   + (1| State:City), data = train)
# mod16 = lmer(log_sales ~ State + Region + Category + Sub.Category +  (1|Customer.ID), data = train)
# mod17 = lmer(log_sales ~ State + Region + Category + Sub.Category  +  (1|City), data = train)

#lmerTest::step(mod0)
anova(mod1, mod2, mod3, mod4, mod6, mod7) # mod 6 sig

AIC(mod1)
AIC(mod2)
AIC(mod3)
AIC(mod4)
AIC(mod5)
AIC(mod6) # samll AIC
AIC(mod7)

# Prediction and RMSE and Diagnostic Plot

#diagnostic plots
plot(mod6, main = "Fitted vs Residuals", ylab = "Residual", xlab = "Fitted")
par(mfrow = c(1,2))

qqnorm(resid(mod6), main = " QQPlot of Residuals")
cust.re = ranef(mod6)
qqnorm(cust.re$Customer.ID[,1], main = "QQ Plot of Random Customer effect")


mix_fit = lmer(log_sales ~ State + Sub.Category +  (1|Customer.ID), data = test)


mix_pred = fitted(mix_fit)
mix_rmse = mean((test$log_sales - mix_pred)^2)
mix_rmse
plot(test$log_sales, mix_pred, xlab = "log(Sale)", ylab = "log(Predicted Sale)", main="Linear Mixed Effect Model")

#### GLM #####

summary(Sales)
glm_1 = glm(Sales ~ State + Sub.Category, family = gaussian(link="log"), data = train)
glm_1
glm_pred_1 = predict.glm(glm_1, newdata = test, type = "response")

cbind(exp(predict(lm_mod3)), glm_pred_1)


##### GLMER #####

glmer_1 = glmer(Sales ~ State + Sub.Category + (1|Customer.ID), family = gaussian(link="log"), data = train)
glmer_1
glmer_pred_1 = fitted(glmer_1, type = "response")
length(glmer_pred_1)
length(train$Sales)
glmer_rmse = sqrt(mean((exp(glmer_pred_1) - train$Sales)^2))
glmer_rmse
#### Random Forest and feature selection ##### Non para => Sales not log-sales


set.seed(222) 
names(train)

boruta <- Boruta(Sales ~ Ship.Mode + Segment + State + Region + Category + Sub.Category + yr + mon + wkday, data = train, doTrace = 2, maxRuns = 50)
print(boruta)
plot(boruta, las = 2,cex.axis = 0.75)
plotImpHistory(boruta)

getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)



rf_1 <- randomForest(Sales ~ State + Region + Category + Sub.Category, data=train) 

#prunning 

# Error rate of Random Forest
plot(rf_1) # not improving after trees = 50,-- n tree

# Tune mtry
t <- tuneRF(train[,c("State", "Region", "Category", "Sub.Category")], train[,"log_sales"],
            stepFactor = 0.5,
            plot = TRUE, #plot OOB
            ntreeTry = 25, # prev plot
            trace = TRUE,
            improve = 0.05) # small number
t  ## select m try from here, where it is minm

rf_1 = randomForest(Sales ~ State + Region + Category + Sub.Category, data=train, ntree = 25, mtry = 2) 
plot(rf_1)

# No. of nodes for the trees
hist(treesize(rf_1), ##treesize function
     main = "No. of Nodes for the Trees",
     col = "green", xlab = "Tree Size")

# Variable Importance
varImpPlot(rf_1,
           sort = T,
           n.var = 10,
           main = "Variable Importance")
importance(rf_1) #quantitative values
varUsed(rf_1) # which var used how many times


# Extract Single Tree
getTree(rf_1, 1, labelVar = TRUE)

rf_pred_1 = predict(rf_1, test)
rf_rmse_1 = sqrt(mean((test$Sales - rf_pred_1)^2))
rf_rmse_1

### State and Sub.Category


rf_2 <- randomForest(log_sales ~ State + Sub.Category, data=train) 

# Error rate of Random Forest
plot(rf_2) # not improving after trees = 50,-- n tree

# Tune mtry
t <- tuneRF(train[,c("State", "Sub.Category")], train[,"log_sales"],
            stepFactor = 0.5,
            plot = TRUE, #plot OOB
            ntreeTry = 50, # prev plot
            trace = TRUE,
            improve = 0.05) # small number
t  ## select m try from here, where it is minm

rf_2 = randomForest(log_sales ~ State + Sub.Category, data=train, ntree = 50, mtry = 1) 
plot(rf_2)

# No. of nodes for the trees
hist(treesize(rf_2), ##treesize function
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf_2,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf_2) #quantitative values
varUsed(rf_2) # which var used how many times


# Extract Single Tree
getTree(rf_2, 1, labelVar = TRUE)


rf_pred_2 = predict(rf_2, test)
rf_rmse_2 = mean((test$log_sales - rf_pred_2)^2)
rf_rmse_2  ## better


tree1 = rpart(Sales ~ State + Region + Category + Sub.Category, train)
tree1
rpart.plot(tree1, cex = 0.5)
rpart.plot(tree1, extra = 1, cex = 0.4) # gives sample sizes


tr_pred = predict(tree1, test)
tr_rmse = mean((test$log_sales - tr_pred)^2)
tr_rmse



mytree <- ctree(Sales ~ State + Region + Category + Sub.Category, train, controls=ctree_control(mincriterion=0.99, minsplit=500)) 
# min confidence = 0.99 and split only when each branch has 500 samples
print(mytree)
plot(mytree,type="simple")
tree_pred = predict(mytree, test)
tr_rmse = sqrt(mean((tree_pred - test$Sales)^2))
tr_rmse

#### KNN ##### Check later
# KNN Model
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)
set.seed(333)
fit_knn <- train(Sales ~ State + Region + Category + Sub.Category,
             data = train,
             method = 'knn',
             metric = 'Rsquared')#, # for continuous response
             #trControl = trControl)
             #preProc = c('center', 'scale'))
#tuneGrid = expand.grid(k=1:70),

# Model Performance
fit_knn
plot(fit_knn)
varImp(fit_knn)
pred_knn <- predict(fit_knn, newdata = test)
knn_rmse = sqrt(mean((pred_knn - test$Sales)^2))
knn_rmse
plot(log(pred_knn) ~ log(test$Sales))

