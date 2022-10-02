# train data
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(lme4)
library(MASS)
library(glmnet)
library(lmerTest)
library(randomForest)
library(Boruta)
library(rpart)
library(rpart.plot)
library(forecast)
library(quantmod)
library(ncvreg)

library(caret)
library(pROC)
library(mlbench)
library(Kendall)
library(arulesViz)
library(arules)
library(readxl)

data = read.csv(file.choose())
data
dim(data)
names(data)
str(data)


# Missing data
apply(data, 2, function(x) sum(is.na(x)))

 # missing only in postal code, and in analysis we exclude postal code, so not deleting


cleaned_data = data %>% 
  mutate(Row.ID = NULL,
         Postal.Code = NULL,
         Country = NULL,
         Customer.Name = NULL,
         Product.ID = NULL)

cleaned_data = cleaned_data %>% 
               mutate(across(where(is.character), as.factor))

cleaned_data = cleaned_data %>% 
               mutate(Order.Date = dmy(Order.Date),
                      Ship.Date = dmy(Ship.Date),
                      log_sales = log(Sales))
                      
cleaned_data = cleaned_data %>% 
               mutate(mon = as.factor(month(Order.Date, label = T)),
                      wkday = as.factor(weekdays(Order.Date)),
                      yr = as.factor(year(Order.Date)),
                      yr_mon = as.factor(paste(yr, mon, sep =":")))


str(cleaned_data)
head(cleaned_data)
dim(cleaned_data)
attach(cleaned_data)

### --------------------------------------------------
##### EDA #####


summary(Sales)  

summary(Sales)
par(mfrow = c(1,1))
hist(Sales)
qqnorm(Sales, main = "QQplot of Sales")
ggplot(cleaned_data) + geom_density(aes(x = Sales)) + labs(title = "Desnsity of Sales", y = "Density")

summary(log(Sales))
hist(log(Sales))
qqnorm(log(Sales), main = "QQplot of log(Sales)")
 ggplot(cleaned_data) + geom_density(aes(x = log(Sales))) + labs(title = "Desnsity of log(Sales)", y = "Density")


##### Effect of time and date #####


## year

yr_grp = cleaned_data %>% 
           group_by(yr) %>% 
           summarise(yr_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
           arrange(desc(yr_avg))
yr_grp 


ggplot(yr_grp) + geom_bar(aes(x = yr, y = yr_avg), stat = 'identity') + labs(x = "Year", y = "Average Sale", title = "Year wise average sales")


# ggplot(cleaned_data) + geom_violin((aes(x = as.factor(yr), y = Sales)))

# exc_outlier = cleaned_data %>% filter(Sales < 1000)
# dim(exc_outlier)
# boxplot(Sales ~ yr, data = exc_outlier)
# 
# 
# cleaned_data %>% filter(Sales<1000) %>%
# ggplot(aes(x= as.Date(Order.Date), y= Sales)) +
#   geom_line() + xlab("")

# No specific trend, may be only seasonal effect

# yr_suspect = cleaned_data %>% group_by(yr) %>% filter(Sales > 5000) # not sure


## Month 

mon_grp = cleaned_data %>% 
  group_by(mon) %>% 
  summarise(mon_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(mon_avg))
mon_grp 

ggplot(mon_grp) + geom_bar(aes(x = mon, y = mon_avg), stat = 'identity') + labs(x = "Month", y = "Average Sale", title = "Month wise Average sales")

# exc_outlier = cleaned_data %>% filter(Sales < 500) 
# dim(exc_outlier)
# boxplot(Sales ~ mon, data = exc_outlier)
# ggplot(mon_exc_outlier) + geom_violin((aes(x = mon, y = Sales)))


## daily

#ord_date = as.Date(Order.Date,format = "%Y-%m-%d") # # Change class of date column 

day_grp = cleaned_data %>% arrange(Order.Date) %>% group_by(Order.Date) %>%
         summarise(daily_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales))
day_grp 

 summary(day_grp$daily_avg)
# 
# exc_outlier = day_grp %>% filter(daily_tot < 300) 
#  dim(exc_outlier) 
#  length(day_grp$daily_avg) ## total obs  

ggplot(day_grp, aes(x= Order.Date, y = daily_avg)) + geom_line() +
   xlab("") + labs(y = "Average Sale", title = "Daily average Sales")

#ggplot(exc_outlier, aes(x= Order.Date, y = daily_avg)) + geom_line() + xlab("")

## conclusion: no trend is present
## day_grp could be our time series data

yr_mon_grp = cleaned_data %>% group_by(yr, mon) %>% summarise(yr_mon_avg = mean(Sales)) %>% arrange(yr, mon)
yr_mon_grp 


## weekdays

wk_grp = cleaned_data %>% 
  group_by(wkday) %>% 
  summarise(wk_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(wk_avg))
wk_grp 

ggplot(wk_grp) + geom_bar(aes(x = wkday, y = wk_avg), stat = 'identity') + labs(x = "Weekdays", y = "Average Sale", title = "Weekday wise average sales") +  coord_flip()

# wk_exc_outlier = cleaned_data %>% filter(Sales < 600) 
# dim(wk_exc_outlier)
# boxplot(Sales ~ wkday, data = wk_exc_outlier)
# ggplot(mon_exc_outlier) + geom_violin((aes(x = mon, y = Sales)))


##### segment #####

seg_grp = cleaned_data %>% 
  group_by(Segment) %>% 
  summarise(seg_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(seg_avg))
seg_grp 

ggplot(seg_grp) + geom_bar(aes(x = Segment, y = seg_avg), stat = 'identity') + labs(x = "Segment", y = "Average Sale", title = "Segment wise average sales")

# exc_outlier = cleaned_data %>% filter(Sales < 500) 
# dim(exc_outlier)
# boxplot(Sales ~ Segment, data = exc_outlier)
# ggplot(exc_outlier) + geom_violin((aes(x = Segment, y = Sales)))


##### City #####

city_grp = cleaned_data %>% 
  group_by(City) %>% 
  summarise(city_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(city_avg))
city_grp 

# top 20 city

top20_city = city_grp  %>% 
  top_n(n = 10, wt = city_avg)
top20_city

top20_city %>% 
  mutate(City = fct_reorder(City, city_avg)) %>% 
  ggplot(aes(City, city_avg))+
  geom_point()+
  labs(y = "Average Sale", title = "City wise average sale") + coord_flip()


##### sate #####

state_grp = cleaned_data %>% 
  group_by(State) %>% 
  summarise(state_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(state_avg))
state_grp 

# top 20 state

top10_state = state_grp  %>% 
  top_n(n = 10, wt = state_avg)
top10_state

top10_state %>% 
  mutate(State = fct_reorder(State, state_avg)) %>% 
  ggplot(aes(State, state_avg))+
  geom_point()+
  labs(y = "Average Sale", title = "State wise average sale") + coord_flip()


##### Region #####

reg_grp = cleaned_data %>% 
  group_by(Region) %>% 
  summarise(reg_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(reg_avg))
reg_grp 

ggplot(reg_grp) + geom_bar(aes(x = Region, y = reg_avg), stat = 'identity') + labs(x = "Region", y = "Average Sale", title = "Region wise average sales")


# exc_outlier = cleaned_data %>% filter(Sales < 500) 
# dim(exc_outlier)
# boxplot(Sales ~ Region, data = exc_outlier)
# ggplot(exc_outlier) + geom_violin((aes(x = Region, y = Sales)))


##### Category #####

cat_grp = cleaned_data %>% 
  group_by(Category) %>% 
  summarise(cat_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(cat_avg))
cat_grp 

ggplot(cat_grp) + geom_bar(aes(x = Category, y = cat_avg), stat = 'identity') + labs(x = "Category", y = "Average Sale", title = "Category wise average sales")

# exc_outlier = cleaned_data %>% filter(Sales < 500) 
# dim(exc_outlier)
# boxplot(Sales ~ Category, data = cleaned_data)
# ggplot(exc_outlier) + geom_violin((aes(x = Category, y = Sales)))


##### Sub.Category #####


subcat_grp = cleaned_data %>% 
  group_by(Sub.Category) %>% 
  summarise(subcat_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(subcat_avg))
subcat_grp 

# all Sub Category

top10_subcat = subcat_grp  %>% 
  top_n(n = 10, wt = subcat_avg)
top10_subcat

top10_subcat %>% 
  mutate(Sub.Category = fct_reorder(Sub.Category, subcat_avg)) %>% 
  ggplot(aes(Sub.Category, subcat_avg))+
  geom_point()+
  labs(y = "Average Sale", title = "SubCategory wise average sale") + coord_flip()

##### Customer #####

cust_grp = cleaned_data %>% 
  group_by(Customer.ID) %>% 
  summarise(cust_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(cust_avg))
cust_grp 

# top 20 Customer

top20_cust = cust_grp  %>% 
  top_n(n = 10, wt = cust_avg)
top20_cust

top20_cust %>% 
  mutate(Customer.ID = fct_reorder(Customer.ID, cust_avg)) %>% 
  ggplot(aes(Customer.ID, cust_avg))+
  geom_point()+
  labs(y = "Average Sale", title = "Customer wise average sale") + coord_flip()

# 1st = Home office, 2nd = Corporate, each from diff location

##### Shipping mode #####

ship_grp = cleaned_data %>% 
  group_by(Ship.Mode) %>% 
  summarise(ship_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales)) %>% 
  arrange(desc(ship_avg))
ship_grp 

ggplot(ship_grp) + geom_bar(aes(x = Ship.Mode, y = ship_avg), stat = 'identity') + labs(x = "Category", y = "Average Sale", title = "Shipping mode wise average sales")

# exc_outlier = cleaned_data %>% filter(Sales < 1000) 
# dim(exc_outlier)
# boxplot(Sales ~ Ship.Mode, data = exc_outlier)
# ggplot(exc_outlier) + geom_violin((aes(x = Ship.Mode, y = Sales)))


##### Category , SubCategory


