----------About project-----------------
  
  # In this project, i will use this sample dataset to predict sales
  # variables in the dataset include Id,TV, radio, newspaper, and sales
  
  
  
  #------------importing dataset------------------

data <- read.csv("C:/Users/user/Desktop/kyalo/data science/datasets
                 /sales data.csv")




#---------data cleaning,preparation $ exploration--------------


# cleaning 

# check if there are missing values

library(mice)

md.pattern(data)

head(data)

# preparation

# Remove Id column 

library(tidyverse)

data <- data %>% 
  select(.,-Id)

head(data)

# check for linear relationship between predictors and response

pairs(data)


#-------------------------modelling------------------------------


# training and testing dataset

set.seed(123)

split <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))

train <- data[split==1,]

test <- data[split==2,]

#model using training set

sales.model <- lm(Sales ~ ., data = train)

summary(sales.model)

# tuning our model



# check if there is an interaction(synergy) 

# correlation plot

cor(data)

library(corrplot)

corrplot(cor(data))

# add some interaction term (newspaper * radio)

sales.model <- lm(Sales ~  TV + Radio + Newspaper + Newspaper*Radio,
                  data = train )

summary(sales.model)

# interaction term is not significant

# new model without newspaper and interaction terms
# which have a very large p-value

sales.model <- lm(Sales ~ .-Newspaper,  data = train)

summary(sales.model)

# diagnostic plot

plot(sales.model)

# diagnostic plots show that our model has no potential problems

# accuracy of the model (RSE/mean(response))

accuracy <- (1.702/mean(train$Sales)) *100

accuracy


#----------------------model evaluation-------------------------------


pred.test <- predict(sales.model, test)

plot(test$Sales, pred.test, col="green")

abline(a=0, b=1)

# our model seem to predicting our test data well

# RMSE of our model

RMSE <- sqrt(mean(pred.test-test$Sales)^2)

# the RMSE is smaller than training RSE indicating that our model is
# doing a good job of predicting our test data


#---------------predicting new data using our model---------------------------


# predicting new data and checking prediction interval

TV <- c(100, 60, 80)
Radio <- c(30, 36, 40)
Newspaper <- c(50,30,25)
newdata <- data.frame(TV, Radio, Newspaper)

predict(sales.model, newdata = newdata, interval="prediction")