#Author: Mike Stack
#Last Revised Date: 10/26/2019
#Script Description: Simple predictive model using extreme gradient boosting trees to predict
#the overall review of a given dining experience


#load necessary libraries
library(xgboost)
library(data.table)
library(mltools)
library(dplyr)


#read in data
dining_experience_data <- read.csv("dining_experience_data.csv",as.is=TRUE)

#split into training/testing set
train <- head(dining_experience_data,as.integer(as.integer(nrow(dining_experience_data)*.80)))
train_smpSize <- nrow(train)
test_smpSize <- nrow(dining_experience_data) - nrow(train)
test <- tail(dining_experience_data,test_smpSize)

#create backup copies of original data splits
train_bkp <- train
test_bkp <- test

#remove index from array
train <- train[,-1]
test <- test[,-1]


#move target variable column to first column index position
train <- train[c("overall_review","table_party_size","order_dollar_amount","duration_of_stay",
                 "table_wait_time")]
test <- test[c("overall_review","table_party_size","order_dollar_amount","duration_of_stay",
                 "table_wait_time")]

setDT(train)
setDT(test)


#building average review model
labels <- train$overall_review
ts_label <- test$overall_review

new_tr <- model.matrix(~.+0,data=train[,-c("overall_review"),with=F])
new_ts <- model.matrix(~.+0,data=test[,-c("overall_review"),with=F])

#set in xgboost matrix form
dtrain <- xgb.DMatrix(data=new_tr,label=labels)
dtest <- xgb.DMatrix(data=new_ts,label=ts_label)

#grid search parameters
max.depth <- c(3,15,30)
eta <- c(0.01,0.10,0.25)
nround <- c(100,200,1000)

#grid search index array
params <- expand.grid("max.depth"=max.depth,"eta"=eta,"nround"=nround)
params <- cbind("index"=seq(1:nrow(params)),params)

#training function
training_function <- function(sub) {
  reg_model <- xgb.train(data=dtrain,booster="gbtree",objective = "reg:linear",
                         max.depth=sub[,'max.depth'],eta=sub[,'eta'],nthread=1,
                         nrounds=sub[,'nround'],min_child_weight=1, subsample = 0.5,
                         colsample_bytree = 1, num_parallel_tree = 1, verbose = 0)
  return(reg_model)
}

#run training function and score results of model (mse/r^2) against test set
results_df <- NULL
for(i in unique(params$index)) {
  #i <- 1
  sub <- subset(params, params$index == i)
  reg_model <- training_function(sub)
  preds <- predict(reg_model, dtest)
  
  actual <- test$overall_review
  #compare predictions against test set
  rss <- sum((preds-actual)^2)
  tss <- sum((actual-mean(actual))^2)
  rsq <- 1 - rss/tss
  
  mse_ <- mse(preds = preds, actuals = actual)
  
  results_i <- cbind(sub, rsq, mse_)
  
  results_df <- rbind(results_df,results_i)
  print(paste("model id trained: ",i,sep=""))
}

#order model results index by most accurate
results_df <- results_df[order(-results_df$rsq),]
print(results_df)
#best combination of hyperparamters
best_model_grid <- head(results_df,1)


#retrain best model
best_model_index <- best_model_grid$index
sub <- subset(params, params$index == best_model_index)
reg_model <- training_function(sub)
preds <- predict(reg_model, dtest)

#final output dataframe
final_forecast_df <- data.frame(test_bkp,review_predictions=preds)
final_forecast_df <- final_forecast_df[,c("Experience_id","review_predictions")]

#join original review results 
final_forecast <- left_join(final_forecast_df,test_bkp, by = "Experience_id")
