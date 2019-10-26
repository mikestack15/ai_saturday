library(xgboost)
library(data.table)
library(mltools)

x <- rnorm(1000)
y <- rnorm(1000)
z <- rnorm(1000)

df1 <- data.frame(x,y,z)


train <- head(df1,as.integer(as.integer(nrow(df1)*.80)))
train_smpSize <- nrow(train)
test_smpSize <- nrow(df1) - nrow(train)
test <- tail(df1,test_smpSize)

train_bkp <- train
test_bkp <- test

setDT(train)
setDT(test)


labels <- train$z
ts_label <- test$z

new_tr <- model.matrix(~.+0,data=train[,-c("z"),with=F])
new_ts <- model.matrix(~.+0,data=test[,-c("z"),with=F])


dtrain <- xgb.DMatrix(data=new_tr,label=labels)
dtest <- xgb.DMatrix(data=new_ts,label=ts_label)


max.depth <- c(3,15,30)
eta <- c(0.01,0.10,0.25)
nround <- c(100,200,1000)


params <- expand.grid("max.depth"=max.depth,"eta"=eta,"nround"=nround)
params <- cbind("index"=seq(1:nrow(params)),params)

training_function <- function(sub) {
  reg_model <- xgb.train(data=dtrain,booster="gbtree",objective = "reg:linear",
                       max.depth=sub[,'max.depth'],eta=sub[,'eta'],nthread=1,
                       nrounds=sub[,'nround'],min_child_weight=1, subsample = 0.5,
                       colsample_bytree = 1, num_parallel_tree = 1, verbose = 0)
  return(reg_model)
}


results_df <- NULL
for(i in unique(params$index)) {
  #i <- 1
  sub <- subset(params, params$index == i)
  reg_model <- training_function(sub)
  
  preds <- predict(reg_model, dtest)
  
  actual <- test$z
  rss <- sum((preds-actual)^2)
  tss <- sum((actual-mean(actual))^2)
  rsq <- 1 - rss/tss
  
  mse_ <- mse(preds = preds, actuals = actual)
  
  
  results_i <- cbind(sub, rsq, mse_)
  
  results_df <- rbind(results_df,results_i)
  print(paste("model id trained: ",i,sep=""))
}

results_df <- results_df[order(-results_df$rsq),]
print(results_df)

final_forecast_df <- data.frame(test_bkp,z_predictions=preds)

