library(dplyr)
library(lubridate)
library(sn)

#set working directory
setwd("/Users/mike/Desktop/mikest_wd")
#initiate data.frame
dining_data <- NULL
instances <- 1000
dining_data$Experience_id <- seq(1,instances)
dining_data <- as.data.frame(dining_data)

#table party size
dining_data$table_party_size <- round(runif(1000,1,8),0)
#dining order $ amount
dining_data$order_dollar_amount <- round(runif(1000,20,60),2) * dining_data$table_party_size
#overall review
dining_data$overall_review <- round(runif(1000,1,5),1)
#duration of stay (mins)
dining_data$duration_of_stay <- round(runif(1000,30,157),2)
#table wait-time (mins)
dining_data$table_wait_time <- round(runif(1000,0,45),0)

