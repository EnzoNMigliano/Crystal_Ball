####### Crystal Ball - Predicting 30 minutes in the future for FAANG Stocks - Machine Learning Stock Model ########
####### Authors #########

# Andrea Gonzales 

# Enzo Novi Migliano

# Raul Ramon


###### Necessary packages ##########



require(tidyverse)

require(forecast) # time series

require(earth) # MARS 

require(h2o)
require(stringr)
require(earth)
require(dplyr)
require(jsonlite)
require(caret)
require(ggplot2)
require(zoo)
require(caTools)
require(gmodels)
require(class)
require(corrplot)
require(Metrics)
require(olsrr)
require(splines)
require(cli)
require(readr)
require(kernlab)

########## Global Functions ############

past.30.min.sd <- function(vector){
  
  past_30_min_diff <- c()
  
  past_30_min_diff[1:30] <- 0
  
  for(i in 31:length(vector)){
    
    data_current <- vector[(i-30):i]
    
    past_30_min_diff[i] <- sd(data_current)
    
    
  }
  
  return(past_30_min_diff)
  
} # end of past.30.min.sd


require()
past.30.min.mean <- function(vector){
  
  past_30_min_diff <- c()
  
  past_30_min_diff[1:30] <- 0
  
  for(i in 31:length(vector)){
    
    data_current <- vector[(i-30):i]
    
    past_30_min_diff[i] <- mean(data_current)
    
    
  }
  
  return(past_30_min_diff)
  
} # end of past.30.min.mean



get.sample.days <- function(number_days){
  
  
  days_data <- list()
  
  j <- 1
  
  for(i in 0:number_days){ # for loop to get the calendar days according to the user's parameter
    
    
    if(i ==0){
      
      days_data[[j]] <- Sys.Date()
      
    }else{
      
      days_data[[j]] <- Sys.Date() - i # decreasing the date at each iteration
      
    }
    
    j <- j + 1
  }
  
  
  keep_day <- c()
  
  for(i in 1:length(days_data)){
    
    todays_date <- weekdays(days_data[[i]])
    
    calendar_date <- days_data[[i]]
    
    
    if(todays_date == "Saturday" | todays_date == "Sunday"){ # The stock market does not work in the weekends
      
      keep_day[i] <- "No"
      
    }else if(format(calendar_date, "%m-%d") %in% c("01-01", #New Years Day
                                                   "01-18", #Martin Luther King Jr. Day
                                                   "02-15", #Presidents Day
                                                   "04-10", #Good Friday
                                                   "05-25", #Memorial Day
                                                   "07-04", #Day before Independence Day
                                                   "07-05", #Independence Day
                                                   "09-07", #Labor Day
                                                   "11-26", #Thanksgiving Day
                                                   "11-27", #Day after Thanksgiving Day
                                                   "12-24", #Day before Christmas
                                                   "12-25") ){
      
      keep_day[i] <- "No"
      
    }else{
      
      keep_day[i] <- "Yes" # If the calendar date is in the week and not a holiday we will keep it
      
    }
    
  }
  
  final_list <- list()
  
  j <- 1
  
  for(i in 1:length(keep_day)){
    
    if(keep_day[i] == "Yes"){ # Keep only the week days
      
      final_list[[j]] <- days_data[[i]]
      
      j <- j + 1
      
    }
    
    
  }
  
  
  return(final_list) # returning list with the calendar days
  
} # end of get.sample.days



stock.data <- function(stocks, time_period, tiingo_token){ # stccks will take a vector with the name of the stocks #time_priod how many days back in time 
  
  require(tidyverse)
  
  list_of_stock_data <- list() # The data of the stocks will be saved here
  
  dates_stock_open <- get.sample.days(time_period) #
  
  
  for(i in 1:length(stocks)){ # iteration for all the stocks given by the users
    
    
    j <- 1
    
    
    while(j < length(dates_stock_open)){
      
      if(j == 1 && length(dates_stock_open)%/% 2 != 0){
        
        url <- paste("https://api.tiingo.com/iex/",
                     stocks[i],"/prices?token=",tiingo_token,"&startDate=", dates_stock_open[[j]],"&resampleFreq=1min&format=csv", sep = "" ) # url link for retrieving the data
        
        
        temp_data <- as.data.frame(read_csv(url))
        
        
        j <- j + 1
        
        
      }else if(j == 1 && length(dates_stock_open)%/% 2 == 0){
        
        url <- paste("https://api.tiingo.com/iex/",
                     stocks[i],"/prices?token=",tiingo_token,"&startDate=", dates_stock_open[[(j + 1)]],"&endDate=", dates_stock_open[[(j)]],"&resampleFreq=1min&format=csv", sep = "" ) # url link for retrieving the data
        
        
        temp_data <- as.data.frame(read_csv(url))
        
        j <- j + 2
        
      } else{
        
        url <- paste("https://api.tiingo.com/iex/",
                     stocks[i],"/prices?token=",tiingo_token,"&startDate=", dates_stock_open[[(j + 1)]],"&endDate=", dates_stock_open[[j]],"&resampleFreq=1min&format=csv", sep = "" ) # url link for retrieving the data
        
        
        temp_data2 <- as.data.frame(read_csv(url)) # reading data from tiingo
        
        
        temp_data <- rbind(temp_data, temp_data2) # Row bind the two data sets 
        
        
        
        j <- j + 2
        
        
        
      }
      
      
    }
    
    
    temp_data <- temp_data %>% arrange(date)
    
    temp_data <- mutate(temp_data,
                        time = format(date, "%H:%M:%S"), # time of the recorded data
                        day = format(date, "%Y-%m-%d"), # calendar date of the recorded data
                        name = stocks[i], # Name of the stock
                        diff = close - open, # differences between the opening pricee and the closing price of tte stock
                        percentage_diff = ((close - open)/close) * 100, # calculaitng the percentage difference
                        market_sentment = (percentage_diff * volume)/100, # Variable measuring the interaction of difference and the volume of the stock
                        last_30_min_mean_diff = past.30.min.mean(diff), # uses the function past.30.min.mean to get the mean of the precious 30 data points of the variabe diff
                        last_30_min_sd_diff = past.30.min.sd(diff), # same as the previous variable but for the Standard deviation
                        last_30_min_variance_diff = (last_30_min_sd_diff)^2) # same as the previous but for the variance
    
    
    list_of_stock_data[[i]] <- temp_data # We are getting only the data from the 30 min and on since the columns last_30_min_*** start after the initial 30 min
    
  }
  
  return(list_of_stock_data)
  
} # end of Stock.data

exporting.data.to.csv <- function(list){ # this function will be utilized to perform h2o computation
  
  
  for(i in 1:length(list)){ # it exports data to the current directory
  
  write.csv(list[[i]], paste(stocks[i],".csv", sep = ""))
  
  }
  
  
  
} # end of exporting data t csv



ts.split <- function(df){ #this function will take a data frame and return it splited according to the date
  
  onemonth <- df$date[nrow(df)]-1814400
  
  test <- df %>% filter(date >= onemonth) # the lastest one month
  
  
  test$sample <- FALSE  
  
  
  train <- df %>% filter(date < onemonth) # the rest of the data
  
  
  train$sample <- TRUE  
  
  
  data_set_final <- rbind(train, test)
  
  
  return(data_set_final) # returns the new data frame with the sample column 
  
  # True is for train and false is for test
  
} # end of ts.split



classifying.30.min <- function(df){ # this function will classify the time series and the MARS predictions
  
  # it will also calculate the accuracy and other metrics
  
  names <- names(df)
  
  
  test_rmse <- rmse(df$diff, df$test_diff) # geting the rmse 
  
  
  diff_test_30 <- rollsum(df$test_diff, k = 30) # calculating the sum of the differences for the last 30 minutes
  
  
  
  
  diff_predicted_30 <-  rollsum(df$diff, k = 30)
  
  
  
  
  diff_test_30_pct <- (diff_test_30/lag(df$close, n= 30))*100 # caculatingt he percentage differences
  
  diff_predicted_30_pct <- (diff_predicted_30/lag(df$close, n= 30))*100  
  
  
  df$diff_test_30_pct <- diff_test_30_pct # inserting a new column with the results
  
  df$diff_predicted_30_pct <- diff_predicted_30_pct
  
  df <- df[30:nrow(df),] # discarting the NA
  
  df$diff_test_30 <- diff_test_30
  
  df$diff_predicted_30 <- diff_predicted_30
  
  
  df <- df %>% select(diff_predicted_30_pct, diff_test_30_pct)
  
  df <- df[2:nrow(df),] # discarting the last NA
  
  
  # transforming  the perrcentage difference into classes
  
  df<- df %>% mutate(diff_predicted_30_pct_class =
                       factor(ifelse(diff_predicted_30_pct >= 5, "up", (ifelse(diff_predicted_30_pct <= 2, "down", "same"))), levels = c("up", "down", "same")))
  
  df<- df %>% mutate(diff_test_30_pct_class =
                       factor(ifelse(diff_test_30_pct >= 5, "up", (ifelse(diff_test_30_pct <= 2, "down", "same"))), levels = c("up", "down", "same")))
  
  
  # getting the metrics
  
  accuracy <- accuracy(df$diff_test_30_pct_class,
                       df$diff_predicted_30_pct_class)
  
  confusion_Matrix <- table(df$diff_test_30_pct_class,
                            df$diff_predicted_30_pct_class)
  
  
  precision <- (sum(diag(confusion_Matrix))/(sum(confusion_Matrix)))
  
  results <- c(accuracy, precision, test_rmse)
  
  return(results)
  
} # end of classifying.30.min

forecast.timeseries <- function(df){
  
  data_ts <- ts.split(df) # used to split the sample in training 
  
  test <- subset(data_ts, sample == FALSE)
  train <- subset(data_ts, sample == TRUE)
  
  train <- train$diff # getting only the difference between stocks for the time series model
  
  fb_ts <- ts(train, start = 1, end=24, frequency = 60) # start in one hour, ends in 24 hours and at a frequency of 60 minutes per hour
  
  modelts <- auto.arima(fb_ts) # builds the arima model
  
  forecast_ts <- forecast(modelts, h = nrow(test))
  
  predictionsts <- as.data.frame(forecast_ts) # gets the foretasted values
  
  diff <- predictionsts$`Point Forecast`
  
  data_frame_ts <- as.data.frame(diff)
  
  test_diff <- as.vector(test$diff)
  
  data_frame_ts$actual <- test_diff
  
  close <- as.vector(test$close)
  
  data_frame_ts$close <- close # the data_frame_ts has the forescated, the actual and the closing values
  
  tf_final <- classifying.30.min(data_frame_ts) # used the function classifying.30.min to get the metrics
  
  return(tf_final)
  
}# end of forecast.timeseries

predict.mars <- function(df){
  
  
  # Splitting the sample in traing and testing set
  
  
  
  set.seed(123)
  df$sample <- sample.split(df$diff, SplitRatio = .75)
  
  df_continous <- df %>% select(-date, -time, -day, -name)
  
  
  
  test <- subset(df_continous, sample == FALSE)
  
  train <- subset(df_continous, sample == TRUE)
  
  
  # model
  
  model_mars <- earth(diff ~ ., train)
  
  
  # Predicting and getting rmse
  
  predicted_diff <- predict(model_mars, test)
  
  test_diff <- as.vector(test$diff)
  
  test_rmse <- rmse(predicted_diff, test$diff)
  
  
  predicted_train <- predict(model_mars, train)
  train_rmse <- rmse(predicted_train, train$diff)
  
  
  
  predicted_classes <- as.data.frame(predicted_diff)
  
  
  
  predicted_classes$test_diff <- test_diff
  
  predicted_classes$close <- as.vector(test$close)
  
  values_final_metrics <- classifying.30.min(predicted_classes) # using the function toget the metrics
  
  values_final_metrics[4] <- train_rmse # adding the train rmse as the last metric
  
  
  return(values_final_metrics)
  
} # end of predict.mars


predict.support.vector <- function(df){
  
  
  
  
  set.seed(123)
  df$sample <- sample.split(df$diff, SplitRatio = .75)
  
  df_continous <- df %>% select(-date, -time, -day, -name)
  
  
  
  test <- subset(df_continous, sample == FALSE)
  
  train <- subset(df_continous, sample == TRUE)
  
  
  # model for support vector machine
  
  set.seed(12345)
  model_SVM <- ksvm(diff~.,
                    data=train,
                    kernel= "vanilladot")
  
  #look at basic
  
  predicted_diff <-predict(model_SVM,  test)
  # Predicting and getting rmse
  
  
  
  test_diff <- as.vector(test$diff)
  
  test_rmse <- rmse(predicted_diff, test$diff)
  
  
  predicted_train <- predict(model_SVM, train)
  
  train_rmse <- rmse(predicted_train, train$diff)
  
  
  
  predicted_classes <- as.data.frame(predicted_diff)
  
  
  
  predicted_classes$test_diff <- test_diff
  
  predicted_classes$close <- as.vector(test$close)
  
  values_final_metrics <- classifying.30.min(predicted_classes) # using the function toget the metrics
  
  values_final_metrics[4] <- train_rmse # adding the train rmse as the last metric
  
  
  return(values_final_metrics)
  
  
  
} # end of rpedic support vector machines  

h2o.models <- function(name_of_stock){
  directory <- getwd()
  DATA_SET= paste(directory,"/",name_of_stock, ".csv", sep="")
  SPLIT_RATIO = .75
  
  
  #library
  library(h2o)
  
  
  #Initialize
  localH2O= h2o.init()
  
  
  #Import (upload) Data
  df <- h2o.importFile(DATA_SET)
  
  names_of_coluns_no_diff <- names(df)
  
  #SAMPLE DATA
  
  autoSplit <- h2o.splitFrame(data = df, ratios = c(.75))
  train <- autoSplit[[1]]
  testValidation <- autoSplit[[2]]
  
  testValidationSplit <- h2o.splitFrame(data = testValidation, ratios = c(.75))
  test <- testValidationSplit[[1]]
  validation <- testValidationSplit[[2]]
  
  
  
  # Deep Learning to predict diff
  deepLearningModel <- h2o.deeplearning(y = "diff",
                                        x = names_of_coluns_no_diff,
                                        training_frame = train, 
                                        validation_frame = validation,
                                        hidden = c(4,3),
                                        activation = "Tanh",
                                        epochs = 1000,
                                        seed = 1234)
  
  # Predict using the deeplearning model and the testing dataset
  pred = h2o.predict(object = deepLearningModel, newdata = test)
  
  pred$close <- test$close
  
  pred$test_diff <- test$diff
  
  pred$diff <- pred$predict
  
  pred_train = h2o.predict(object = deepLearningModel, newdata = train)
  
  pred_train$actual <- train$diff
  
  r_data_pred <- as.data.frame(pred)
  
  r_data_pred  <- r_data_pred[, 2:4]
  
  r_data_train <- as.data.frame(pred_train)
  
  
  results <- classifying.30.min(r_data_pred)
  
  results[4] <- rmse(r_data_train$actual,
                     r_data_train$predict)
  
  results[5] <- "deep_learning"
  
  
  
  # Random forest model
  
  diff_decision <- h2o.randomForest(x = names_of_coluns_no_diff,
                                    y = "diff",
                                    ntrees = 10,
                                    max_depth = 5,
                                    min_rows = 10,
                                    training_frame = train, 
                                    validation_frame = validation)
  
  
  
  
  
  
  # Generate predictions on a validation set (if necessary):
  predict <- h2o.predict(diff_decision, newdata = test)
  
  predict$close <- test$close
  
  predict$test_diff <- test$diff
  
  predict$diff <- predict$predict
  
  
  
  r_data_predict<- as.data.frame(predict)
  
  
  results2 <- classifying.30.min(r_data_predict)
  
  
  
  results2[5] <- "random_forest"
  
  
  # AutoML to predict diff
  
  
  autoMLModel <- h2o.automl(y = "diff",
                            x = names_of_coluns_no_diff,
                            training_frame = train, 
                            validation_frame = validation,
                            balance_classes = TRUE,
                            max_runtime_secs = 60,
                            seed = 1234)
  
  # Predict using the GLM model and the testing dataset
  
  pred2 = h2o.predict(object = autoMLModel, newdata = test)
  
  
  
  pred2$close <- test$close
  
  pred2$test_diff <- test$diff
  
  pred2$diff <- pred2$predict
  
  pred2_train = h2o.predict(object = deepLearningModel, newdata = train)
  
  pred2_train$actual <- train$diff
  
  r_data_pred2 <- as.data.frame(pred2)
  
  r_data_pred2  <- r_data_pred[, 2:4]
  
  r_data_train2 <- as.data.frame(pred2_train2)
  
  
  results3 <- classifying.30.min(r_data_pred2)
  
  results3[4] <- rmse(r_data_train2$actual,
                      r_data_train2$predict)
  
  automodelname <- print(h2o.get_leaderboard(object = autoMLModel, extra_columns = 'ALL')[1,1])
  
  results3[5] <- automodelname
  
  return(results,
         results2,
         results3)
  
}# end of h20.models




########### Global variables ###########



tiingo_token <- "" # write your tiingo token

stocks <- c("fb", "amzn", "aapl", "nflx", "googl") # Name of the stocks you would like to track

time_period <- 240 # The time period is given in days (at least 4 months back in time 120 days)

Sys.setenv(TZ='EST') # setting the timezone for the one of the market


# list to store the results of the alr=gorithm

time_series <- list()

mars_results <- list()

support_vector_machines <- list()


h2o_results <- list()

########## Begging of the Source code ###########


historical_faang<- stock.data(stocks = stocks, time_period = time_period, tiingo_token = tiingo_token) #getting the latest data



for(i in 1:length(historical_faang)){ # getiing the results for the follwoing models

time_series[[i]] <- forecast.timeseries(historical_faang[[i]])

mars_results[[i]] <- predict.mars(historical_faang[[i]])

support_vector_machines[[i]] <- predict.support.vector(historical_faang[[i]])

}


exporting.data.to.csv(stocks) # exportingt he data for the h2o

for(i in 1:length(historical_faang)) {
  
  
  h2o_results[[i]] <-  h2o.models(stocks[i])
  
}



for(i in 1:length(stocks)){
  
  h2o_deep <- h2o_results[[i]][1]
  
  h2o_random <- h2o_results[[i]][2]
  
  h2o_auto <-  h2o_results[[i]][3]
  
  datafinal<- as.data.frame(rbind(h2o_deep,h2o_random, h2o_auto, time_series[[i]], mars_results[[i]], support_vector_machines[[i]]))
  
  write.csv(datafinal,paste(stocks[i],"datafinal",".csv", sep =""))
  
  
}



######### End of the Source code ###############




####### Crystal Ball 2020 Copyright ######


###### We hope you enjoy our solution #######
