file_path_yiqi <- "C:/Users/98669/Desktop/Master/TERM3/Advanced R/github/r_advanced_group/"
train_data <- read.csv(file.path(file_path_yiqi, "train.csv"), header=TRUE,sep=",");
test_data <- read.csv(file.path(file_path_yiqi, "test.csv"), header=TRUE,sep=",");
submission <- read.csv(file.path(file_path_yiqi, "submission.csv"), header=TRUE,sep=",");

############################# 0. Import libraries ###############################################################
library(data.table);
library(dplyr);
library(foreach);
library(zoo);
library(tseries);
library(forecast);
library(ggplot2);
library(ggfortify);

############################# 1. Convert columns into right format ##############################################
class(train_data$Province_State);
class(train_data$Country_Region);
class(train_data$Date);
train_data$Date = as.Date(train_data$Date, format = "%Y-%m-%d");
test_data$Date = as.Date(test_data$Date, format = "%Y-%m-%d");
class(train_data$Date);

############################# 2. Check the COVID data by dates ##################################################
#### Confirmed Cases by date
confirmed <- zoo(train_data$ConfirmedCases,seq(from=as.Date("2020-01-22"), to=as.Date("2020-05-15"), by=1));
str(confirmed);

#### Fatalities data by date
fatalities <- zoo(train_data$Fatalities,seq(from=as.Date("2020-01-22"), to=as.Date("2020-05-15"), by=1));
str(fatalities);

#### Compare both confirmed and fatalities cases
Cases <- merge(confirmed, fatalities)
ggplot(Cases, aes(x=Index)) + 
  geom_line(aes(y=confirmed, color="Confirmed Cases")) +
  geom_line(aes(y=fatalities, color="Fatalities Cases"))  +
  xlab('Dates') +
  ylab('Cases') + 
  ggtitle("Confirmed vs Fatalities Cases From Jan to May in 2020");

############################# 3. Try time series model for Confirmed and Fatalities cases worldwide  #############

############################# 3.1. Confirmed #############################
#### Check diff steps
diff_con <- diff(confirmed);
plot(diff_con, col = "red");

diff_2_con <- diff(diff_con);
plot(diff_2_con, col = "red");

d <- 2;
dat <- diff_2_con;

#### Select p (PACFplot)
ggPacf(dat);
p <- 6;

#### Select q (ACFplot)
ggAcf(dat);
q <- 0;

#### "manual" model
Arima(confirmed, order = c(p, d, q));
# sigma^2 estimated as 747.7:  log likelihood=-532.9
# AIC=1079.8   AICc=1080.86   BIC=1098.89


#### Automatic arima
auto.arima(confirmed)
# sigma^2 estimated as 900.2:  log likelihood=-544.86
# AIC=1097.72   AICc=1098.09   BIC=1108.63


#### Widen search
auto.arima(confirmed, seasonal = FALSE,
           stepwise=FALSE, approximation=FALSE);
# sigma^2 estimated as 837.2:  log likelihood=-540.65
# AIC=1093.3   AICc=1094.1   BIC=1109.67

#### "manual" model performs better for Confirmed Cases (for worldwide)

############################# 3.2. Fatalities #############################
#### Check diff steps
diff_fat <- diff(fatalities);
plot(diff_con, col = "blue");

diff_2_fat <- diff(diff_fat);
plot(diff_2_fat, col = "blue");

d <- 2;
dat <- diff_2_fat;

#### Select p (PACFplot)
ggPacf(dat);
p <- 14;

#### Select q (ACFplot)
ggAcf(dat);
q <- 0;

#### "manual" model
Arima(fatalities, order = c(p, d, q), method="ML");
# sigma^2 estimated as 2.639:  log likelihood=-211.31
# AIC=452.62   AICc=457.57   BIC=493.53


#### Automatic arima
auto.arima(fatalities);
# sigma^2 estimated as 3.367:  log likelihood=-227.17
# AIC=466.33   AICc=467.13   BIC=482.7

#### Widen search
auto.arima(fatalities, seasonal = FALSE,
           stepwise=FALSE, approximation=FALSE);
# sigma^2 estimated as 3.258:  log likelihood=-225.69
# AIC=463.39   AICc=464.18   BIC=479.75

#### Again, "manual" model performs better for for Fatalities Cases (for worldwide)

############################# 4. Forecast the time series for each country & build submission file  ##############
#### Form country list using in for loop
country_list <- unique(train_data$Country_Region);
length(country_list);

#### Check the period that we need to predict in
test_time <- difftime(max(test_data$Date),
                      min(test_data$Date), units = "days");
test_time;

#### Form the for loop
for (c in 1:length(country_list)) {
  train_model <- train_data %>% 
    filter(train_data["Country_Region"] == country_list[c]) %>%
    arrange(Date);
  
  test_model <- test_data %>% 
    filter(test_data["Country_Region"] == country_list[c]) %>%
    arrange(Date);
  
  # Build time series model for confirmed cases & predict during the test time period
  ts_confirmed <- zoo(train_model$ConfirmedCases,
                        seq(from=as.Date("2020-01-22"), 
                            to=as.Date("2020-05-15"), by=1));

  model_confirmed <- auto.arima(ts_confirmed);
  predict_confirmed <- forecast(model_confirmed, h = test_time);
  
  # Build time series model for fatalities cases & predict during the test time period
  ts_fatalities <- zoo(train_model$Fatalities,
                      seq(from=as.Date("2020-01-22"), 
                          to=as.Date("2020-05-15"), by=1));
  
  model_fatalities <- auto.arima(ts_fatalities);
  predict_fatalities <- forecast(model_fatalities, h = test_time);
  
  # Round results to integer
  for (t in 1:test_time){
    if (predict_confirmed[["upper"]][t] > 0) {
      test_model$ConfirmedCases[t] <- as.numeric(round(predict_confirmed[["upper"]][t]))
    } else {
      test_model$ConfirmedCases[t] <- 0
    }
    if (predict_fatalities[["upper"]][t] > 0) {
      test_model$Fatalities[t] <- as.numeric(round(predict_fatalities[["upper"]][t]))
    } else {
      test_model$Fatalities[t] <- 0
    }
  }

  # Rewrite the submission file
    if (submission$ForecastId %in% test_model$ForecastId) {
      submission$ConfirmedCases[submission$ForecastId] <- test_model$ConfirmedCases;
      submission$Fatalities[submission$ForecastId] <- test_model$Fatalities
    }

  # Each iteration need to clear the saved models
  rm(train_model)
  rm(test_model)
  rm(predict_confirmed)
  rm(predict_fatalities)
};

############################# 5. Save the submission file to local  ##########################################
# write.csv(submission,"test_submission.csv", row.names = FALSE);
