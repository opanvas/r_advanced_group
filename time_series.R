file_path_yiqi <- "C:/Users/98669/Desktop/Master/TERM3/Advanced R/github/r_advanced_group/"
data <- read.csv(file.path(file_path_yiqi, "train.csv"), header=TRUE,sep=",");
test <- read.csv(file.path(file_path_yiqi, "test.csv"), header=TRUE,sep=",");

library(data.table);
library(dplyr);
library(foreach);
library(doParallel);
library(pROC);
library(cluster);
library(astsa);
library(zoo);
library(tseries);
library(forecast);
library(ggplot2);
library(ggfortify);

# ConfirmedCases data
confirmed <- zoo(data$ConfirmedCases,seq(from=as.Date("2020-01-22"), to=as.Date("2020-05-15"), by=1));
autoplot(confirmed)+xlab("Date")+ylab("Confirmed Cases");
str(confirmed);

# Check diff steps
diff_con <- diff(confirmed);
plot(diff_con, col = "red") ;

diff_2_con <- diff(diff_con);
plot(diff_2_con, col = "red") ;

d <- 2;
dat <- diff_2_con;

### Select p (PACFplot)
ggPacf(dat);

p <- 6;

### Select q (ACFplot)
ggAcf(dat);

q <- 0;

### "manual" model
Arima(confirmed, order = c(p, d, q));

# Automatic arima
auto.arima(confirmed)

### Widen search
auto.arima(confirmed, seasonal = FALSE, 
           stepwise=FALSE, approximation=FALSE);

# Prediction
# Final model
fit <- Arima(confirmed, order = c(p, d, q));

# Make predictions
pred <- forecast(fit, h = 5);
pred;

# Plot predictions
autoplot(pred)

# Fatalities data
fatalities <- zoo(data$Fatalities,seq(from=as.Date("2020-01-22"), to=as.Date("2020-05-15"), by=1));
autoplot(fatalities)+xlab("Date")+ylab("Fatalities");
str(fatalities);

# Check diff steps
diff_fat <- diff(fatalities);
plot(diff_con, col = "blue");

diff_2_fat <- diff(diff_fat);
plot(diff_2_fat, col = "blue");

d <- 2;
dat <- diff_2_fat;

### Select p (PACFplot)
ggPacf(dat);

p <- 14;

### Select q (ACFplot)
ggAcf(dat);

q <- 0;

### "manual" model
Arima(fatalities, order = c(p, d, q));

# Automatic arima
auto.arima(fatalities)

### Widen search
auto.arima(fatalities, seasonal = FALSE, 
           stepwise=FALSE, approximation=FALSE);

# Prediction
# Final model
fit <- auto.arima(fatalities, seasonal = FALSE, 
                  stepwise=FALSE, approximation=FALSE);

# Make predictions
pred <- forecast(fit, h = 5);
pred;

# Plot predictions
autoplot(pred)

