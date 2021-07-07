#PACKAGES#
install.packages("gganimate")
library(dplyr)  #data manipulation
library(ggplot2)  #data viz
library(rworldmap) #map (world) data viz
library(RColorBrewer) #map coulouring
library(plotly) #interactive plots
library(hrbrthemes) #plot themes
library(lubridate)  #data transform (dates)
library(dygraphs) #interactive time series graphs
library(xts)    # To make the convertion data-frame / xts format
library(gganimate)  #animated graphs




train <- read.csv(file = '/users/raphaeltanneur/Desktop/EDA_R/train.csv');
test <- read.csv(file = '/users/raphaeltanneur/Desktop/EDA_R/test.csv');

df <- as.data.frame(train);
test_df <- as.data.frame(test);
head(df);

str(data);
summary(data);

#checks
# class(df$Date)
# df$Date <- as.Date(df$Date, format = "%Y-%m-%d");
# 
# df$ModelRegion <- if_else(df$Province_State == "",
#                           paste0(df$Country_Region, "-", df$Country_Region),
#                           paste0(df$Country_Region, "-", df$Province_State));
# 
# test_df$Date <- as.Date(test_df$Date, format = "%Y-%m-%d");
# 
# test_df$ModelRegion <- if_else(test_df$Province_State == "",
#                                paste0(test_df$Country_Region, "-", test_df$Country_Region),
#                                paste0(test_df$Country_Region, "-", test_df$Province_State));
# 
# 
# print(paste(min(df$Date), max(df$Date), min(test_df$Date), max(test_df$Date)));



#### OR 


train$Date = as.Date(train$Date, format = "%Y-%m-%d")
test$Date = as.Date(test$Date, format = "%Y-%m-%d")
train$Province_State = as.character(train$Province_State)
test$Province_State = as.character(test$Province_State)
train$Country_Region = as.character(train$Country_Region)
test$Country_Region = as.character(test$Country_Region)



colSums(is.na(train))
colSums(is.na(test))
sum(is.na(train))

head(train)
head(test)

#Calculation total number of cases per country
all_countires = train %>%
  group_by(Country_Region) %>%
  summarise(Confirmed = sum(ConfirmedCases),
            Deaths = sum(Fatalities))

all_countires;

total_by_date = train %>%
  group_by(Date) %>%
  summarise(Confirmed = sum(ConfirmedCases),
            Deaths = sum(Fatalities))

total_by_date;

ggplot(total_by_date, aes(Date)) +
  geom_line(aes(y = Confirmed, colour = "Confirmed Cases")) +
  geom_line(aes(y = Deaths, colour = "Fatalities")) +
  #labs(x = 'Date since 22-01-2020', y = 'Count Per Million') +
  ggtitle("Covid-19 Confirmed vs Fatalities Cases")


MostConfirmed = all_countires %>% arrange(-Confirmed) %>% top_n(10)
MostDeaths = all_countires %>% arrange(-Deaths) %>% top_n(10)

conf_cases = ggplot(data = MostConfirmed, aes(
  x = reorder(Country_Region,-Confirmed),
  y = Confirmed,
  fill = Country_Region
)) +
  geom_bar(stat = "identity") + xlab("Country") +
  ggtitle("Top 10 Countries Confirmed Cases")

conf_cases;

deaths = ggplot(data = MostConfirmed, aes(
  x = reorder(Country_Region,-Deaths),
  y = Deaths,
  fill = Country_Region
)) +
  geom_bar(stat = "identity") + xlab("Country") +
  ggtitle("Top 10 Countries Death Cases")


deaths
