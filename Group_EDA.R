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
all_countries = train %>%
  group_by(Country_Region) %>%
  summarise(Confirmed = sum(ConfirmedCases),
            Deaths = sum(Fatalities))

all_countries;

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

deaths = ggplot(data = MostDeaths, aes(
  x = reorder(Country_Region,-Deaths),
  y = Deaths,
  fill = Country_Region
)) +
  geom_bar(stat = "identity") + xlab("Country") +
  ggtitle("Top 10 Countries Death Cases")


deaths



#Adding cumulative columns for Confirmed Cases and Fatalities
train_cum <- train %>% group_by(Country_Region) %>% mutate(cumconfirmed=cumsum(ConfirmedCases), cumfatalities=cumsum(Fatalities), days = Date - first(Date) + 1)
head(train_cum)
tail(train_cum)



#Extracting certain countries
italy <- train_cum %>% filter(Country_Region=="Italy")
head(italy)
spain <- train_cum %>% filter(Country_Region=="Spain")
head(spain)
usa <- train_cum %>% filter(Country_Region=="US")
head(usa)



#Summary statistics
summary(train_cum)
by(train_cum$ConfirmedCases, train_cum$Country_Region, summary)
by(train_cum$cumconfirmed, mtrain_cum$Country_Region, summary)
by(train_cum$Fatalities, train_cum$Country_Region, summary)
by(train_cum$cumfatalities, train_cum$Country_Region, summary)
summary(italy)
summary(spain)
summary(usa)



#Graph visualizations of the 3 selected countries
#Italy ConfirmedCases and Fatalities
italy_plot <- ggplot(italy) + geom_bar(aes(x=Date, y=ConfirmedCases), stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 confirmed Cases in Italy", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(aes(x=Date, y=Fatalities), color = 'red')

italy_plot


#Spain ConfirmedCases and Fatalities
spain_plot <- ggplot(spain) + geom_bar(aes(x=Date, y=ConfirmedCases), stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 confirmed Cases in Spain", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(aes(x=Date, y=Fatalities), color = 'red')

spain_plot


#US ConfirmedCases and Fatalities
usa_plot <- ggplot(usa) + geom_bar(aes(x=Date, y=ConfirmedCases), stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 confirmed Cases in the US", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(aes(x=Date, y=Fatalities), color = 'red')

usa_plot



#Confirmed cases by country for our 3 countries
countryselection <- train_cum %>% filter(Country_Region==c("US", "Italy", "Spain"))

comparative_plot <- ggplot(countryselection, aes(x=days, y=ConfirmedCases, colour=Country_Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

comparative_plot



# Matrix of line graphs of confirmed cases and fatalities for the 3 countries
str(countryselection)

countryselection %>% gather("Type", "Cases", -c(Date, days, Country_Region)) %>%
  ggplot(aes(x=days, y=Cases, colour=Country_Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cases by Country", x= "Days", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(rows=vars(Type))
