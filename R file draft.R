#Perform Exploratory Data Analysis over the given data. This could include operations like comparing the trends of different countries, checking if there exists weekday seasonality, etc. Score: 2 points.
#Create a Shiny app that contains your EDA for step 1. You can put your shiny app into an Rmarkdown for higher score. Score: 3 points.
#Create a time series model to try to predict both cases and fatalities for test range of dates defined in the constest. Score: 2 points.
#Create a Machine Learning to try to predict both cases and fatalities for test range of dates defined in the constest. Score: 2 points.
#Give a final submission file to upload to Kaggle (it must have the appropiate format describen in the contest). It can be the output of steps 3 or 4, the one that yields the best predictions. Score will be given based on which of the group predictions gets a better evaluation score on the Kaggle platform when submitting the file:
#  1st position: 1 point.
#2nd position: 0.75 points.
#3rd position: 0.6 points.
#4th position: 0.5 points.
#5th position: 0.4 points.
#6th position: 0.3 points.
#7th position: 0.2 points.
#8th position: 0.1 points.

library(data.table);
library(foreach);
library(doParallel);
library(pROC);
library(cluster);
library(astsa);
library(tseries)
library(forecast)
