# Time-Series-Analysis
Forecasting models built on bike ridership dataset. 
Data used uploded inside the data folder.

## File Guide
*Forecasting bike rentals- EDA.ipynb* : exploratory data analysis and visualizations, helping better understand the data and modelling decisions. 

*Forecasting bike ridership.R* : .R script to generate a forecast using an ARIMA model of order c(5,1,7) implemented and cross-validated using tsCV(). 

### Results 
<img width="1001" alt="Screen Shot 2020-01-13 at 9 17 46 PM" src="https://user-images.githubusercontent.com/17786269/72316248-65b40d80-364a-11ea-95f7-4875450411bc.png">

#### ARIMA:
MAPE: 12.07%
RMSE: 63.02

#### Prophet:
MAPE: 10.28%
RMSE: 51.06
