# FIN567: Portfolio Market Risks Project
Hello there!\
This repository holds my final project done during the period of FIN567 Financial Risk Management course.

## Project Source Codes:
**Mission: On October 30, 2020, measure the risks of a portfolio of options on the S&P 50 index, and then carry out the analysis of an options portfolio on a specified date.**
* [Greek Letter Risks](https://github.com/yschang306/FIN567_Portfolio_Market_Risks_Projects/blob/main/FIN567_Final_Project/Code/FIN567_Project_Part1.R)
  * Compute the option implied volatilities and Greek letter risks of the several options and the portfolio. 
  * Show the value of the options portfolio for S&P 500 index values ranging from 10% below to 10% above the current index value.
* [Delta-normal VaR](https://github.com/yschang306/FIN567_Portfolio_Market_Risks_Projects/blob/main/FIN567_Final_Project/Code/FIN567_Project_Part2.R)
  * Estimate GARCH(1,1) models for the log changes in the S&P 500 index and the VIX using the most recent 1,000 observations. 
  * Estimare DCC(1,1) models of the correlation between log changes in the S&P 500 index and the VIX with AR(1) model for the conditional mean.
  * Compute delta-normal VaR using a probability of 5% and a holding period of one day.
* [Filtered Historical Simulation VaR (one-day period)](https://github.com/yschang306/FIN567_Portfolio_Market_Risks_Projects/blob/main/FIN567_Final_Project/Code/FIN567_Project_Part3.R)
  * Compute filtered historical simulation VaR using a probability of 5% and a holding period of one day.
* [Filtered Historical Simulation VaR (21-day period)](https://github.com/yschang306/FIN567_Portfolio_Market_Risks_Projects/blob/main/FIN567_Final_Project/Code/FIN567_Project_Part4.R)
  * Compute filtered historical simulation VaR using a probability of 5% and a holding period of one month (21 trading days).
