# Team Members: Yu-Shiuan Chang (yschang4), Wei Ting Chao (wtchao3),
#               Wei Wang (weiw14), Ming-Ying Lu (mylu3)

########## Data Preprocessing ##########
library(readxl)

path <- "/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part1/F567.2022-1.project.SPXoptions.xlsx"
current_data <- read_excel(path, sheet = "Option prices", col_names = FALSE, range = "B1:B3")
current_date <- as.Date(current_data[[1,1]], origin = "1899-12-30")
current_index <- current_data[[2,1]]
dividend <- current_data[[3,1]]
options_data <- read_excel(path, sheet = "Option prices", range = "A5:I25")
zcb_interest_rate <- read_excel(path, sheet = "zero-coupon yield curve")

########## Calculating Implied Volatilities ##########
library("fOptions")
n <- nrow(options_data)
s <- current_index
# Calculate time to maturity
options_data$`Expiration Date` <- as.Date(options_data$`Expiration Date`)
days <- as.numeric(options_data$`Expiration Date` - current_date)
adjusted_days <- numeric(n)
adjusted_days[1:9] <- days[1:9] - 6.5/24
adjusted_days[10:14] <- days[10:14] 
adjusted_days[15:20] <- days[15:20] - 6.5/24
time <- adjusted_days/365
rate <- approx(zcb_interest_rate$days, zcb_interest_rate$rate, adjusted_days)$y / 100

implied_vol <- rep(0, n)

for(i in 1:n){
  price <- options_data$Price[i]
  x <- options_data$`Strike Price`[i]
  r <- rate[i]
  b <- r - dividend
  t <- time[i]
  implied_vol[i] <- GBSVolatility(price, TypeFlag = "c", s, x, t, r, b)
}

options_data$`Implied volatility` <- implied_vol

########## Calculating Greek Letter Risks ##########
delta <- rep(0, n)
gamma <- rep(0, n)
theta <- rep(0, n)
vega <- rep(0, n)

for(i in 1:n){
  x <- options_data$`Strike Price`[i]
  r <- rate[i]
  b <- r - dividend
  t <- time[i]
  vol <- implied_vol[i]
  delta[i] <- GBSGreeks(Selection = "delta", TypeFlag = "c", s, x ,t, r, b, vol)
  gamma[i] <- GBSGreeks(Selection = "gamma", TypeFlag = "c", s, x ,t, r, b, vol)
  theta[i] <- GBSGreeks(Selection = "theta", TypeFlag = "c", s, x ,t, r, b, vol)
  vega[i] <- GBSGreeks(Selection = "vega", TypeFlag = "c", s, x ,t, r, b, vol)
}

options_data$Delta <- delta * options_data$Multiplier
options_data$Gamma <- gamma * options_data$Multiplier
options_data$Theta <- theta * options_data$Multiplier
options_data$Vega <- vega * options_data$Multiplier

# Export data
library("writexl")
write_xlsx(options_data,"/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/FIN567_Project_Part1_1.xlsx")

########## Calculating Portfolio Greek Letter Risks ##########
port_delta <- sum(options_data$Delta * options_data$Quantity)
port_delta
port_gamma <- sum(options_data$Gamma * options_data$Quantity)
port_gamma
port_theta <- sum(options_data$Theta * options_data$Quantity)
port_theta 
port_vega <- sum(options_data$Vega * options_data$Quantity)
port_vega

########## Calculating Value of the Options Portfolio ##########
index_values <- seq(0.9 * current_index, 1.1 * current_index, by = 10)
c <- length(index_values)

# Calculate option values
v <- data.frame(matrix(ncol = c, nrow = n))

for(i in 1:n){
  x <- options_data$`Strike Price`[i]
  r <- rate[i]
  b <- r - dividend
  t <- time[i]
  vol <- implied_vol[i]
  for(j in 1:c){
    s <- index_values[j]
    v[i, j] <- GBSOption(TypeFlag = "c", s, x, t, r, b, vol)@price
  }
}

# Calculate portfolio values
port_values <- rep(0, c)
for(i in 1:c){
  port_values[i] <- sum(options_data$Quantity * options_data$Multiplier * v[, i]) 
}

port_values_data <- data.frame(index_values, port_values)

# Export data
write_xlsx(port_values_data,"/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Portfolio_values.xlsx")


# Plot the change of portfolio values
options(scipen=999)
plot(port_values_data$index_values, port_values_data$port_values, type = "l", col = "red",
     main = "Changes of Portfolio Values based on S&P500 Index Values",
     xlab = "S&P500 Index Values",
     ylab = "Portfolio Values")
  
