########## Using solution from part 2 ##########

# Two useful libraries:
library(xts)
library(rmgarch)
# library(fGarch) # a 3rd library I used to check the GARCH estimates

# read the .csv files containing the SPX and VIX data
SPX_history <- read.csv("/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part2/SPX_History.csv")
VIX_history <- read.csv("/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part2/VIX_History_1.csv")

# do a bit of work to clean up the data
SPX <- subset(SPX_history, select = c(Date,Close))
VIX <- subset(VIX_history, select = c(Date,Close))
colnames(SPX)[colnames(SPX) == "Date"] <- "LDate"
colnames(VIX)[colnames(VIX) == "Date"] <- "LDate"
SPX$LDate <- as.Date(SPX$LDate,format="%m/%d/%Y")
SPX <- subset(SPX, select = c(LDate,Close))
VIX$LDate <- as.Date(VIX$LDate,format="%m/%d/%Y")
VIX <- subset(VIX, select = c(LDate,Close))
colnames(SPX)[colnames(SPX) == "Close"] <- "SPX"
colnames(VIX)[colnames(VIX) == "Close"] <- "VIX"

# merge the dataframes
SPXVIX <- merge(SPX, VIX, by="LDate")
SPXVIX$SPXret[2:nrow(SPXVIX)] <- log(SPXVIX$SPX[2:nrow(SPXVIX)]/SPXVIX$SPX[1:(nrow(SPXVIX)-1)])
SPXVIX$VIXret[2:nrow(SPXVIX)] <- log(SPXVIX$VIX[2:nrow(SPXVIX)]/SPXVIX$VIX[1:(nrow(SPXVIX)-1)])

#select the data to use in estimation (the 1,000 returns up through 10/30/2020)
valuedate <- which(SPXVIX$LDate == as.Date("10/30/2020",format="%m/%d/%Y"))
returns <- SPXVIX[(valuedate-999):valuedate,c(1,4,5)]

## he function dccfit() will be used; it estimates the DCC model in two stages
## First a univariate GARCH model is fitted to each return series
## The standardized residuals are then extracted
## Then a model with dynamically changing conditional correlation matrix is fitted

## As an initial step, estimate a univariate GARCH model for each return process
#  SPX: Specify univariate GARCH(1,1) model and set mean return = 0
uspec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                    distribution.model = "norm")
# VIXX: Specify univariate GARCH(1,1) model and use AR(1) = ARMA(1,0) for the mean
uspec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
                     distribution.model = "norm") 

## Check the univariate specification for the two series
fit.marg1 <- ugarchfit(spec = uspec1, data = returns[,2])
fit.marg2 <- ugarchfit(spec = uspec2, data = returns[,3])
coef(fit.marg1)
coef(fit.marg2)
sigma_marg1 <- sigma(fit.marg1)

## Now turn to estimating the DCC model
## Combine univariate specifications of the 2 GARCH models
marginspec <- multispec(c(uspec1, uspec2))

## Create DCC(1,1) specification
mspec <- dccspec(marginspec, dccOrder = c(1,1), model = "DCC", distribution = "mvnorm")

## Fit the DCC(1,1) model
mod <- dccfit(mspec,returns[,2:3])
mod
coef(mod)

##Let's use a different GARCH function to check the estimates for the GARCH models
# umodel1 <- garchFit( ~ garch(1,1), data = returns[,2], include.mean = FALSE)
# umodel2 <- garchFit( ~ arma(1,0)+garch(1,1), data = returns[,3], include.mean = TRUE)

## Let's extract some useful things from the results:
SPXcoef <- fit.marg1@fit$coef    #GARCH model parameter estimates
SPXsigma <- fit.marg1@fit$sigma  #standard deviations for each date
SPXz <- fit.marg1@fit$z          #GARCH shocks z for each date
VIXcoef <- fit.marg2@fit$coef    #GARCH model parameter estimates
VIXsigma <- fit.marg2@fit$sigma  #standard deviations for each date
VIXz <- fit.marg2@fit$z          #GARCH shocks z for each date
DCCcoef <- mod@mfit$coef        #DCC parameter estimates (includes the GARCH estimates)
DCCR <- mod@mfit$R              #DCC correlations for each date
DCCQ <- mod@mfit$Q              #DCC q's for each date


########## FHSVaR ##########
# Sigma of t+1 using GARCH model
SPXvar <- SPXcoef[1] + SPXcoef[2] * returns$SPXret[1000]^2 + SPXcoef[3] * SPXsigma[1000]^2
VIXvar <- VIXcoef[3] + VIXcoef[4] * returns$VIXret[1000]^2 + VIXcoef[5] * VIXsigma[1000]^2
VIX_ar <- VIXcoef[1] + VIXcoef[2] * returns$VIXret[1000]
SPXsigma1 <- sqrt(SPXvar)
VIXsigma1 <- sqrt(VIXvar)

# Re-scaled return
SPX_r <- SPXsigma1 * SPXz
VIX_r <- VIX_ar + VIXsigma1 * VIXz

# Option data
library(readxl)

path <- "/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part1/F567.s2022.project.part1.solution.xlsx"
current_data <- read_excel(path, sheet = "Option prices", col_names = FALSE, range = "B3:B6")
current_index <- current_data[[2,1]]
dividend <- current_data[[3,1]]
options_data <- read_excel(path, sheet = "Option prices", range = "A9:M29")

library("fOptions")
n <- nrow(options_data)
days <- options_data$`Time to expiry (days)`-3
t_maturity <- days / 365

# Calculate portfolio profit and loss
portfolio_value <- sum(options_data$`Market Value`)
portfolioPL <- rep(0, 1000)

for (i in 1:1000){
  new_option_market_value <- numeric(n)
  spx_r <- SPX_r[i]
  vix_r <- VIX_r[i]
  for(j in 1:n){
    x <- options_data$`Strike Price`[j]
    r <- options_data$`Interest rate`[j]
    b <- r - dividend
    t <- t_maturity[j]
    vol <- options_data$`Implied volatility`[j] * exp(vix_r)
    s <- current_index * exp(spx_r)
    multiplier <- options_data$Multiplier[j]
    amount <- options_data$Quantity[j]
    new_option_value <- GBSOption(TypeFlag = "c", s, x, t, r, b, vol)@price
    new_option_market_value[j] <- multiplier * amount * new_option_value
  }
  portfolioPL[i] <- sum(new_option_market_value) - portfolio_value
}

# Compute the FHSVaR as the negative of the 5% quantile
FHSVaR <- -quantile(portfolioPL, probs = 0.05)
FHSVaR

