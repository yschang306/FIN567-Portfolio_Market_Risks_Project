########## Using solution from part 3 ##########

# Several useful libraries:
library(readxl)
library(xts)
library(rmgarch)
library(fOptions) #needed to compute option values

##Part 2: Estimate the GARCH, NGARCH, and DCC models

# read the .csv files containing the SPX and VIX data
SPX_history <- read.csv("/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part2/SPX_History.csv")
VIX_history <- read.csv("/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part2/VIX_History_1.csv")

# do a bit of work to clean up the data
SPX <- subset(SPX_history, select = c(Date,Close))
VIX <- subset(VIX_history, select = c(Date,Close))
colnames(SPX)[colnames(SPX) == "Date"] <- "LDate"
colnames(VIX)[colnames(VIX) == "Date"] <- "LDate"
SPX$Date <- as.Date(SPX$LDate,format="%m/%d/%Y")
SPX <- subset(SPX, select = c(Date,Close))
VIX$Date <- as.Date(VIX$LDate,format="%m/%d/%Y")
VIX <- subset(VIX, select = c(Date,Close))
colnames(SPX)[colnames(SPX) == "Close"] <- "SPX"
colnames(VIX)[colnames(VIX) == "Close"] <- "VIX"

# merge the dataframes
SPXVIX <- merge(SPX, VIX, by="Date")
SPXVIX$SPXret[2:nrow(SPXVIX)] <- log(SPXVIX$SPX[2:nrow(SPXVIX)]/SPXVIX$SPX[1:(nrow(SPXVIX)-1)])
SPXVIX$VIXret[2:nrow(SPXVIX)] <- log(SPXVIX$VIX[2:nrow(SPXVIX)]/SPXVIX$VIX[1:(nrow(SPXVIX)-1)])

#select the data to use in estimation (the 1,000 returns up through 10/30/2020)
valuedate = which(SPXVIX$Date == as.Date("10/30/2020",format="%m/%d/%Y"))
returns <- SPXVIX[(valuedate-999):valuedate,c(1,4,5)]

## The function dccfit() will be used; it estimates the DCC model in two stages
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

## Let's extract some useful things from the GARCH and DCC results:
SPXcoef <- fit.marg1@fit$coef    #GARCH model parameter estimates for SPX
SPXsigma <- fit.marg1@fit$sigma  #standard deviations for each date
SPXz <- fit.marg1@fit$z          #GARCH shocks z for each date
VIXcoef <- fit.marg2@fit$coef    #GARCH model parameter estimates for VIX
VIXsigma <- fit.marg2@fit$sigma  #standard deviations for each date
VIXz <- fit.marg2@fit$z          #GARCH shocks z for each date
DCCcoef <- mod@mfit$coef        #DCC parameter estimates (includes the GARCH estimates)
DCCR <- mod@mfit$R              #DCC correlations for each date
DCCQ <- mod@mfit$Q              #DCC q's for each date

########## Part 4 FHSVaR for holding period of 21 trading days ##########
## Assuming that the correlation is constant as in 3_SimTSRisk.pptx slide 11
set.seed(1)

FH <- 10000 # number of simulation times
days <- 21  # one-month holding period (21 trading days)

SPX_simulate_r <- rep(0, FH)
VIX_simulate_r <- rep(0, FH)

for (i in 1:FH){
  spx_r <- rep(0, days)
  spx_var <- rep(0, days)
  vix_r <- rep(0, days)
  vix_var <- rep(0, days)
  vix_mean <- rep(0, days)
  for (j in 1: days){
    # Randomly draw shocks
    N <- sample(1:1000, 1)
    spxz <- SPXz[N]
    vixz <- VIXz[N]
    
    # Update variance using GARCH model
    if (j == 1){
      # Conditional variances for the next date from 10/30/2020
      spx_var[j] <- SPXcoef[1]+SPXcoef[2]*returns[1000,2]^2+SPXcoef[3]*SPXsigma[1000]^2
      vix_var[j] <- VIXcoef[3]+VIXcoef[4]*returns[1000,3]^2+VIXcoef[5]*VIXsigma[1000]^2
      vix_mean[j] <- VIXcoef[1]+VIXcoef[2]*returns[1000,3]
    } else{
      spx_var[j] <- SPXcoef[1]+SPXcoef[2]*spx_r[j-1]^2+SPXcoef[3]*spx_var[j-1]
      vix_var[j] <- VIXcoef[3]+VIXcoef[4]*vix_r[j-1]^2+VIXcoef[5]*vix_var[j-1]
      vix_mean[j] <- VIXcoef[1]+VIXcoef[2]*vix_r[j-1]
    }

    # Calculate log changes
    spx_r[j] <- sqrt(spx_var[j]) * spxz           
    vix_r[j] <- vix_mean[j] + sqrt(vix_var[j]) * vixz  
  }
  # Calculate log changes for 21 days
  SPX_simulate_r[i] <- sum(spx_r)
  VIX_simulate_r[i] <- sum(vix_r)
}

# Option data
path <- "/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part1/F567.s2022.project.part1.solution.xlsx"
optiondata <- read_excel(path, sheet = "Option prices", range = "A9:M29")
Strike     <- as.numeric(unlist(optiondata[,3]))
tau        <- as.numeric(unlist(optiondata[,11]))
rate       <- as.numeric(unlist(optiondata[,12]))
volatility <- as.numeric(unlist(optiondata[,13]))
Quantity   <- as.numeric(unlist(optiondata[,6]))
Price      <- as.numeric(unlist(optiondata[,7]))
value      <- as.numeric(unlist(optiondata[,9]))

# Compute the option values on the current date t (10/30/2020)
S <- rep(1,20)*SPXVIX[valuedate,2]
q <- rep(1,20)*0.0163917
b <- rate - q
optionPrice <- GBSOption("c", S=S, X=Strike, Time=tau, r=rate, b=b, sigma=volatility, title = NULL, 
                         description = NULL)@price
PortValue <- 100 * Quantity %*% optionPrice

# Compute portfolio profit and loss for 12/1/2020
new_tau <- tau - 32/365
new_PortValue <- rep(0,FH)
PortfolioPL <- rep(0, FH)
for(i in 1:FH){
  new_S <- SPXVIX[valuedate,2] * exp(SPX_simulate_r[i]) #simulated SPX value
  new_Volatility <- volatility *exp(VIX_simulate_r[i])
  new_Price <- GBSOption("c", S=new_S, X=Strike, Time=new_tau, r=rate, b=b, sigma=new_Volatility, title = NULL, 
                        description = NULL)@price
  new_PortValue[i] <- 100 * Quantity %*% new_Price
  PortfolioPL[i] <- new_PortValue[i]-PortValue
}

# Compute 5% FHSVaR
FHSVaR_21 <- -quantile(PortfolioPL, 0.05)
FHSVaR_21

