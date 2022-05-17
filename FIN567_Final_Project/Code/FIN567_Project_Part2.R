# Team Members: Yu-Shiuan Chang (yschang4), Wei Ting Chao (wtchao3),
#               Wei Wang (weiw14), Ming-Ying Lu (mylu3)

########## Data Preprocessing ##########
options(scipen=999)
date <- "2020-10-30"
SP500_raw <- read.csv("/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part2/SPX_History.csv")
SP500_raw$Date <- as.Date(SP500_raw$Date, format = "%m/%d/%Y")
SP500_raw <- subset(SP500_raw, select = c(Date,Close))


VIX_raw <- read.csv("/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part2/VIX_History_1.csv")
VIX_raw$Close <- VIX_raw$Close/100
VIX_raw$Date <- as.Date(VIX_raw$Date, format = "%m/%d/%Y")
VIX_raw <- subset(VIX_raw, select = c(Date,Close))

# merge the dataframes
SPXVIX <- merge(SP500_raw, VIX_raw, by="Date")
SPXVIX$SPXret[2:nrow(SPXVIX)] <- log(SPXVIX$Close.x[2:nrow(SPXVIX)]/SPXVIX$Close.x[1:(nrow(SPXVIX)-1)])
SPXVIX$VIXret[2:nrow(SPXVIX)] <- log(SPXVIX$Close.y[2:nrow(SPXVIX)]/SPXVIX$Close.y[1:(nrow(SPXVIX)-1)])

row_index <- which(SPXVIX$Date == date)
SP500 <- data.frame(return = SPXVIX$SPXret[(row_index-999):row_index])
rownames(SP500) <- SPXVIX$Date[(row_index-999):row_index]

VIX <- data.frame(log_change = SPXVIX$VIXret[(row_index-999):row_index])
rownames(VIX) <- SPXVIX$Date[(row_index-999):row_index]

###############  GARCH(1,1) & DCC(1,1)  ###############
library(rmgarch)

# GARCH
# garch_model_SP500 <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "GARCH"),
#                          mean.model = list(armaOrder = c(0,0), include.mean = FALSE))
garch_model_SP500 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "norm")
garch_SP500 <- ugarchfit(spec = garch_model_SP500, data = SP500)
sigma_SP500 <- sigma(garch_SP500)
coef(garch_SP500)

# garch_model_VIX <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1,1), submodel = "GARCH"), 
#                              mean.model = list(armaOrder = c(1,0), include.mean = FALSE))
garch_model_VIX <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                              mean.model = list(armaOrder = c(1,0), include.mean = TRUE), distribution.model = "norm")
garch_VIX <- ugarchfit(spec = garch_model_VIX, data = VIX)
sigma_VIX <- sigma(garch_VIX)
coef(garch_VIX)

# DCC
mspec <- multispec(c(garch_model_SP500, garch_model_VIX))
df <- data.frame(SP500, VIX = VIX$log_change)
dcc <- dccfit(dccspec(mspec, dccOrder = c(1,1), model = "DCC", distribution = "mvnorm"), df)
dcc_cor <- rcor(dcc)
coef(dcc)

## Let's extract some useful things from the GARCH and DCC results:
SPXcoef <- garch_SP500@fit$coef    #GARCH model parameter estimates for SPX
SPXsigma <- garch_SP500@fit$sigma  #standard deviations for each date
SPXz <- garch_SP500@fit$z          #GARCH shocks z for each date
VIXcoef <- garch_VIX@fit$coef    #GARCH model parameter estimates for VIX
VIXsigma <- garch_VIX@fit$sigma  #standard deviations for each date
VIXz <- garch_VIX@fit$z          #GARCH shocks z for each date
DCCcoef <- dcc@mfit$coef        #DCC parameter estimates (includes the GARCH estimates)
DCCR <- dcc@mfit$R              #DCC correlations for each date
DCCQ <- dcc@mfit$Q              #DCC q's for each date

###############  DNVaR  ###############
library(readxl)
path <- "/Users/changyushiuan/Desktop/2022Spring/FIN567/Project/Part1/F567.s2022.project.part1.solution.xlsx"
current_data <- read_excel(path, sheet = "Option prices", col_names = FALSE, range = "B3:B6")
current_date <- as.Date(current_data[[1,1]], origin = "1899-12-30")
current_index <- current_data[[2,1]]

options_data <- read_excel(path, sheet = "Option prices", range = "A9:U30")
portfolio_delta <- options_data$Delta[nrow(options_data)]
portfolio_vega <- options_data$Vega[nrow(options_data)]
vol <- SPXVIX$Close.y[row_index]

# Compute delta-normal VaR using the GARCH and DCC models
# Need the conditional means, variances, and correlation for the next date
SPXmean <- 0
VIXmean <- VIXcoef[1]+VIXcoef[2]*VIX$log_change[1000]
SPXvariance <- SPXcoef[1]+SPXcoef[2]*SP500$return[1000]^2+SPXcoef[3]*SPXsigma[1000]^2
VIXvariance <- VIXcoef[3]+VIXcoef[4]*VIX$log_change[1000]^2+VIXcoef[5]*VIXsigma[1000]^2  #first two params are for AR model
rhobar <- (1/1000)*SPXz %*% VIXz
q11 <- 1.0 + DCCcoef[9]*(SPXz[1000]^2 - 1) + DCCcoef[10]*(DCCQ[[1000]][1,1]-1)
q22 <- 1.0 + DCCcoef[9]*(VIXz[1000]^2 - 1) + DCCcoef[10]*(DCCQ[[1000]][2,2]-1)
q12 <- rhobar + DCCcoef[9]*(SPXz[1000]*VIXz[1000] - rhobar) + DCCcoef[10]*(DCCQ[[1000]][1,2]-rhobar)
rho <- q12/sqrt(q11*q22)
rho

X1 <- portfolio_delta * current_index
X2 <- portfolio_vega * vol
expected_change <- X1 * SPXmean + X2 * VIXmean
variance <- X1^2 * SPXvariance + X2^2 *VIXvariance + 2 * X1 * X2 * rho * sqrt(SPXvariance) * sqrt(VIXvariance)
std <- sqrt(variance)
DNVaR <- abs(expected_change - 1.645 * std)
DNVaR
