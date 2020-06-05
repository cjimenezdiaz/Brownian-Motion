# Libraries
library(tidyquant)
library(lubridate)
library(TTR)
library(dplyr)

# Download Prices
DDBB <- "SPY" %>%
    tq_get(get  = "stock.prices",
           from = Sys.Date() - years(100),
           to   = Sys.Date()) %>%
    mutate(return = ROC(adjusted, n = 1)) %>%
    na.omit() 

# Apply a filter
DDBB_Filter <- DDBB %>%
    filter(date > "2008-01-01" & date < "2009-03-01")

# Create Geometric Brownian Motion
set.seed(round(runif(1, min = 0, max = 1000)))

nsim  <- 50
S0    <- last(DDBB$adjusted)
mu    <- mean(DDBB_Filter$return)
sigma <- sd(DDBB_Filter$return)
t     <- 180

gbm   <- matrix(ncol = nsim, nrow = t)

for (simu in 1:nsim) {
    for (day in 2:t) {
        epsilon        <- rnorm(t)
        dt             <- 1 / t
        gbm[1, simu]   <- S0
        gbm[day, simu] <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon[day] * sqrt(dt))
    }
}

gbm <- apply(gbm, 2, cumprod)

ts.plot(gbm, gpars = list(col=rainbow(10)))

# Statistic Analysis
a <- gbm %>%
    t() %>%
    as.data.frame() %>%
    select(180)
    
sum(a < S0)/50
