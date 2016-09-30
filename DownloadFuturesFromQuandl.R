###########################################################################
# QUANDL DATA -------------------------------------------------------------
###########################################################################
#   Use this script to download futures data from quandl
#
#   Notes:
#   Quandl also has a CME database, but need to cat contract months
#   by hand.  this might be the way to go if I really need to get
#   my hands dirty.   Another option is the CHRIS database
#   which provides free continuous contracts, but is inconsistent
#   https://www.quandl.com/CHRIS/documentation/documentation
#   guy yollins uses the OFDP data set, the open futures data project
#   https://www.quandl.com/OFDP/documentation/documentation

#   better option?  quandl has a steven's continuous futures data
#   subscription available here for $50 per month, but only daily data:
#   https://www.quandl.com/subscribe/12834
#   looks like i would use the following for backtesting:
#       first of month roll rule
#       use calendar-weighted prices: gives best estimate of PL
#           (prices don't jump when contracts roll, which distorts
#             PNL for open positions.  requires first of month roll)
#   use this for trading:
#       open-interest-switch roll: roll contracts based when open interest
#           moves to the back month
#       unadjusted pricing:  don't adjust prices for trading decisions
#       drawback here:  indicators might jump when contracts roll, but
#           tracks more often what everybody else is seeing and
#           follows the most liquidity.  also support/resistance areas
#           found using this method are most common.
###########################################################################

# Initialize workspace
suppressWarnings(try(rm(list=ls()),silent=TRUE))   # clears the workspace
switch(Sys.info()[['sysname']],
       Windows = {setwd("c:/Users/jharper/Dropbox/03_School/Programming/R/FileDirectory/")},
       Linux = {stop("hold on, this is a Linux Box!")},
       Darwin ={setwd("/Users/harper/Dropbox/HarpFolders/04_Code/R/FileDirectory/")}
)
require(quantstrat)
require(Quandl)
options(digits.secs=6)  # shouldn't need to change this often

## Variables
periodicity <- "daily"
month <- "1"  # 1 for front month, 2 for 2nd month, etc.
myCode <- "nzqMsaBpoWYcyFq72T5s"  # API Auth code for quandl
startDate <- "1950-01-01"
output.dir <- "/users/harper/Dropbox/HarpFolders/02_Trading/Data/RawFiles/1_RawCSVFiles/"

## CME Products
my.tickers <-  c("AD", "BP", "CD", "EC", "JY", "SF", "CL", "ES", "GC", "O",  "NG", "NQ", "QG", "QM", "SI", "YM", "US", "C",  "FV", "BO", "SM", "TY", "O",  "S",  "TU", "W")
real.names <-  c("6A", "6B", "6C", "6E", "6J", "6S", "CL", "ES", "GC", "HO", "NG", "NQ", "QG", "QM", "SI", "YM", "ZB", "ZC", "ZF", "ZL", "ZM", "ZN", "ZO", "ZS", "ZT", "ZW")
dataSource <- 2

# Eurex Products
# dataSource <- 3
# my.tickers <-  c("FDAX", "FESX", "FGBL", "FGBM", "FGBS", "FGBX")
# real.names <- my.tickers

# Other products:
# "SB ## ICE exchange
## "GM", "SIM"   ## MCX Exchange ticker names
## "YG", "YI"    ## MCX Exchange real names

switch(dataSource,
   "1" = {dataSource <- "OFDP/FUTURE_"}, # open financial data project
   # OFDP doesn't have ES Close prices for most of the series
   "2" = {dataSource <- "CHRIS/CME_"}, # continuous contracts
   "3" = {dataSource <- "CHRIS/EUREX_"} # continuous contracts
)

## Load Data From Quandl
# s <- 1
for (s in seq(my.tickers)) {
  try(
      x <- Quandl(code = paste(dataSource, my.tickers[s], month, sep = ""),
                  authcode = myCode, 
                  #type = "xts"
                  type = "raw"
                  )
  )
  if (exists("x")) {
    ## Cleaning / Renaming columns
    if(dataSource == "OFDP/FUTURE_") {
      # cleaning ODFP database:
      colnames(x)[4] <- "Close"
      x <- x[,1:5]   # take out the open interest column
      name <- paste("daily", real.names[s], "quandlCHRIS", sep = ".")

    } else if (dataSource == "CHRIS/CME_") {
      # cleaning CHRIS/CME database:
      #colnames(x)[6] <- "Close"
      #x <- x[,c(1:3,6,7)]
      name <- paste("daily", real.names[s], "quandlCHRIS", sep = ".")

    } else if (dataSource == "CHRIS/EUREX_") {
      # cleaning CHRIS/EUREX database:
      colnames(x)[4] <- "Close"
      x <- x[,c(1:5)]
      name <- paste("daily", real.names[s], "quandlCHRIS", sep = ".")

      } else stop("dataSource is invalid")

    ## put product name in the columns?
    #colnames(x) <- paste(s, colnames(x), sep =".")

    ## basic zerovalue data cleaning
    # cat("\nNumber of NAs in", s, "is:", sum(is.na(x)))
    # cat("\nNumber of 0 values in", s, "is:", sum(coredata(x) == 0))
    # cat("\nReplacing 0 values with NAs....")
    # x[x == 0] <- NA
    # cat("\nNumber of 0 values now in", s, "is:", sum(coredata(x) == 0))
      # x <- na.approx(x)  # na.approx interpolates NA values
    assign(name, x)
    rm(x)
  }
}


## Save as csv
tickers <- ls(pattern = "daily")
# x <- tickers[s]
for (s in seq(tickers)) {
  print(tickers[s])
  write.csv(get(tickers[s]), file = paste0(output.dir, tickers[s], ".txt"))
}
