##########################################

#   Harp Functions
#
#   Date:  2014-11-13

##########################################


###########################################################################
# Set Environment ---------------------------------------------------------
###########################################################################
## Info for updating R and important packages:
# install.packages("installr");  # for windows only
# require(installr); updateR()
# install.packages("quantstrat", repos="http://R-Forge.R-project.org", type = "source")
# install.packages("blotter", repos="http://R-Forge.R-project.org", type = "source")
# install.packages("FinancialInstrument", repos="http://R-Forge.R-project.org", type = "source")
library(quantstrat)
library(PerformanceAnalytics)
dir.unittests <- "../02_Strats/lib/UnitTests/Data/"
dir.data      <- switch(Sys.info()[['sysname']],
                               Windows = {"C:/Users/jharper/documents/R/HarpStrats/01_Scripts/UnitTests/Data/"}, 
                               Linux   = {stop("hold on, this is a Linux Box!")},
                               Darwin  = {"/Users/harper/Dropbox/HarpFolders/02_Trading/Data/"})


## Stuff to set each session
Sys.setenv(TZ = "America/Chicago")     ## Sys.setenv(TZ="UTC") # default is America/Chicago, UTC is recommended
options(digits.secs = 6)  # shouldn't need to change this often
loadInstruments(dir = "../02_Strats/", file_name = "InstrumentDefs.Rdata")
# params <- list()
# params$colorset <-  rainbow12equal
#   to set equal, use rainbow12equal, greenmono, rich12equal, or tim12equal
#   to highlight one series, use redfocus, greenfocus, bluefocus


## Set custom chart theme for chart.Posn
## (helps show entries/exits more clearly)
#params$my.theme               <- chart_theme()
#params$my.theme$col$dn.col    <- 'lightslategray'
#params$my.theme$col$up.col    <- 'lightgray'
#params$my.theme$col$dn.border <- 'lightgray'
#params$my.theme$col$up.border <- 'lightgray'


# #########################################################################
# Functions ---------------------------------------------------------------
# ######################################################################### 
LoadPrices <- function(product, date, dir.prices = "//gv-wsjh1/ITGTickData", price.file = "Trades", period = "minutes", periodicity = NULL, output.vol = FALSE) {
  # Desc: Wrapper for load function, loads price files
  # Args
  #   product             like "CL" or "ES", etc
  #   date                char date,  like "2015-06-11"
  #   dir.prices          where to search for price files
  # Returns
  #   returns a named list of prices
  # -----------------------------------------------------------------------
  ## Filter by date, then by product, then by trades/bids/offers
  files  <- list.files(dir.prices, full.names = F, recursive = FALSE)
  prices <- subset(files, grepl(product, files) & grepl(date, files) & grepl(price.file, files))
  
  ## Load prices to a temp environment
  #if (!exists(prices) || xts::periodicity(get(prices))[[2]] > periodicity) {}
  #temp <- new.env()
  prices <- sapply(prices, function(file) load(paste0(dir.prices, "/", file), envir = globalenv()))
  if (length(prices) == 0) stop("ERROR: No match for product ", product, " and date ", date, " and price.file = ", price.file)
  if (length(prices) >  1) stop("ERROR: Multiple matches for product ", product, " date ", date, " and price.file = ", price.file)
  x <- get(prices, envir = globalenv())
  
  # Convert to OHLC minute data if needed
  if (!is.null(periodicity)) {
    #lapply(x, FUN = function(x) ConvertTickToMinute(x, periodicity))
    header <- unlist(strsplit(prices, "[.]"))[1]
    x <- ConvertTickToMinute(x, period, periodicity, header)
  } 
  return(x)                                                    
}

Multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
HarpColors <- function(color, set = "bold") {
  # 3 sets of colors:
  x <- list()
  # bold set
  x$bold <- c("#e41a1c", # 1 red
              "#377eb8", # 2 blue
              "#4daf4a", # 3 green 
              "#984ea3", # 4 purple 
              "#ff7f00", # 5 orange
              "#f781bf", # 6 pink
              "#a65628", # 7 brown
              "#999999", # 8 grey
              "#ffff33") # 9 yellow

  # pastel set
  x$pastel <- c("#8dd3c7", # light blue
                "#ffffb3", # light yellow
                "#bebada", # light purple
                "#fb8072", # light red
                "#80b1d3", # light blue
                "#fdb462", # light orange
                "#b3de69", # light green
                "#fccde5", # light pink
                "#d9d9d9") # light grey
  
  # light candle chart set
  x$ltcandle <- c("#fff7fb", # very light grey
                  "#a6bddb", # darker blue grey
                  "#c994c7", # grey-red
                  "#edf8fb", # light green
                  "#d4b9da", # light red
                  "#c994c7", # darker red
                  "#c7e9c0", # green 2 
                  "#f2f2f2", # grey
                  "#fddaec", # pink
                  "#e5d8bd") # brown
                  
  # colorblind set
  x$cb <- c("#a6cee3", # light blue
            "#1f78b4", # dark blue
            "#b2df8a", # light green
            "#33a02c", # dark green
            "#fb9a99") # pink
  return(x[[set]][color])
}
ConvertXtsToDataFrame <- function(xts.object, names) {
  # convert xts/zoo objects to data frames, when i want to use the index as a column
  # this is often a hassle, as one of the columns turns into a factor and its a pain
  # to bind the index to the column.
  timestamps    <- as.data.frame(format(index(xts.object)), stringsAsFactors = FALSE)
  output        <- cbind(timestamps, as.data.frame(xts.object), row.names = 1:nrow(xts.object))
  names(output) <- names
  return(output)
}
FindNumberOfDecimalPlaces <- function(x) {
  # submit a numeric value and find out how many decimal places there are
  # must end with .001 not .000
  #
  if (x %% 1 != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

CalcReturnOnMargin <- function(trades, margin, max.qty = NULL) {
  # takes in a trades dataframe and adds a return on margin and cm.return column
  # return on margin = log(1 + (trade.pnl / (margin * max.qty)))
  # uses log returns so they are additive
  # says "what is the return on this profit, given the captial allocated to generate this profit?"
  #
  # args
  #   trades            this is a trades dataframe with columns like: side, qty, cm.pnl, price, etc
  #   margin            margin rate for the futures product
  #   cm.pnl.colname    character name for the cm.pnl column.  
  #   max.qty           this is the maximum qty the strategy can take.  used to calculate what the  
  #                     total capital allocated to generate the profit. if null, calculate from the frame
  # returns
  #   the original dataframe with a new return and cm.return column
  # ---------------------------------------------------------------------
  if (is.character(trades)) trades <- get(trades)
  stopifnot(is.data.frame(trades),
            is.numeric(margin), 
            (is.null(max.qty) || is.numeric(max.qty)))
  if (!all((c("qty", "cm.pnl") %in% names(trades)))) stop("ERROR: colnames missing qty, or cm.pnl")
  if (all(c("returns", "cm.returns", "trade.pnl") %in% names(trades))) {
    warning("this is a re-run of the CalcReturnOnMargin function.  no harm, just wasting processing time :)")
    trades <- trades[, -match(c("returns", "cm.returns", "trade.pnl"), colnames(trades))]  # remove extra columns
  }
  
  ## calculate max.qty
  if (is.null(max.qty)) {
    # qty        <- ifelse(trades$side == "S", -1 * trades$qty, trades$qty)
    cm.qty     <- cumsum(trades$qty)
    max.qty    <- max(abs(cm.qty)) 
  }
  ## calculate returns
  trades$cm.pnl  <- as.numeric(trades$cm.pnl)
  trade.pnl      <- c(trades$cm.pnl[1], diff(trades$cm.pnl))
  returns        <- log(1 + (trade.pnl / (max.qty * margin)))
  if (any(is.nan(returns))) stop("ERROR: getting NANs in the return calculation.  is there a negative PNL < -(qty * margin) to create a log(-1) issue?")
  cm.returns     <- cumsum(returns)
  
  ## append to original dataframe
  output <- cbind(trades, returns, cm.returns, trade.pnl)
  return(output)
}

SearchAndExclude <- function(pattern, exclude=NULL) {
  # use in place of a ls(pattern =) when you want to search for objects but exclude some of them
  #
  # args
  #   pattern       string of the pattern to search for
  #   exclude       character vector of things to subset out of the output. if null, just an ls()
  # returns
  #   character vector of objects in the global environment
  # --------------------------------------------------------------
  x <- ls(pattern = pattern, envir = globalenv())
  for (char in exclude) {
    x <- x[!grepl(char, x)]
  }
  return(x)
}


# RegisterInstrument ------------------------------------------------------
RegisterInstrument <- function(ticker.name, instrument.type, multiplier,
                               currency, tick.size, identifiers,
                               #instrument.def.dir= paste0(dir.data, "StrategyTesting/1_Full/"),
                               instrument.def.dir= paste0("./Scripts/HarpStrats/"),
                               backup.dir=paste0(instrument.def.dir, "backup/"),
                               verbose = TRUE){
  # Registers the ticker as a financial instrument for strategy testing
  #
  # Args
  #   ticker.name       the character name of the time series object
  #   instrument.type   either stock, future, or currency
  #   multiplier        multiply times the price to find the notional value
  #   tick.size         minimum increment
  #   identifiers       aliases for the instrument
  #
  # Returns
  #   sets the instrument for use in strateyg testing
  if (instrument.type %in% c("stock", "future", "currency")) {
    switch(instrument.type,
           "stock"      = {stock(ticker.name, currency = currency,
                                 multiplier = multiplier,
                                 identifiers = identifiers,
                                 tick_size = tick.size)},
           "future"     = {future(ticker.name, currency = currency,
                                  multiplier = multiplier,
                                  identifiers = identifiers,
                                  tick_size = tick.size)},
           "currency"   = {currency(ticker.name,
                                    multiplier = multiplier,
                                    identifiers = identifiers,
                                    tick_size = ticksize)}
           )
  # backup definition file and save a new definition file
  file.name <- paste0(instrument.def.dir, "InstrumentDefs.Rdata")
  date <- Sys.Date()
  backup.file <- paste0(backup.dir, "InstrumentDefs_", date, ".Rdata")
  file.rename(from = file.name, to = backup.file)
  saveInstruments(file_name = "InstrumentDefs.Rdata", dir = instrument.def.dir)
  } else stop ("Instrument type must be stock, future, or currency")
  if (verbose) {
    x <- getInstrument(ticker.name)  # get instrument info
    cat("\nInstrument Info")
    print(x)
    cat("\n\nInstrument List\n", ls(envir=FinancialInstrument:::.instrument))
  }
}


# DeleteEntireDay ---------------------------------------------------------
DeleteEntireDay <- function(symbol.name, bad.data) {
# Extracts day from bad.data xts object and deletes the entire day
  #
  # Pass in an xts object and an xts object with bad data, removes the entire day
  #
  # Args
  #   symbol.name     name of xts.object
  #   bad.data        subset of xts.object with bad observations
  #
  # Returns
  #   the original data with the bad days subtracted out
  #

  x <- get(symbol.name)
  if(is.xts(x) && is.xts(bad.data)) {
    ## extract the indexes from bad data
    # Not working great:  bad.days <- as.character(index(to.daily(bad.data)))
    bad.days <- as.character(strptime(index(bad.data), format = '%Y-%m-%d'))

    ## for debugging
    cat("\nThese are the days to be deleted")
    as.data.frame(print(bad.days))

    delete.these.indexes <- index(x[as.character(bad.days)])
    output <- x[!(index(x) %in% delete.these.indexes)]
    cat("\nComplete!")
    return(output)
  } else stop("symbol.name and bad.data must both be xts files")
}


# CalcRatioExpectedMinutes ------------------------------------------------
CalcRatioExpectedMinutes <- function(xts.object, filter.threshold = .8,
                                     start.time = "05:00",
                                     end.time = "15:00",
                                     timer = FALSE,
                                     verbose = TRUE) {
  # For intraday data, calculates the ratio of observed minutes / expected minutes
  #
  # Args
  #   xts.object          pass in an xts object
  #   start.time          start time of the interval
  #   end.time            end time of the interval
  #   filter.threshold    filter out anything above this ratio
  #
  # Returns
  #   vector of dates and ratios, ordered smallest to largest
  if(timer) timer.start <- Sys.time()
  if(is.xts(xts.object) && periodicity(xts.object)[[6]] == "minute"){

    ## calculate interval and expected minutes
    window <- paste0("T", start.time, "/", "T", end.time)
    time.2 <- paste0("2015-03-18 ", end.time)
    time.1 <- paste0("2015-03-18 ", start.time)
    expected.minutes <- as.numeric(difftime(time.2, time.1, units = "min"))

    ## subset and find the number of rows in each day
    x <- xts.object[window]
    ep <- endpoints(x, on = "days")
    output <- period.apply(x, INDEX = ep, FUN = function(x) nrow(!is.na(x)))/expected.minutes
    colnames(output)[1] <- "expected.minutes.ratio"
    output <- output[output < filter.threshold]

    if (timer) {
      timer.end<-Sys.time()
      cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
    }

    if(verbose){
      return(output)
    } else return(nrow(output))
  } else stop("xts.object needs to point to an intraday XTS object")
}


# InspectData -------------------------------------------------------------
InspectData <- function(symbol.name, bad.data, start.time = "05:00", end.time = "15:00") {
  # Given a data frame, chart and view the data
  #
  # Args
  #   symbol.name     name of OHLCV xts.object
  #   bad.data        subset of xts.object with timestamp and bad observations
  #   n.before        period to inspect before the time
  #   n.after         period to inspect after the time
  #   start.time      beginning of trading hours
  #   end.time        end of trading hours
  #
  # Returns
  #   plots a chart of trading hours for each day
  #   opens the View tab for n.before and n.after for each day
  x <- get(symbol.name)
  if (is.xts(bad.data) && is.OHLCV(x)){
    for(i in seq(nrow(bad.data))){

      ## Extract times
      timestamp <- format(index(bad.data[i,]))
      date <- strptime(timestamp, format = '%Y-%m-%d')
      window <- paste0(date, " ", start.time, "/", date, " ",end.time)
      #print(window)  # for debugging


      ## Plot chart and view data
      par(ask = TRUE)
      try(plot(Cl(x[window]), main = paste(symbol.name, date)))
      print(window)  # for debugging
      View(x[window], title = symbol.name)
      par(ask = FALSE)
    }
  } else stop("symbol.name and bad.data must both be xts files")
}


# SaveXTStoRdata ----------------------------------------------------------
SaveXTStoRdata <- function(xts.files, output.dir, input.dir = NULL, archive.dir = NULL){
  # Wrapper for for SaveToRdata function
  # Date:    2015-03-15
  #
  # Args
  #   xts.files       char vector of xts files in memory
  #   output.dir      directory to save the data
  #   input.dir       location of original file (optional)
  #   archive.dir     destination directory for original file (optional)
  #
  # Returns
  #   saves files to output directory in format: xtsName.FIRSTDATE.TO.LASTDATE.rdata
  #
  for(file in xts.files){
    ## Create output name like Periodicity.Product.Source.First.TO.Last.Rdata
    First <- as.Date(index(first(get(file))))
    Last  <- as.Date(index(last(get(file))))

    ## Save data as Rdata file in output directory
    SaveToRdata(x = file, startDate = First, endDate = Last,
                outputDirectory = output.dir)

    ## Move original file to archiveDir
    if (!is.null(input.dir) && !is.null(archive.dir)) {
        cat("\nMoving original file to archive directory")
        output.name <- paste(file, First, "TO", Last, "Rdata", sep = ".")
        old.location <- paste0(input.dir, output.name)
        new.location <- paste0(archive.dir, output.name)
        ifelse(file.exists(old.location), file.rename(from = old.location, to = new.location),
               warning("ERROR: file does not exist in input directory"))
        cat("File move is complete")
    }
  }
}


# AddMissingMinutes -------------------------------------------------------
AddMissingMinutes <- function(ohlcv.symbol, replace.volume = FALSE,
                              locf.prices = FALSE, weekdays.only = TRUE,
                              start.time = "05:00", end.time = "15:00") {
    # Add missing minutes to data set, then replaces
    # NAs in volume column with 0, LOCF the price data
    #
    # Args
    #   ohlcv           must be the name of an ohlcv object with "Volume" column and price columns
    #   replace.volume  if TRUE, replaces NA volumes with 0 volume
    #   locf.prices     if TRUE, replaces NA prices with last observation carried forward
    #   weekdays.only   if TRUE, subsets out only weekdays
    #   start.time      beginning of interval to add minutes.  set to 00:00 to catch all
    #   end.time        end of interval to add minutes.  set to 23:59 to catch all
    #
    # Returns
    #   returns the object with added time and substitutions
    #
    # Date:    2015-03-15
    x <- get(ohlcv.symbol)
    if (is.OHLCV(x)) {

        ## create an empty xts object, subset only the trading hours
        ## and merge with the original object to fill in missing times
        window <- paste0(first(index(x)), "/", last(index(x)), "/M")
        trading.hours <- paste0("T", start.time, "/", "T", end.time)
        empty <- as.xts(timeBasedSeq(x = window))
        x <- merge(x, index(empty[trading.hours]))
        # empty <- zoo(order.by = seq(first(index(x)), last(index(x)), by="1 min"))
        # x <- merge(x, empty)

        if (any(replace.volume, locf.prices)) {
            x <- ReplaceNAs(x, replace.volume, locf.prices)
        }
        if (weekdays.only) x <- x[.indexwday(x) %in% 1:5]
        return(x)
    } else stop("ReplaceNAs requires OHLCV data" )
}


# ReplaceNAs --------------------------------------------------------------
ReplaceNAs <- function(ohlcv.data, replace.volume = TRUE, locf.prices = TRUE,
                       interpolate.prices = FALSE, interpolate.volume = FALSE){
    # Replaces NAs in volume column with 0, LOCF the price data
    #
    # Args
    #   ohlcv               must be an ohlcv object with "Volume" column and price columns
    #   replace.volume      if TRUE, replaces NA volumes with 0 volume
    #   locf.prices         if TRUE, replaces NA prices with last observation carried forward
    #   interpolate.prices  if TRUE, replaces NAs in OHLC columns using linear interpolation
    #   interpolate.volume  if TRUE, replaces NAs in volume column using linear interpolation
    #
    # Returns
    #   returns the object with replaced NAs
    #
    # Date:    2015-03-15
    if (is.OHLCV(ohlcv.data)) {

        if (replace.volume) ohlcv.data[, "Volume"][is.na(ohlcv.data[, "Volume"])] <- 0
        if (locf.prices) ohlcv.data[, 1:4] <- na.locf(ohlcv.data[, 1:4], maxgap = 20)
        if (interpolate.prices) ohlcv.data[, 1:4] <- na.approx(ohlcv.data[, 1:4])
        if (interpolate.volume) ohlcv.data[, "Volume"] <- na.approx(ohlcv.data[, "Volume"])

        return(ohlcv.data)
    } else stop("ReplaceNAs requires OHLCV data" )
}


# LoadRData ---------------------------------------------------------------
LoadRData <- function(input.dir,rdata.files=NULL ){
  #  Loads any rdata file names passed in or found in an input directory
  #
  # Args
  #   input.dir       input directory to look for *.Rda files
  #   rdata.files     vector of char file names or leave as NULL
  #
  # Returns
  #   loads the rdata file to the global environment
  #
  # Date:    2015-03-11
  if(is.null(rdata.files)) {
      rdata.files <- list.files(path = input.dir, pattern = "*.Rda")
  }
  if(length(rdata.files) == 0){
      stop("No Rdata files in the input directory!")
  } else {
       for(file in rdata.files){
          cat("\nLoading Rdata File:", file)
          load(file = paste0(input.dir, file), envir = .GlobalEnv )

          ## strip the extra date stuff off the object name
          x <- unlist(strsplit(file, split = "[.]"))
          newName <- paste(x[1], x[2], x[3], sep = ".")
          oldName <- substring(file, 1, last = (nchar(file) - nchar(x[7]) - 1))
          assign(newName, get(oldName), envir = .GlobalEnv)
          rm(list =  oldName, envir = .GlobalEnv)
       }
  }
}


# FindXtsFiles ------------------------------------------------------------
## search all objects and return a character vector of XTS object names
## input:  character vector of all Files (use ls())
## output:  character vector of only XTS files

FindXtsFiles <- function(allFiles = ls()){
    myXtsFiles <- NULL
    for(file in allFiles){
        if(is.xts(get(file))) {
            myXtsFiles <- append(file, myXtsFiles)
        }
    }
    return(myXtsFiles)
}


# WriteCharts -------------------------------------------------------------
WriteCharts <- function(periodicity = "daily", myTickers, outputDir, logFile ){
    ## take loaded xts files, covert to desired periodicity
    ## and save as PDF candleCharts
    ## 2015-02-27
    start_t<-Sys.time()
    cat("\n---------------------------------------\n", file = logFile, append = TRUE)
    cat("\nWriting Charts.......\n")
    cat(date(), "\n", file = logFile, append = TRUE, sep = "")
    for(ticker in myTickers){
        cat("\nWriting Ticker:", ticker, file = logFile, append = TRUE, sep = "")

        if(periodicity == "weekly")   x <- to.weekly(get(ticker))
        if(periodicity == "monthly")  x <- to.monthly(get(ticker))
        if(periodicity == "daily")    x <- to.daily(get(ticker))
        if(periodicity == "hourly")   x <- to.hourly(get(ticker))
        if(periodicity == "minute")   x <- to.minutes(get(ticker))

        pdf(file = paste0(chartDir, ticker, ".pdf"))
        try(chartSeries(x, theme = "white", name = ticker))
        dev.off()
    }
    end_t<-Sys.time()
    cat("\n\nRunTime: ", (end_t-start_t), file = logFile, append = TRUE)
    cat("\n---------------------------------------\n", file = logFile, append = TRUE)
    cat("\nWriteCharts Complete\n")
}


# FindZeroValues ----------------------------------------------------------
FindZeroValues <- function(xts.object, search.volumes = FALSE,
                           timer = FALSE,
                           verbose = TRUE) {
    # Subsets rows of xts data if there is a 0 value in that row
    #
    # Args
    #   xts.object:     pass in an xts objectxts.object
    #   verbose:        if TRUE, returns a subset of the original object, if FALSE,
    #                   just returns a count
    #
    # Returns
    #   either a subset of the object or a count of rows with 0 values
    #
    # Date:    2015-03-11
  if (timer) timer.start <- Sys.time()

  if (is.xts(xts.object)) {
      #xts.object <- get(xts.object)
      ## check each row for any value == 0 and then subset
      if (search.volumes == FALSE) {
        #xts.object <- OHLC(xts.object)
        has.zero <- apply(OHLC(xts.object), 1, function(row) any(row == 0 ))
      } else {
        has.zero <- apply(xts.object, 1, function(row) any(row == 0 ))
      }
      output <- xts.object[has.zero,]
  } else stop("xts.object needs to be an XTS object")

  if (timer) {
    timer.end<-Sys.time()
    cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
  }

  if(verbose){
    return(output)
    } else return(nrow(output))
}


# FindNAs -----------------------------------------------------------------
FindNAs <- function(xts.object, timer = FALSE, verbose = TRUE){
  # Subsets rows of xts data if there is a NA value in that row
  #
  # Args
  #   xts.object:     pass in an xts objectxts.object
  #   verbose:        if TRUE, returns a subset of the original object, if FALSE,
  #                   just returns a count
  #
  # Returns
  #   either a subset of the object or a count of rows with 0 values
  #
  # Date:    2015-03-11

  if (timer) timer.start <- Sys.time()

  if(is.xts(xts.object)){
    #xts.object <- get(xts.object)
    ## check each row for any value == 0 and then subset
    has.na <- apply(xts.object, 1, function(row) any(is.na(row)))
    output <- xts.object[has.na,]
  } else stop("xts.object needs to be an XTS object")

  if (timer) {
    timer.end<-Sys.time()
    cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
  }

  if(verbose){
    return(output)
  } else return(nrow(output))
}


# FindExtremeTrueRange ----------------------------------------------------
FindExtremeTrueRange <- function(xts.object, atr.multiplier = 4,
                                 start.time = "05:00", end.time = "15:00",
                                 timer = FALSE,
                                 verbose = TRUE){
  # Subsets data where there is an "extreme" True Range in the price series
  #
  # True Range = maximum of these values:
  #  	todays range:	       high - low
  #	prev.close to high:	   abs(high - previous close)
  #	prev.close to low: 	   abs(low - previous close)
  #
  # Args
  #   xts.object:     pass in an xts objectxts.object
  #   atr.multiplier: "extreme" defined as  atr.multiplier * atr$atr
  #   verbose:        if TRUE, returns a subset of the original object, if FALSE,
  #                   just returns a count
  #
  # Returns
  #   either a subset of the object or a count of rows with 0 values
  #
  # Date:    2015-03-11

  if (timer) timer.start <- Sys.time()

  if(is.xts(xts.object)){
    periodicity <- periodicity(xts.object)[[6]]
  } else stop("xts.object needs to be an XTS object")

  if (periodicity %in% c("minute", "daily")) {
    ## subset where TR > (atr.multiplier * ATR)
    atr <- ATR(HLC(na.omit(xts.object)), n = 20, "EMA")[,1:2]
    atr$atr <- atr$atr * atr.multiplier
    colnames(atr) <- c("TrueRange", "ATR")
    x <- cbind(xts.object, atr )

    ## Intraday only:  subset trading hours interval
    if (periodicity == "minute"){
      trading.hours <- paste0("T", start.time, "/", "T", end.time)
      x <- x[trading.hours]
    }

    ## subset x where the ranges are extreme
    output <- x[with(x, TrueRange > ATR),]
  } else stop("Periodicity needs to be either daily or minute")

  if (timer) {
    timer.end<-Sys.time()
    cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
  }

  if(verbose){
    return(output)
  } else return(nrow(output))
}


# FindOutsideBBand --------------------------------------------------------
FindOutsideBBand <- function(xts.object, sd.multiplier = 4,
                             start.time = "05:00", end.time = "15:00",
                             timer = FALSE,
                             verbose = TRUE){
  # Subsets data where there is close outside of bollinger bands
  #
  # Args
  #   xts.object:     pass in an xts objectxts.object, either daily or minute
  #   sd.multiplier:  plots bollinger bands as x * standard deviation from moving average
  #   verbose:        if TRUE, returns a subset of the original object, if FALSE,
  #                   just returns a count
  #
  # Returns
  #   either a subset of the object or a count of rows with 0 values
  #
  # NOTES:
  #   what to use here?  Could use:
  #       Low < DnBand || High > UpBand # this would be caught with ATR filter
  #       PctB > 1 || PctB < 0 # this might be more valuable, since uses close
  #       same as above, but use typical price?  wasn't great when i tested this
  #
  # Date:    2015-03-11
  if (timer) timer.start <- Sys.time()

  if(is.xts(xts.object)){
    periodicity <- periodicity(xts.object)[[6]]
  } else stop("xts.object needs to be an XTS object")

  if (periodicity %in% c("minute", "daily")) {
    ## subset where the close price is outside of the BolBands
    bb <- BBands(Cl(na.omit(xts.object)), n = 20,
                 maType = "SMA", sd = sd.multiplier)[, c(1,3,4)]
    colnames(bb) <- c("DnBand", "UpBand", "PctB")
    x <- cbind(xts.object, bb)

    ## Intraday only:  subset trading hours interval
    if (periodicity == "minute"){
      trading.hours <- paste0("T", start.time, "/", "T", end.time)
      x <- x[trading.hours]
    }
    output <- x[with(x, PctB < .00 | PctB > 1),]
  } else stop("Periodicity needs to be either daily or minute")

  if (timer) {
    timer.end<-Sys.time()
    cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
  }

  if(verbose){
    return(output)
  } else return(nrow(output))
}


# FindIntradayPriceJump ---------------------------------------------------
# FindIntradayPriceJump <- function(xts.object, pct.jump.threshold = .10,
#                                   start.time = "05:00", end.time = "15:00",
#                                   timer = FALSE,
#                                   verbose = TRUE){
#   # Description: finds intraday jumps in close-to-close price series
#   #
#   # Args
#   #   xts.object:         pass in an xts object
#   #   pct.jump.threshold  finds values where the return is greater than x * prev return
#   #   start.time          beginning of interval.  for all day, set to 00:00
#   #   end.time            end of interval.  for all day, set to 23:59
#   #   verbose:            if TRUE, returns a subset of the original object, if FALSE,
#   #                         just returns a count
#   #
#   # Returns
#   #   either a subset of the object or a count of rows flagged
#   #
#   # Date:    2015-03-11
#   if (timer) timer.start <- Sys.time()
# 
#   if(is.xts(xts.object) && periodicity(xts.object)[[6]] == "minute"){
# 
#     ## calculate returns (need to lag here - don't want forward returns,
#     ##   want to see the actual prices that caused the bad returns)
#     returns <- lag(abs(ROC(xts.object[,1:4], type = "discrete", na.pad = FALSE)))
# 
#     ## subset out trading hours - don't care about jumps outside of trading
#     trading.hours <- paste0("T", start.time, "/", "T", end.time)
#     returns <- returns[trading.hours]
# 
#     ## find endpoints that mark the end of each day in the series
#     ep <- endpoints(returns, "days")
# 
#     ## pass in a set of returns, test each row to see if any element
#     ## in the row is greater than the threshold
#     ## outputs the values in a list
#     JumpTest <- function(returns, xts.object){
#         # for each row of the return, is any value > threshold?
#         has.jumps <- apply(returns, 1, function(row) any(row > pct.jump.threshold))
#         # output a logical vector where TRUE = jumps in that row
#         return(has.jumps)
#     }
# 
#     ## apply the JumpTest function to the period subsets
#     ## between the endpoints
#     ## then bind the list elements and use to subset the xts object
#     has.jumps <- period.apply(returns, INDEX = ep, FUN = JumpTest)
#     output <- xts.object[unlist(has.jumps), ]
# 
#   } else stop("xts.object needs to be an intraday XTS object")
# 
#   if (timer) {
#     timer.end<-Sys.time()
#     cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
#   }
# 
#   if(verbose){
#     return(output)
#   } else return(nrow(output))
# }


# FindIntradayGaps --------------------------------------------------------
FindIntradayGaps <- function(xts.object, min.gap.length = 4, start.time = "05:00",
                             end.time = "15:00", timer = FALSE, verbose = TRUE){
  # Description: finds gaps during trading hours greater than gap.length.minutes
  #
  # Args
  #   xts.object:         pass in an xts object
  #   gap.length.minutes  maximum allowable gap length before
  #   start.time          beginning of time interval
  #   end.time            end of time interval
  #   verbose:            if TRUE, returns a subset of the original object, if FALSE,
  #                         just returns a count
  #
  # Returns
  #   either a vector of days that have gaps > threshold or
  #   a count of days that have been flagged
  #
  # Date:    2015-03-11
  if (timer) timer.start <- Sys.time()

  if(is.OHLCV(xts.object) && periodicity(xts.object)[[6]] == "minute"){
    trading.hours <- paste0("T", start.time, "/", "T", end.time)
    x <- xts.object[trading.hours]
    x <- cbind(x[-nrow(x)], diff(index(x)))
    x <- x[-endpoints(x, "days"), ]  # endpoints gaps across the interval
    colnames(x)[6] <- "gap.length.minutes"
    output <- x[with(x, gap.length.minutes > min.gap.length), 6]
  } else stop("xts.object needs to be an intraday OHLCV XTS object")

  if (timer) {
    timer.end<-Sys.time()
    cat("\nRunTime: ", (timer.end - timer.start), "\n\n")
  }

  if (verbose){
    return(output)
  } else return(nrow(output))
}


# CheckData ---------------------------------------------------------------
CheckData <- function(my.tickers, log.file="", max.output=20,
                      function.timer = TRUE, verbose=TRUE,
                      find.zero.values=FALSE,
                      find.NAs=FALSE,
                      find.extreme.truerange=FALSE,
                      find.outside.bband=FALSE,
                      find.intraday.pricejump=FALSE,
                      find.intraday.gaps=FALSE,
                      calc.ratio.exp.minutes=FALSE,
                      sd.multiplier=4,
                      atr.multiplier=4,
                      pct.jump.threshold=.1,
                      min.gap.length=4,
                      filter.threshold=.8){
    # A wrapper that calls a bunch of data checking functions
    #
    # Args
    #   my.tickers:     symbols for XTS objects in memory
    #   log.file:       if "", outputs to console, otherwise outputs to this file
    #   max.output:     max number of found values to print to the file per method
    #   verbose:        if true, spits out bad data, otherwise just a count
    #   find*:          on/off switch to calculate using this method
    #
    # Returns
    #   counts of bad data from each subfunction, and if count
    #   is less than a threshold, prints them out to either
    #   a file or the console
    #
    # Date:    2015-03-10
    #
    ## ----------------------------
    ## Initialization Stuff
    ## ----------------------------
    start_t <- Sys.time()
    output <- list()
    cat("\n=======================================", file = log.file, append = TRUE)
    cat("\n",date(), "\n", file = log.file, append = TRUE, sep = "")
    cat("\nChecking Data.......\n", file = log.file, append = TRUE)
    cat("\nFindZeroValues", ifelse(find.zero.values, "            Enabled", "            Disabled"),
        file = log.file, append = TRUE, sep = "")
    cat("\nFindNAs", ifelse(find.NAs, "                   Enabled", "                   Disabled"),
        file = log.file, append = TRUE, sep = "")
    cat("\nFindExtremeTrueRange", ifelse(find.extreme.truerange, "      Enabled", "      Disabled"),
        file = log.file, append = TRUE, sep = "")
    cat("\nFindOutsideBBand", ifelse(find.outside.bband, "          Enabled", "          Disabled"),
        file = log.file, append = TRUE, sep = "")
#     cat("\nFindIntradayPriceJump", ifelse(find.intraday.pricejump, "     Enabled", "     Disabled"),
#         file = log.file, append = TRUE, sep = "")
    cat("\nFindIntradayGaps", ifelse(find.intraday.gaps, "          Enabled", "          Disabled"),
        file = log.file, append = TRUE, sep = "")
    cat("\nCalcRatioExpectedMinutes", ifelse(calc.ratio.exp.minutes, "  Enabled", "  Disabled"),
        file = log.file, append = TRUE, sep = "")

    ## ----------------------------
    ## Call subfunctions
    ## ----------------------------
    for(t in my.tickers){

        cat("\n\nStarting Check For ", t, "\n", file = log.file, append = TRUE, sep = "")
        product.timer.start <- Sys.time()

        ## get data and change to minutes if tick data
        x <- get(t)
        if (dim(x)[2] == 2) x <- to.minutes(x)

        ## intialize output values
        zero.values    <- NULL
        nas            <- NULL
        extreme.ranges <- NULL
        outside.bb     <- NULL
        price.jumps    <- NULL
        gaps           <- NULL
        exp.minutes    <- NULL
        ## run sub-functions
        if(find.zero.values) {
            timer.start <- Sys.time()
            zero.values <- FindZeroValues(x)
            name <- paste(t, "zero.values", sep = "_")
            output[[length(output) + 1]] <- zero.values
            names(output)[length(output)] <- name
            timer.end<-Sys.time()
            if (function.timer) {
                cat("\nRunTime for FindZeroValues: ", (timer.end - timer.start),
                file = log.file, append = TRUE)
            }
        }
        if(find.NAs) {
            timer.start <- Sys.time()
            nas <- FindNAs(x)
            name <- paste(t, "NAs", sep = "_")
            output[[length(output) + 1]] <- nas
            names(output)[length(output)] <- name
            timer.end<-Sys.time()
            if (function.timer) {
                cat("\nRunTime for FindNAs: ", (timer.end - timer.start),
                   file = log.file, append = TRUE)
            }
        }
        if(find.extreme.truerange) {
            timer.start <- Sys.time()
            extreme.ranges <- FindExtremeTrueRange(x, atr.multiplier)
            name <- paste(t, "extreme.ranges", sep = "_")
            output[[length(output) + 1]] <- extreme.ranges
            names(output)[length(output)] <- name
            timer.end<-Sys.time()
            if (function.timer) {
                cat("\nRunTime for FindExtremeTrueRange: ", (timer.end - timer.start),
                   file = log.file, append = TRUE)
            }
        }
        if(find.outside.bband) {
            timer.start <- Sys.time()
            outside.bb <- FindOutsideBBand(x, sd.multiplier)
            name <- paste(t, "outside.bb", sep = "_")
            output[[length(output) + 1]] <- outside.bb
            names(output)[length(output)] <- name
            timer.end<-Sys.time()
            if (function.timer) {
                cat("\nRunTime for FindOutsideBBand: ", (timer.end - timer.start),
                   file = log.file, append = TRUE)
            }
        }
#         if(find.intraday.pricejump) {
#             timer.start <- Sys.time()
#             price.jumps <- FindIntradayPriceJump(x, pct.jump.threshold)
#             name <- paste(t, "intraday.price.jumps", sep = "_")
#             output[[length(output) + 1]] <- price.jumps
#             names(output)[length(output)] <- name
#             timer.end<-Sys.time()
#             if (function.timer) {
#                 cat("\nRunTime for FindIntradayPriceJump: ", (timer.end - timer.start),
#                    file = log.file, append = TRUE)
#             }
#         }
        if(find.intraday.gaps) {
            timer.start <- Sys.time()
            gaps <- FindIntradayGaps(x, min.gap.length)
            name <- paste(t, "intraday.gaps", sep = "_")
            output[[length(output) + 1]] <- gaps
            names(output)[length(output)] <- name
            timer.end<-Sys.time()
            if (function.timer) {
                cat("\nRunTime for FindIntradayGaps: ", (timer.end - timer.start),
                   file = log.file, append = TRUE)
            }
        }
        if(calc.ratio.exp.minutes) {
          timer.start <- Sys.time()
          exp.minutes <- CalcRatioExpectedMinutes(x, filter.threshold)
          name <- paste(t, "intraday.exp.minutes", sep = "_")
          output[[length(output) + 1]] <- exp.minutes
          names(output)[length(output)] <- name
          timer.end<-Sys.time()
          if (function.timer) {
            cat("\nRunTime for CalcRatioExpectedMinutes: ", (timer.end - timer.start),
                file = log.file, append = TRUE)
          }
        }

        ## ----------------------------
        ## Summary Output for t
        ## ----------------------------

        cat("\n\n---------------------------------------", file = log.file, append = TRUE)
        cat("\nSummary For", t, "\n", file = log.file, append = TRUE)
        cat("\nZero-Values               ", ifelse(find.zero.values, nrow(zero.values), "NA"), file = log.file, append = TRUE)
        cat("\nNAs                       ", ifelse(find.NAs, nrow(nas), "NA"), file = log.file, append = TRUE)
        cat("\nExtreme Ranges            ", ifelse(find.extreme.truerange, nrow(extreme.ranges), "NA"), file = log.file, append = TRUE)
        cat("\nOutside BBands            ", ifelse(find.outside.bband, nrow(outside.bb), "NA"), file = log.file, append = TRUE)
        #cat("\nIntraday Price Jumps      ", ifelse(find.intraday.pricejump, nrow(price.jumps), "NA"), file = log.file, append = TRUE)
        cat("\nDays With Intraday Gaps   ", ifelse(find.intraday.gaps, nrow(gaps), "NA"), file = log.file, append = TRUE)
        cat("\nRatio of Expected Minutes ", ifelse(calc.ratio.exp.minutes, nrow(exp.minutes), "NA"), file = log.file, append = TRUE)
        cat("\n", file = log.file, append = TRUE)


        if(verbose){
            ## ----------------------------
            ## Detailed Output for t
            ## ----------------------------
            ## Output Bad Data timestamps
            cat("\n---------------------------------------", file = log.file, append = TRUE)
            cat("\nBad Data For", t, "\n", file = log.file, append = TRUE)

            # For Console, use print()
            # For File, use write.zoo()

            ## Zero Values
            if (!is.null(zero.values) && nrow(zero.values) > max.output){
                cat("\nZero-Values:  Too Many To Print\n", file = log.file, append = TRUE)
                #cat("\n******************", file = log.file, append = TRUE)
            } else if (find.zero.values && nrow(zero.values) <= max.output) {
                cat("\nZero-Values for", t, "\n", file = log.file, append = TRUE)
                if (log.file == "") {
                    print(zero.values)
                } else {
                    write.zoo(zero.values, col.names = FALSE, sep = ";\t\t\t",
                              file = log.file, append = TRUE)
                }
                #cat("\n******************", file = log.file, append = TRUE)
            }
            ## NAs
            if (!is.null(nas) && nrow(nas) > max.output){
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nNAs:  Too Many To Print\n", file = log.file, append = TRUE)
            } else if (find.NAs && nrow(nas) <= max.output) {
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nNAs for", t, "\n", file = log.file, append = TRUE)
                if (log.file == "") {
                    print(nas)
                } else {
                    write.zoo(nas, col.names = FALSE, sep = ";\t\t\t",
                              file = log.file, append = TRUE)
                }
            }
            ## Extreme True Range
            if (!is.null(extreme.ranges) && nrow(extreme.ranges) > max.output){
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nExtreme Ranges:  Too Many To Print\n", file = log.file, append = TRUE)
            } else if (find.extreme.truerange && nrow(extreme.ranges) <= max.output) {
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nExtreme Ranges for", t, file = log.file, append = TRUE)
                cat("\nUsing atr.multiplier =", atr.multiplier, "\n\n", file = log.file, append = TRUE)
                if (log.file == "") {
                    print(extreme.ranges)
                } else {
                    write.zoo(extreme.ranges, col.names = FALSE, sep = ";\t\t\t",
                              file = log.file, append = TRUE)
                }
            }
            ## Outside Bollinger Bands## Extreme True Range
            if (!is.null(outside.bb) && nrow(outside.bb) > max.output){
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nOutside BBands:  Too Many To Print\n", file = log.file, append = TRUE)
            } else if (find.outside.bband && nrow(outside.bb) <= max.output) {
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nOutside BBands for", t, file = log.file, append = TRUE)
                cat("\nUsing sd.multiplier =", sd.multiplier, "\n\n", file = log.file, append = TRUE)
                if (log.file == "") {
                    print(outside.bb)
                } else {
                    write.zoo(outside.bb, col.names = FALSE, sep = ";\t\t\t",
                              file = log.file, append = TRUE)
                }
            }
            ## Intraday Price Jumps
            if (!is.null(price.jumps) && nrow(price.jumps) > max.output){
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nIntraday Price Jumps:  Too Many To Print\n", file = log.file, append = TRUE)
            } else if (find.intraday.pricejump && nrow(price.jumps) <= max.output) {
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nIntraday Price Jumps for", t, file = log.file, append = TRUE)
                cat("\nUsing pct.jump.threshold =", pct.jump.threshold, "\n\n", file = log.file, append = TRUE)
                if (log.file == "") {
                    print(price.jumps)
                } else {
                    write.zoo(price.jumps, col.names = FALSE, sep = ";\t\t\t",
                              file = log.file, append = TRUE)
                }
            }
            ## Intraday Gaps
            if (!is.null(gaps) && nrow(gaps) > max.output){
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nIntraday Gaps:  Too Many To Print\n", file = log.file, append = TRUE)
            } else if (find.intraday.gaps && nrow(gaps) <= max.output) {
                cat("\n******************", file = log.file, append = TRUE)
                cat("\nIntraday Gaps for", t, file = log.file, append = TRUE)
                cat("\nUsing min.gap.length =", min.gap.length, "\n\n", file = log.file, append = TRUE)
                if (log.file == "") {
                    cat(gaps, sep = "\n")
                } else {
                    write.zoo(gaps, col.names = FALSE, sep = ";\t\t\t",
                               file = log.file, append = TRUE)
                }
            }
            ## Intraday Ratio of Observed to Expected Minutes
            if (!is.null(exp.minutes) && nrow(exp.minutes) > max.output){
              cat("\n******************", file = log.file, append = TRUE)
              cat("\nRatio of Expected Minutes:  Too Many To Print\n", file = log.file, append = TRUE)
            } else if (calc.ratio.exp.minutes && nrow(exp.minutes) <= max.output) {
              cat("\n******************", file = log.file, append = TRUE)
              cat("\nRatio of Expected Minutes for", t, file = log.file, append = TRUE)
              cat("\nUsing filter.threshold =", filter.threshold, "\n\n", file = log.file, append = TRUE)
              if (log.file == "") {
                #cat(exp.minutes, sep = "\n")  #doesn't display correctly...
                print(exp.minutes)
              } else {
                write.zoo(exp.minutes, col.names = FALSE, sep = ";\t\t\t",
                          file = log.file, append = TRUE)
              }
            }

            # End Output
            ## -------------------------
        }
        product.timer.end<-Sys.time()
        if (function.timer) {
            cat("\n\nRunTime For Product:", t, (product.timer.end - product.timer.start),
               file = log.file, append = TRUE)
        }
        cat("\n", file = log.file, append = TRUE)
        ## Separator for each t
        cat("\n=======================================", file = log.file, append = TRUE)
    }
    ## Run after for-loop is complete
    end_t<-Sys.time()
    cat("\n\nRunTime For All Products: ", (end_t-start_t), file = log.file, append = TRUE)
    cat("\n\n=======================================", file = log.file, append = TRUE)
    cat("\n\nCheckData Complete\n", file = log.file, append = TRUE)
    return(output)
}


# RenameNTFiles -----------------------------------------------------------
RenameNTFiles <- function(txtToNT=FALSE, dailyMinuteOrTick,
                          dir = paste0(dir.data, "RawFiles/")) {
    ## renaming function for NT files
    ## date 2015-02-26
    ## set dailyMinuteOrTick
    if(txtToNT){
        myFiles <- list.files(dir, pattern = ".txt")
        for(file in myFiles){
            x <- unlist(strsplit(file, split = "[.]"))
            oldName <- paste0(dir, file)
            newName <- paste0(dir, x[1], ".", x[2], ".NT" )
            file.rename(from = oldName, to = newName)
        }
    }

    ## renames anything like ES.Last.NT to minute.ES.NT
    if(dailyMinuteOrTick == "minute" || dailyMinuteOrTick == "daily"){
        myFiles <- list.files(dir, pattern = ".NT")
        for(file in myFiles){
            x <- unlist(strsplit(file, split = "[.]"))
            oldName <- paste0(dir, file)
            newName <- paste0(dir, dailyMinuteOrTick, "." ,x[1], ".NT" )
            file.rename(from = oldName, to = newName)
        }
    }

    ## renames tick files (.Ask.NT, .Bid.NT, .Last.NT) to (.askTick.NT, etc)
    ##  so can separate "last" Minute files from "last" Tick files
    if(dailyMinuteOrTick == "tick"){
        myFiles <- list.files(dir, pattern = ".NT")
        for(file in myFiles){
            x <- unlist(strsplit(file, split = "[.]"))
            oldName <- paste0(dir, file)
            newName <- paste0(dir, "tick", x[2], "." ,x[1], ".NT" )
            file.rename(from = oldName, to = newName)
        }
    }
}


# LoadNTData --------------------------------------------------------------

LoadNTData <- function(inputFile, preprocessingDir) {
    # takes in symbol names, directory, loads objects into memory
    # date:  2014-11-20

    # Args:
    #       inputFile:  a character vector containing names of files
    #            in pre-processing directory
    #       preprocessingDir:  directory with *.NT files

    # Returns:
    #       a set of a XTS objects loaded to memory
    #       should return a list of objects?

    # TEST VALUES
    #       inputFile <- c("Test1.NT", "Test2.NT")
    #       preprocessingDir <- "./Data/RawFiles/"

    # for the files in the folder, read, format, assign the name
    start_t<-Sys.time()
    for(s in seq(inputFile)) {

        ## set column names
        name <- unlist(strsplit(inputFile[s], split = "[.]"))[1]
        if(any(name==c("tickAsk", "tickBid", "tickLast"))){
            colNames <- c("TimeStamp", "Price", "Size" )
        } else {
            colNames <- c("TimeStamp", "Open", "High",
                          "Low", "Close", "Volume")
        }

        # read the file, assign contents to x
        x <- read.table(paste(preprocessingDir, inputFile[s], sep = "" ),
                        #nrows = 50,  # comment out to grab everything!
                        header = FALSE,
                        sep=";",
                        row.names=NULL,
                        col.names = colNames,
                        comment.char = "",
                        stringsAsFactors = FALSE)

        # convert TimeStamp column to POSIXct
        x$TimeStamp <- as.POSIXct(x = x$TimeStamp, tz = "UTC",
                                  "%Y%m%d %H%M%S")

        # convert to xts by subtracting TS column, using POSIX TS as index
        x <- xts(x[,-1], x$TimeStamp)

        # rename x
        # assign(inputFile[s], x)
        return(x)
    }
    end_t<-Sys.time()
    cat("\n\nRunTime: ", (end_t-start_t))

}



# SubsetDataandSaveRdataFiles ---------------------------------------------
SubsetDataAndSaveRdataFiles <- function(symbol, writeWhat = "all"){
        # subsets XTS objects into training and test sets, writes to .Rdata files
        # date:  2014-11-19

        # Args:
        #       symbol:  a character vector containing names of xts objects
        #       writeWhat: either full, training, testA, testB, or all
        #               (writes all 4 data files)
        #
        # Returns:
        #       a set of .Rdata files in the corresponding folders

        # TEST VALUES
        #       symbol <- c("IBM", "GS")
        #       writeWhat <- "full"
        #       periodicity <- "daily"
        #

        require(quantstrat)
        output <- ".Rdata"

        # Test write what for garbage input
        if (writeWhat %in% c("full", "all", "training", "testA",
                             "testB") == FALSE) {
                 stop("ERROR: writeWhat needs to be either full, all,
                         training, testA, or testB")

        }

        for (s in symbol) {
                # calculate the begin and end dates of each data set
                x <- get(s)
                full.StartDate <- as.Date(index(x[1,1]))
                full.EndDate <- as.Date(index(x[nrow(x),1]))
                train.StartDate <- full.StartDate
                # 60 percent of the data set
                train.EndDate <- as.Date(index(x[trunc(.60*nrow(x))]))
                # start testA the day after training ends
                testA.StartDate <- train.EndDate + 1
                # next 20% of data
                testA.EndDate <- as.Date(index(x[trunc(.80*nrow(x))]))
                # start testB the day after testA ends
                testB.StartDate <- testA.EndDate + 1
                testB.EndDate <- full.EndDate

                # write full dataset to csv
                if (writeWhat == "full" || writeWhat == "all") {
                        myDirectory <- paste0(dir.data, "StrategyTesting/1_Full/")
                        newName <- paste(s, ".",
                                         full.StartDate, ".",
                                         "TO", ".",
                                         full.EndDate,
                                         sep = "")

                        # assigns new ptr to object x
                        # no need to subset for this one
                        assign(newName, x)
                        save(list = newName, file = paste(sep = "",
                                                          myDirectory,
                                                          newName,
                                                          output))
                }
                # subset training set, write to csv
                if (writeWhat == "training" || writeWhat == "all") {
                        myDirectory <- paste0(dir.data, "StrategyTesting/2_TrainingData/")
                        newName <- paste(s, ".",
                                         train.StartDate, ".",
                                         "TO", ".",
                                         train.EndDate,
                                         sep = "")

                        x.subset <- x[paste(train.StartDate, "/",
                                            train.EndDate, sep = "")]
                        assign(newName, x.subset)
                        save(list = newName, file = paste(sep = "",
                                                          myDirectory,
                                                          newName,
                                                          output))
                }
                # subset x to a new object, write the csv or rdata file from the new obj
                if (writeWhat == "testA" || writeWhat == "all") {
                        myDirectory <- paste0(dir.data, "StrategyTesting/3_TestSetA/")
                        newName <- paste(s, ".",
                                         testA.StartDate, ".",
                                         "TO", ".",
                                         testA.EndDate,
                                         sep = "")
                        x.subset <- x[paste(testA.StartDate, "/",
                                            testA.EndDate, sep = "")]
                        assign(newName, x.subset)
                        save(list = newName, file = paste(sep = "",
                                                          myDirectory,
                                                          newName,
                                                          output))
                }
                # subset x to a new object, write the csv or rdata file from the new obj
                if (writeWhat == "testB" || writeWhat == "all") {
                        myDirectory <- paste0(dir.data, "StrategyTesting/4_TestSetB/")
                        newName <- paste(s, ".",
                                         testB.StartDate, ".",
                                         "TO", ".",
                                         testB.EndDate,
                                         sep = "")
                        x.subset <- x[paste(testB.StartDate, "/",
                                            testB.EndDate, sep = "")]
                        assign(newName, x.subset)
                        save(list = newName, file = paste(sep = "",
                                                          myDirectory,
                                                          newName,
                                                          output))
                }
        }
}



# LoadITGData --------------------------------------------------

LoadITGData <- function(inputFile, preprocessingDir = paste0(dir.data, "RawFiles/")) {
    # Summary:
    #       take in ITG prices file name, directory, and whether the output is
    #       going to be trades or bid or ask prices, loads into memory
    #
    # Args:
    #       inputFile:  file name must be in the format of "Product.Date.prices"
    #           ex:  ESZ4.2014-11-10.prices
    #       preprocessingDir:  directory where ITG files are stored

    #
    # Returns:
    #       an xts objects with either Trades, BidPrices, or Ask Prices:
    #

    # Test Values:
    #       inputFile <- c("XXZ4.2014-11-10.prices")
    #       preprocessingDir <- "./Data/RawFiles/"
    #       preprocessingDir <- "./Data/RawFiles/UnitTests/"
    #       priceType <- "Trades"

    require(quantstrat)
    # Test for garbage inputs
        if (!file.exists(paste(preprocessingDir, inputFile, sep = ""))) {
           stop("inputFile does not exist")
        }

    # Load the data
        x <- read.table(file = paste(preprocessingDir, inputFile, sep = "" ),
                        #nrows = 50,  # comment out to grab everything!
                        header = FALSE,
                        sep = " ",
                        row.names = NULL,
                        fill = TRUE,  # to handle the different length of trade-rows vs quote-rows
                        #col.names = myColumnNames,
                        comment.char = "",
                        stringsAsFactors = FALSE
        )
}


# FilterITGData --------------------------------------------------
FilterITGData <- function(x, productDate, priceType = "Trades") {
    # Summary:
    #       take in a pre-loaded ITG prices file, directory, and output either
    #       a trades, bidprices, or ask prices object
    #
    # Args:
    #       x:  object name
    #       productDate: used to add on top of the time to make a complete
    #           timestamp
    #       priceType:  either Trades, BidPrices, AskPrices
    #
    # Returns:
    #       an xts objects filtered either by Trades, BidPrices, or Ask Prices.
    #
    # Test Values:
    #       x <- LoadITGData(inputFile = "XXZ4.2014-11-10.prices" )
    #       x <- fullDataSet
    #       priceType <- "Trades"
    # -------------------------------------------------------------------
    # Test for garbage inputs
    require(quantstrat)
    if (priceType %in% c("Trades", "BidPrices", "AskPrices") == FALSE) {
        stop("priceType needs to be Trades, BidPrices, or AskPrices")
    }
    if (ncol(x) != 9) {  # sometimes the read.table function creates 10 columns?
      browser()
      stop("ERROR:  There are", ncol(x), "columns in this data frame"  )
    }

    # Set appropriate params for trades, bids or ask prices
        # Column names must be different for trades or quotes
        ifelse(priceType == "Trades",
               myColumnNames <- c("TimeStamp", "Product", "Exchange",
                                  "Trade_or_Quote", "TradePrice", "TradeQty"),
#                myColumnNames <- c("TimeStamp", "Product", "Exchange",
#                                   "Trade_or_Quote", "C6", "C7", "TradePrice",
#                                   "TradeVol", "TotalVol"),
               myColumnNames <- c("TimeStamp", "Product", "Exchange",
                                  "Trade_or_Quote", "Levels", "BidPrice",
                                  "BidQty", "AskPrice", "AskQty"))
        # The columns that hold Trade and Volume need to switch too
        switch(priceType,
               "Trades" = {priceColumn <- "TradePrice"
                           volumeColumn <- "TradeQty"
                           tradeOrQuote <- "T"},
               "BidPrices" = {priceColumn <- "BidPrice"
                              volumeColumn <- "BidQty"
                              tradeOrQuote <- "Q"},
               "AskPrices" = {priceColumn <- "AskPrice"
                              volumeColumn <- "AskQty"
                              tradeOrQuote <- "Q"})

    # Set column names
        colnames(x) <- myColumnNames

    # Filter the data table
        output <- subset(x, x$Trade_or_Quote == tradeOrQuote)

    # Convert timestamp to POSIX, then to XTS
        # productName <- substr(x, start = 1, stop = 2)
        # productDate <- substr(x, start = 6, stop = 15)
        output$TimeStamp  <- paste(productDate, output$TimeStamp)
        output$TimeStamp  <- as.POSIXct(x = output$TimeStamp,
                                       tz = "America/Chicago",
                                       "%Y-%m-%d %H:%M:%OS" )
        output <- xts(output[,-1], output$TimeStamp)

    # Subset the price and volume columns, bind to timestamp
        # take out either TradePrice, bid, ask column from output
        tmpPriceColumn <- output[,priceColumn]
        # to convert TradeVol column to numeric, need to column bind to numeric data
        output <- cbind(tmpPriceColumn, as.numeric(output[,volumeColumn]))
        # call price column "Close" to match the convention in the other data sets
        colnames(output) <- c("Close","Volume")

        return(output)

    # NOTE:
    # had this code in here before, but i don't think i need it.
    # if the output is bidprices or ask prices, it removes the qty
    # but i think i want this in here to show the qty at either the
    # best bid or ask.  plus, it makes it sync up with the other price series
    # naming conventions
    # remove quantity column if this is a Quote series
    # if(tradeOrQuote == "Q") x <- x[,1]
    #return(x)

}



# CombineITGData --------------------------------------------------

CombineITGData <- function(symbol, priceType, startDate, endDate ) {

    # Summary:
    #       takes in objects named like tick.ES.trades.2014-11-10,
    #       binds them together so it can be fed to the subset and
    #       save functions

    # Args:
    #       symbol:  product name, like ES
    #       priceType: either Trades, BidPrice, or AskPrice
    #       startDate:  the first date in the series
    #       endDate:  the last date in the series

    # Returns:
    #       1 xts object:
    #           tick.ProductName.StartDate.TO.EndDate.Trades
    #

#     FROM STACK EXCHANGE:
#         resources:
#         # Get the files names
#         files = list.files(pattern="*.csv")
#     # from stack exchange, not sure if this works
#     myfiles = do.call("rbind", lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
#

}
# UNIT TEST:
    # run the loadITGdata function to create a few days' worth of objects
    # bind a few objects together
    # check the final object for matching values



# SaveToCSV --------------------------------------------------

SaveToCSV <- function(x, startDate, endDate, periodicity = "daily",
                      outputDirectory = paste0(dir.data, "StrategyTesting/1_Full/")) {
    # Wrapper function to saves an XTS object to CSV file
    # date:  2014-12-01

    # Args:
    #       x: name of an XTS object to be written, like "IBM"
    #       startDate: beginning in data, used in filename
    #       endDate:  end of data, used in filename
    #       periodicity: either daily, minute, tickNT, or tickITG.
    #                    just adds the description to front of
    #                    output filename
    #       outputDirectory: directory to write the files

    # Returns:
    #       a .csv file written to the outputDirectory
    #       in the format: periodicity.productName.startDate.TO.endDate.output

    # TEST VALUES
    #       getSymbols("IBM")
    #       x <- "IBM"
    #       outputDirectory = "./Data/StrategyTesting/1_Full/"
    #       periodicity <- "daily"
    #       startDate <- "2007-01-03"
    #       endDate <- "2014-11-28"

    # Initialize
        require(quantstrat)
        output <- ".csv"

    # Test for garbage inputs
        if (periodicity %in% c("daily", "minute", "tickNT", "tickITG") == FALSE) {
            stop ("periodicity should be daily, minute, tickNT or tickITG")
        }

    # create output file name
        newName <- paste(periodicity, x, startDate, "TO",
                         endDate, sep = ".")
        assign(newName, get(x)) #assigns newName to the data "x" points to

    # write dataset to file
        write.zoo(get(newName), file = paste(outputDirectory, newName,
                                             output, sep = ""))
}


# SaveToRData --------------------------------------------------
SaveToRdata <- function(x, startDate, endDate,
                        #periodicity = "daily",
                        outputDirectory = paste0(dir.data, "StrategyTesting/1_Full/")) {
    # Wrapper function to saves an XTS object to RData file
    # date:  2014-12-01

    # Args:
    #       x: name of an XTS object to be written, like "IBM"
    #       startDate: beginning in data, used in filename
    #       endDate:  end of data, used in filename
    #       periodicity: either daily, minute, tickNT, or tickITG.
    #                    just adds the description to front of
    #                    output filename
    #       outputDirectory: directory to write the files

    # Returns:
    #       an .Rdata file written to the outputDirectory
    #       in the format: periodicity.productName.startDate.TO.endDate.output

    # TEST VALUES
    #       getSymbols("IBM")
    #       x <- "IBM"
    #       outputDirectory = "./Data/StrategyTesting/1_Full/"
    #       periodicity <- "daily"
    #       startDate <- "2007-01-03"
    #       endDate <- "2014-11-28"
    #

    # Initialize
        require(quantstrat)
        output <- ".Rdata"

    # Test for garbage inputs
        #if (periodicity %in% c("daily", "minute", "tickNT", "tickITG") == FALSE) {
        #    stop ("periodicity should be daily, minute, tickNT or tickITG")
        #}

    # create output file name

        #newName <- paste(periodicity, x, startDate, "TO",
        #                 endDate, sep = ".")
        newName <- paste(x, startDate, "TO",
                     endDate, sep = ".")

        assign(newName, get(x)) #assigns newName to the data "x" points to

    # write dataset to file
        save(list = newName, file = paste0(outputDirectory, newName, output))
}

CalculateSQN <- function(opt.results) {
  # take in results list from quantstrat optimization
  #
  # Args
  #   opt.results     list produced by quantstrat optimization
  #
  # Returns
  #   vector of SQN values
  # -----------------------------------------------
  if(!is.list(opt.results)) opt.results <- get(opt.results)
  means  <- opt.results$tradeStats$Avg.Trade.PL
  sd     <- opt.results$tradeStats$Std.Dev.Trade.PL
  n      <- opt.results$tradeStats$Num.Trades
  output  <-  sqrt(n) * means / sd
  return(output)
}


# T.indicator -------------------------------------------------------------
    # Creates a TA indicator that holds N-period forward returns
    # use this to asses the value of an entry signal

    # date:  2014-12-26

    # Args:
    #       quotes:  a OHLC XTS object
    #       tgt.margin=0.025:  a minimum threshold, used to determine
    #           whether to include that period's return in the summation.
    #           if the value is close enough to 0, doesn't consider it
    #       n.days=10:  the number of periods to sum when calculating
    #           forward returns

    # Returns:
    #       either an XTS object or a generic timeseries, sorted by timestamp

    require(quantmod)
    T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
      # v is a vector of the average HLC values
      v <- apply(HLC(quotes),1,mean)

      # initializes a matrix r
      #   fills the matrix with NA values
      #   creates k columns
      #   creates the same number of rows as the quotes object
      r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))

      # for x = 1 to 10:
      #     calculate returns from close to AvgPrice
      #       for days 1, 2, ... n.days
      #     then lag the series by 1
      #     stuff the value into the x column of the r matrix
      #     default is a 10-column matrix, so calculate 10 returns
      for(x in 1:n.days) r[,x] <- Next(Delt(Cl(quotes),v,k=x),x)

      # sums the returns of each row if above/below the threshold
      #     apply - applies the function "sum" to matrix r across the rows
      #         (to sum the columns, set to 2)
      x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))

      # if the input is an xts object, return an xts object, order by timestamp
      if (is.xts(quotes)) xts(x,time(quotes)) else x
    }

