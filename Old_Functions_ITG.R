###########################################################################

## Functions For Processing ITG Backtester Data

###########################################################################

## Set some environment variables
# install.packages("blotter", repos="http://R-Forge.R-project.org")
# install.packages("quantstrat", repos="http://R-Forge.R-project.org")
# require(devtools); install_github("joshuaulrich/xts")
require(quantstrat)
require(xts)
require(PerformanceAnalytics)
require(ggplot2)
#require(xlsx) # can't run it in script mode with this package, for some reason
Sys.setenv(TZ = "America/Chicago")
options(digits.secs = 6)  # shouldn't need to change this often
colorset <- PerformanceAnalytics::rainbow6equal #rich6equal #dark6equal #tim6equal #grey6mono # tim12equal #rich12equal  #rainbow12equal
## update R
# install.packages("installr")
# require(installr)
# updateR(F, T, T, F, T, T, T)


#  ------------------------------------------------------------------------
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

#  ------------------------------------------------------------------------
CreateProjectFolders <- function(project.name, my.path, folder.charts, folder.csvfiles,
                                 folder.processed, folder.rdata, folder.logs) {
  # Creates folder structure for ITG projects
  #
  # Args      
  #  project.name         name of project, should be like "20151022.CL.Project"
  #
  # Returns 
  #   creates a bunch of new folders and sets some variables
  # -----------------------------------------------------------------------
  
  ## Create Folder Structure ##
  cat("\nProject Folder is", project.name)                   
  if (is.na(match(paste0(".//", project.name), list.dirs("./", recursive = F)))) {  
    cat("\nCreating Project Folder")
    dir.create(my.path)
  }
  if (is.na(match(folder.csvfiles, list.dirs(my.path, recursive = F)))) {
    cat("\nCreating CSV Folder")
    dir.create(folder.csvfiles)
  }
  if (is.na(match(folder.charts, list.dirs(my.path, recursive = F)))) {
    cat("\nCreating Charts Folder")
    dir.create(folder.charts)
  }
  if (is.na(match(folder.processed, list.dirs(my.path, recursive = F)))) {
    cat("\nCreating Processed Folder")
    dir.create(folder.processed)
  }
  if (is.na(match(folder.rdata, list.dirs(my.path, recursive = F)))) {
    cat("\nCreating Rdata Folder")
    dir.create(folder.rdata)
  }
  if (is.na(match(folder.logs, list.dirs(my.path, recursive = F)))) {
    cat("\nCreating Log Folder")
    dir.create(folder.logs)
  }
}




#  ------------------------------------------------------------------------
AddCommissions <- function(trades, commish.per.side, qty.column = 2, cm.pnl.column=5){
  ## adds commissions to each line of cumulative pnl based on a sum of qty and commish rate
  #
  # Args
  #   trades              XTS or dataframe of trades
  #   commish.per.side    positive number one-way commissions rate
  #   qty.column          the column in the dataframe that holds qty
  #   cm.pnl.column       the column in the dataframe that holds cumulative PNL
  # Returns
  #   the original dataframe but with commissions added
  # -----------------------------------------------------------------------

  ## Trap for errors ##
  if (!is.data.frame(trades) && !is.xts(trades)) stop("ERROR: trades object must be XTS or a dataframe")
  if (!cm.pnl.column %in% 1:dim(trades)[2])      stop("ERROR: cm.pnl.column does not exist in trades object")
  if (!qty.column %in% 1:dim(trades)[2])         stop("ERROR: qty.column does not exist in trades object")
  if (commish.per.side < 0)                      stop("ERROR: commish.per.side must be >= 0")

  ## make sure qty and cm.pnl columns are numeric
  if (!is.numeric(trades[, qty.column])) {
    trades[, qty.column] <- as.numeric(trades[, qty.column])
  }
  if (!is.numeric(trades[, cm.pnl.column])) {
    trades[, cm.pnl.column] <- as.numeric(trades[, cm.pnl.column])
  }

  ## adds qty.sum * commissions rate to each cumulative PNL total
  for (i in 1:nrow(trades)){
    if (i == 1){
      qty.sum <- trades[i, qty.column]
      trades[i, cm.pnl.column] <- trades[i, cm.pnl.column] + (qty.sum * -1 * commish.per.side)

    } else {
      qty.sum <- qty.sum + trades[i, qty.column]
      trades[i, cm.pnl.column] <- trades[i, cm.pnl.column] + (qty.sum * -1 * commish.per.side)
    }
  }
  return(trades)
}


#   -----------------------------------------------------------------------
TrimTrades <- function(trades, pnl.stop, cm.pnl.column=5) {
  # Trims a dataframe of trades based on a theoretical PNL stop
  #
  # Args
  #   trades          a dataframe of trades
  #   cm.pnl.column   the column that contains the cumulative PNL
  #   pnl.stop        the theoretical pnl stop, like: -200
  #
  # Returns
  #   a trimmed down dataframe, replaces the end PNL with pnl.stop if
  #   it would have been stopped out
  # --------------------------------------------------------------
  ## trap for errors ##
  if (!is.data.frame(trades) && !is.xts(trades)) stop("ERROR: trades object must be XTS or a dataframe")
  if (!cm.pnl.column %in% 1:dim(trades)[2])      stop("ERROR: cm.pnl.column does not exist in trades object")
  if (pnl.stop > 0)                              stop("ERROR: pnl.stop must be less than 0")

  ## TESTING
  # trades <- ProfitTicks1.Stop0$trades.20141120.block12
  # trades <- complete.cases(trades)
  # tail(unclass(trades))
  # tail(trades)

  ## find first row past the pnl.stop
  i <- 1
  while (trades[i,cm.pnl.column] > pnl.stop) {
    if (i == nrow(trades)) break  # at the end of the object
    i <- i + 1
  }

  ## subset and save
  output <- trades[1:i, ]
  if (output[i, cm.pnl.column] < pnl.stop) {
    output[i, cm.pnl.column] <- pnl.stop # set last PNL to the pnl.stop
  }
  return(output)
}


#   -----------------------------------------------------------------------
ProcessCsvFiles <- function(output.folders, commish.per.side=0, logfile="", ...) {
  # Process a group of folders with CSV data into a list
  #
  # Args
  #   output.folders    character vector of folder names containing CSV files
  #   stop.loss         stop amount to trim the trades files down to. 0 to disable
  #
  # Returns
  #   a list where each element is a dataframe of trades called "trades.date.blockNumber"
  # Update 6/29 removed PNL stop functionality
  #------------------------------------------------------------
  ## Testing
  #   output.folders  <- paramset.folder  
  #   folder    <- output.folders[1]
  #   my.files  <- list.files(folder)
  #   file      <- my.files[10]
  #   output.folders <- project.folder

  ## TODO:  preallocate the space for the output list

  ## Trap for errors
  if (commish.per.side < 0) stop("ERROR: commish.per.side must be >= 0")

  ## Create output list
  output <- list()
  element <- 1
  count.of.empty.files <- 0
  
  ## Processes each file in each output folder ##
  for (folder in output.folders) {
    # folder <- output.folders[1]
    
    ## Get a list of files in the folder
    my.files  <- list.files(folder)
    if (!length(my.files) > 0) stop("ERROR: no files in this folder")

    ## Process all of the files and save to the list
    for (file in my.files) {
      # file <- my.files[2]
      #suppressWarnings(rm(filename))
      # filename <- paste0(folder, file)  # changed this 2015-05-15 because of concatenation issues
      filename <- paste(folder, file, sep = "/")
      cat("\n\n-----------------------", file = logfile, append = TRUE)
      cat("\nProcessing: ", filename, file = logfile, append = TRUE)

      ## Load the file from the filename directory
      filename <- tryCatch(read.csv(filename, header = FALSE, sep = " ", dec = "."), 
                           error = function(c) "ERROR: can't read file")
      
      ## File should have at least 4 header rows
      if (is.null(nrow(filename))) {
        count.of.empty.files <- count.of.empty.files + 1  #warning (filename, "has no data")
        cat("\nFile", file, "has no data", "\nCount of empty files is ", count.of.empty.files, file = logfile, append = TRUE)
      } else {
        ## Remove Header rows
        header.rows <- c(1:4)
        filename <- filename[-c(header.rows), ]
        total.rows <- nrow(filename)

        ## Remove Footer Rows, can be 2,3,4 rows...
        while (any(is.na(filename[nrow(filename),]))) {
          last.row <- nrow(filename)
          filename <- filename[-last.row, ]
          #cat("\nRemoved Row Number", last.row, "/", total.rows)
        }
    
        ## omit any rows with any NA values, in case previous block
        ## did not remove everything
        filename <- filename[complete.cases(filename), ]
        #cat("\nRemoved Headers and Footers")
    
        ## don't need positions anymore, can get this later
        # try(position.output <- last(filename[, 9]))
        # try(cat("\nPosition Output is:", position.output))  
        if (dim(filename)[1] > 0) {
          ## convert character time to POSIX time
          ## and make each timestap unique by adding 2ms (1ms doesn't always work...)
          timestamps <- as.POSIXct(x = filename[,2], tz = "America/Chicago", "%Y%m%d_%H:%M:%OS" )
          trades <- as.data.frame(make.index.unique(xts(filename[,-2], order.by = timestamps), eps = .000002), stringsAsFactors = FALSE)
          trades <- trades[, c(2,3,4,6,8,1)]
          colnames(trades) <- c("side", "qty", "product", "price", "cm.pnl", "action")
          trades[, 5] <- as.numeric(trades[, 5])
          trades[, 2] <- as.numeric(trades[, 2])
  
          #head(trades)
          #cat("\nConverted Time And Set Column Headers ")
  
          ## Create objectname and assign
          ## name to PNL summary and trades.object
          block   <- substr(file, start = 8, stop = 14)
          year    <- substr(timestamps[1], start = 1, stop = 4)
          month   <- substr(timestamps[1], start = 6, stop = 7)
          day     <- substr(timestamps[1], start = 9, stop = 10)
          date    <- paste0(year, month, day)
          object.name <- paste(date, block, sep = ".")
  
          ## Add commissions if needed
          if (commish.per.side > 0) {
            trades <- AddCommissions(trades, commish.per.side)
          }
  
          ## Assign to list
          output[[element]] <- trades
          names(output)[[element]] <- paste0("trades.", object.name)
          element <- element + 1
  
          ## OLD CODE:  OUTPUT A SEPARATE OBJECT, DELETE IF UNNEEDED
          #end.pnl <- paste("end.pnl", object.name, sep = ".")
          #assign(paste0("trades.", object.name), trades)
          #try(assign(end.pnl, position.output))
  
          # rm(end.pnl, filename, output, position.output)
          cat("\nCompleted processing for", object.name, file = logfile, append = TRUE)
        } else {
          count.of.empty.files <- count.of.empty.files + 1  #warning (filename, "has no data")
          cat("\nFile", file, "has no data", "\nCount of empty files is ", count.of.empty.files, file = logfile, append = TRUE)
        }
      }
    }
  }
  cat("\n\n=============================", file = logfile, append = TRUE)
  cat("\nProcessing Complete", file = logfile, append = TRUE)
  cat("\nCount of empty files = ", count.of.empty.files, file = logfile, append = TRUE)
  return(output)
}


#   -----------------------------------------------------------------------
OutputEndingPNL <- function(trades.list, cm.pnl.column=5, filename=NULL) {
  ## take in a ParamSet List and find the ending PNL, then output a dataframe of ending PNLs
  #
  # Args
  #  trades.list      List representing a complete parameter set where each element contains all the trades for a date.block
  #  cm.pnl.column    In each element, this is the column that contains the cumulative PNL column
  #  filename         if not null, writes a csv file to this location
  #
  # Returns
  #  a dataframe of the ending PNLs from each element in the list
  #  also writes a CSV file if the filename is supplied
  #
  # Testing
  # trades.list <- all.paramsets[1]
  # trades.list <- name.paramset
  # name.paramset
  # -----------------------------------------------------------------------
  if (!is.list(trades.list)) trades.list <- get(trades.list)
  all.names <- names(trades.list)
  stopifnot(grepl("block", all.names[1]))  # needs to be "block" in the name for this to work correctly
  
  ## grab a unique set of dates from the list element names
  dates <- unique(substr(all.names, start = 8, stop = 15))  # better
  dates <- dates[order(dates)]  # order dates sequentially
  
  ## figure out how many time blocks are included
  timeblocks <- unlist(strsplit(all.names, "[.]"))
  timeblocks <- unique(timeblocks[grep(pattern = "block", timeblocks)])
  timeblocks <- timeblocks[order(timeblocks)]
  
  ## find dimensions of the output dataframe and create  
  output <- as.data.frame(matrix(data = NA, ncol = length(timeblocks), 
                                 nrow = length(dates),
                                 dimnames = list(dates, timeblocks)))



  ## Find the list element, output in the corresponding row and column of the dataframe
  for (timeblock in timeblocks) {
    # timeblock <- timeblocks[1]
    for (date in dates){
      # date <- dates[1]
      name <- paste("trades", date, timeblock, sep = ".")
      last.row <- nrow(trades.list[[name]])
      data <- trades.list[[name]][last.row, cm.pnl.column]
      if (length(data) > 0) try(output[date, timeblock] <- data)
    }
  }
  ## Write CSV file if needed
  if (!is.null(filename)){
    write.csv(output, filename)
  }
  return(output)
}


#   -----------------------------------------------------------------------
MakeHeatMapTable <- function(all.tables, summary="means"){
  # takes in a group of summary tables, takes the column means, medians, or sds
  # and binds them together into a dataframe
  #
  # Args
  #   all.tables    character vector of summary end.pnl tables
  #   summary       how to summarize the summary end.pnl tables(mean, median, sd)
  #
  # Returns
  #   a dataframe of either medians, means, sds
  #   rows are paramsets, columns are time blocks
  # -----------------------------------------------------------------
  
  ## trap for errors
  if(!summary %in% c("means", "medians", "sd")){
    stop("ERROR: summary needs to be: means, medians, or sd")
  }
  
  ## preallocate output frame
  output <- data.frame(stringsAsFactors=FALSE,
                       block01=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block02=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block03=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block04=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block05=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block06=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block07=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block08=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block09=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block10=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block11=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block12=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block13=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block14=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block15=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block16=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block17=rep(NA, length(all.tables)), #numeric(length(dates)),
                       block18=rep(NA, length(all.tables))  #numeric(length(dates))
  )

  ## summarize input tables and write to output table
  for(i in seq(all.tables)) {
    x <- get(all.tables[i])
    if (summary == "means")   row <- apply(x, 2, function(col) mean(x=col, na.rm = TRUE))
    if (summary == "medians") row <- apply(x, 2, function(col) median(x=col, na.rm = TRUE))
    if (summary == "sd")      row <- apply(x, 2, function(col) sd(x=col,     na.rm = TRUE))

    ## place output into row and name row
    output[i,] <- row
    
    ## better to rename these row labels later than mis-label them...
    #row.names(output)[i] <- row.labels[i]
    row.names(output)[i] <- all.tables[i]
    
  }
  #colnames(output) <- col.labels
  return(output)
}


#   -----------------------------------------------------------------------
MakeTimeBlockLabels <- function(start.time=6, end.time=15){
  h <- start.time:end.time
  m <- rep(c(15, 45), length(h))
  a <- paste(h, 15, sep = ":")
  b <- paste(h, 45, sep = ":")
  i <- 1
  k <- 1
  output <- NULL
  for(i in seq(a)){
    output[k] <- a[i]
    k <- k+1
    output[k] <- b[i]
    k <- k+1
  }
  output <- output[-length(output)]
  output <- output[-length(output)]
  return(output)
}


#   -----------------------------------------------------------------------
BindRowsAndRecalcPNL <- function(list.name, element.names) {
  # Take a list of trades and bind some together into a unified cumulative PNL
  # 
  # Args
  #   list.name          name of the list to draw from or the object
  #   element.names      ordered vector of element names to use to subset the list
  #
  # Returns
  #   an xts object with timestamp and re-calculated cumulative PNL
  # ------------------------------------------------------------------
  ## Added to either pass in the list object or the list name
  if (is.character(list.name)) {
    my.list <- get(list.name)
  } else my.list <- list.name
  stopifnot(is.list(my.list))
  stopifnot(is.character(element.names) || length(element.names) > 0) 
  
  ## TESTING
  # element.names     <- names(get(list.name))
  # i <- 1
  ## if the first date, just convert to XTS, otherwise add the previous end PNL and then convert to XTS
  for (i in seq(element.names)) {
    my.trades <- my.list[[element.names[i]]]
    
    ## testing:  tracking down dim issues
    #cat("\n\n", element.names[i], "dim test: ", dim(my.trades)[1])   
    
    if (dim(my.trades)[1] > 0) {
      ## Convert to XTS
      timestamps <- as.POSIXct(x = row.names(my.trades), tz = "America/Chicago", "%Y-%m-%d %H:%M:%OS" )  
      temp <- make.index.unique(xts(my.trades$cm.pnl, order.by = timestamps), eps = .000002)
      
      ## if the first element, create the output object, else add previous PNL to current object
      if (element.names[i] == first(element.names)) {
        output <- temp
      } else {
        previous.pnl <- as.numeric(last(output)[1])
        temp[,1] <- previous.pnl + temp[,1]
        output <- rbind(output, temp)
      }
    }
  }
  return(output)
}  


#   -----------------------------------------------------------------------
MakeTimeSequentialEquityCurves <- function(paramset, filter = NULL) {
  ## takes in a list of trades, subsets the list, orders them sequentially, 
  ## and outputs a time-sequential cumulative PNL object for making equity curves
  #
  # Args
  #   paramsets           a list containing trades
  #   filter              character vector of values to remove from the list, eg "block01"
  # Returns
  #   a 1-column XTS object with cumulative PNL
  # -----------------------------------------------------------------------
  # Testing 
  # filter <- c("block01", "block03")
  ## trap for errors
  x <- get(paramset)
  stopifnot(is.list(x))  
  stopifnot(is.character(filter) || is.null(filter))
  
  ## filter out garbage from the list
  if (!is.null(filter)) {
    for (i in seq(filter)) {
      x <- x[-grep(pattern = filter[i], names(x))]
      cat("\nremoved elements matching:", filter[i])
    }
  }
  
  ## order by date, then by block
  x <- x[order(names(x))]
  
  ## call BindRows function
  output <- BindRowsAndRecalcPNL(list.name = x, element.names = names(x))
}



#   -----------------------------------------------------------------------
CalcEquityCurveSummaryStats <- function(paramset, stop.amt=NULL, filter=NULL, verbose=F) {
  ## takes in a list of trades, subsets the list, orders them sequentially, 
  ## and outputs a trade stats list (similar to MakeTimeSequentialEquityCurves function)
  # 
  # Args
  #   paramset            character name of a list containing trades.  each list element usually corresponds
  #                       to a date-timeblock of trades, like all the trades on Jan 1, 2015, 
  #                       between 6:15 and 6:45
  #   filter              character vector of values to remove from the list, eg "block01"
  #   stop.amt            used to determine if the strategy was stopped out during this period
  #
  # Returns
  #   a list containing:
  #   1. stats
  #   2. end of day PNLs
  #   3. datatframe of closed trades (when position = 0)
  #   4. dataframe of returns (of closed trades)
  #   5. cumulative PNL (all trades)
  # ------------------------------------------------------------------
  ## Trap for errors ##
  if (!is.character(paramset)) stop("ERROR: paramset should be a character pointer to a list")
  x <- get(paramset)  
  cat("\nProcessing: ", paramset)
  stopifnot(is.list(x))  
  stopifnot(is.character(filter) || is.null(filter))
  
  ## filter out garbage from the list ##
  if (!is.null(filter)) {
    for (i in seq(filter)) {
      x <- x[-grep(pattern = filter[i], names(x))]
      if (verbose) cat("\nremoved elements matching:", filter[i])
    }
  }
  
  ## order by date, then by block
  if (length(x) == 0) stop("ERROR: all elements have been filtered out of the list")
  x <- x[order(names(x))]
  
  ## create output list
  output <- list()
  my.row.names <-  c("NetPNL","Total Roundtrips", "Total Trades (Flat-to-Flat)", "Total Trades (RetOnMargin>0)", "Average Trade(PNL/TotalTrades)", "Average PNL Per RoundTrip", "Average Return on Margin", "Long Trades", "Pct Long Trades", 
                     "Short Trades", "Pct Short Trades", "Win Count", "Long Win Count", "Short Win Count", "Pct Winners", "Average Win($)",
                     "Loss Count", "Long Loss Count", "Short Loss Count", "Pct Losers", "Average Loss($)", "Number of Trading Days", "Average Daily PNL",
                     "Best End-Of-Day PNL", "Worst End-Of-Day PNL", "Winning Days Count", "Winning Days Pct", "Losing Days Count", "Losing Days Pct",
                     "Periods Stopped Out", "Total Time Blocks","Winning Blocks Count", "Losing Blocks Count", "Stop-Out Percentage", "Highest PNL", "Lowest PNL", 
                     "Max Long Position", "Avg Max Long Position", "Max Short Position", "Avg Max Short Position", 
                     "Max Drawdown($)", "Max Drawdown(pct)", "Average Recovery", "Average Drawdown", "NetPNL/Max Drawdown", "Profit Factor", "Regression Slope", "Standard Dev(PNL)", "SemiDev(PNL)", "Standard Dev(Returns)", "SemiDev(Returns)", "Ulcer Index",
                     "Sharpe Ratio", "Sortino Ratio", "System Quality Number(PNL)", "System Quality Number(Returns)", "Omega Ratio",
                     "Robust Sharpe Ratio", "Robust SQN w/Returns", "Robust Sharpe w/SemiDev", "Robust SQN w/SemiDev", "Robust SQN w/Ulcer"
  )
  stats <- as.data.frame(matrix(data = NA, 
                                nrow = length(my.row.names), 
                                ncol = 1, 
                                dimnames = list(my.row.names, paramset)))
  long.win.ct       <- 0
  long.loss.ct      <- 0
  shrt.win.ct       <- 0
  shrt.loss.ct      <- 0
  gross.profit      <- 0
  gross.loss        <- 0
  net.pnl           <- 0
  total.roundtrips  <- 0
  stopped.out       <- 0
  biggest.win       <- 0
  biggest.loss      <- 0
  best.period       <- 0
  worst.period      <- 0
  last.date         <- NULL
  best.eod.pnl      <- 0
  worst.eod.pnl     <- 0
  avg.eod.pnl       <- 0 
  all.trades        <- NULL  # all end pnl from each flat-to-flat trade
  all.eod.pnl       <- NULL  # all end pnl from each day
  all.end.pnl       <- NULL  # all end pnl from each timeblock
  previous.pnl      <- NULL  # used in cumulative PNL calc
  all.cm.pnl        <- NULL
  trading.periods   <- 0
  all.max.long.pos  <- NULL
  all.max.shrt.pos  <- NULL
  
  ## Cycle through all of the dataframes in the list ##
  for (i in seq(names(x))) {
    if (dim(x[[i]])[1] > 0) {  ## skip anything without data in it
      if (verbose) cat("\nProcessing", names(x)[i])
      
      ## -------------------------------------
      ## Global Stats ##
      ## -------------------------------------
      trading.periods <- trading.periods + 1
      end.pnl <- last(with(x[[i]], cm.pnl))
      net.pnl <- net.pnl + end.pnl
      all.end.pnl <- c(all.end.pnl, end.pnl)
      best.period  <- max(best.period, end.pnl)  # best time block
      worst.period <- min(worst.period, end.pnl) # worst time block
      if (!is.null(stop.amt)) {
        if (end.pnl < stop.amt) stopped.out <- stopped.out + 1  
        if (verbose) cat("\nStopped-Out Count: ", stopped.out)
      }
      total.roundtrips <- total.roundtrips + sum(with(x[[i]], qty))/2
      
      
      ## -------------------------------------
      ## Daily stats ##
      ## -------------------------------------
      ## check the date in row 1 of the trades object
      current.date <- as.Date(rownames(x[[i]])[1], format = "%Y-%m-%d")
      if (is.null(last.date)) {                # first day? if so, initialize counters
        all.eod.pnl       <- end.pnl     
        day.ct            <- 1
        win.days.count    <- 0 
        losing.days.count <- 0
      } else if (current.date == last.date) {  # same day?  increment today's pnl
        all.eod.pnl[day.ct] <- all.eod.pnl[day.ct] + end.pnl  
      } else if (current.date > last.date) {   # new day? increment date ct and add a new element to eod.pnl vector
        all.eod.pnl   <- c(all.eod.pnl, end.pnl)
        if (all.eod.pnl[day.ct] >= 0) {
          win.days.count <- win.days.count + 1
        } else losing.days.count <- losing.days.count + 1
        day.ct        <- day.ct + 1
      }
      last.date   <- current.date
      
      
      ## -------------------------------------
      ## Per-Trade Stats ##
      ##
      ## - trade defined as a closed position
      ## - these are stats that can be calc'd 
      ##   without going through each row
      ## -------------------------------------
      ## find PNL at close of each position and adjust if there are more than one
      trade.side   <- x[[i]][with(x[[i]], qty > 0 & action == "CLOSE:"), "side"]     
      trade.pnl    <- x[[i]][with(x[[i]], qty > 0 & action == "CLOSE:"), "cm.pnl"]   
      trade.dates  <- rownames(x[[i]][with(x[[i]], qty > 0 & action == "CLOSE:"), ]) 
      timestamps   <- as.POSIXct(x = row.names(x[[i]]), tz = "America/Chicago", "%Y-%m-%d %H:%M:%OS" )  
      cm.pnl       <- as.xts(x[[i]][,"cm.pnl"], order.by = timestamps)
      ## using ifelse here for backwards compatibility.  if not using modify qty function, get rid of the "else" part
      if ("returns" %in% names(x[[i]])) {
        returns <- as.xts(x[[i]][,"returns"], order.by = timestamps)
      } else if ("return" %in% names(x[[i]])) {
        returns <- as.xts(x[[i]][,"return"], order.by = timestamps)
      } else stop("ERROR: there is no return or returns column in this dataframe")
      if ("cm.returns" %in% names(x[[i]])) {
        cm.returns <- as.xts(x[[i]][,"cm.returns"], order.by = timestamps)
      } else if ("cm.return" %in% names(x[[i]])) {
        cm.returns <- as.xts(x[[i]][,"cm.return"], order.by = timestamps)
      } else stop("ERROR: there is no cm.return or cm.returns column in this dataframe")
      
      ## Find Counts of Long, Short, Wins and Losses
      ## NOTE: this needs to be vectorized instead of a for-loop
      if (length(trade.pnl) > 1) trade.pnl <- c(trade.pnl[1], diff(trade.pnl))  # converts cumulative pnl to a per-trade trade.pnl
      for (t in seq(trade.side)) {
        if (trade.pnl[t] >= 0 && trade.side[t] == "S") {
          long.win.ct    <- long.win.ct + 1   # long trades need a sell to close them
        } else if (trade.pnl[t] >= 0 && trade.side[t] == "B") {
          shrt.win.ct    <- shrt.win.ct + 1   # short trades need a buy to close them
        } else if (trade.pnl[t] < 0 && trade.side[t] == "S") {
          long.loss.ct   <- long.loss.ct + 1   # long trades need a sell to close them
        } else if (trade.pnl[t] < 0 && trade.side[t] == "B") {
          shrt.loss.ct   <- shrt.loss.ct + 1   # long trades need a sell to close them
        } else stop("ERROR: issue with win, loss, short, long count")    
      }  
      
      ## -----------------------------------------------
      ## Per-Row Stats ##
      ##
      ## - these stats require going through each row
      ## - usually has an order or time characteristic
      ## - example: max position
      ## -----------------------------------------------
      my.pos       <- 0
      max.long.pos <- 0
      max.shrt.pos <- 0
      for (row in index(x[[i]])) {
        if (x[[i]][row, "side"] == "B") {
          my.pos <- my.pos + 1 * x[[i]][row, "qty"]
        } else if (x[[i]][row, "side"] == "S") {
          my.pos <- my.pos - 1 * x[[i]][row, "qty"]
        } else stop("ERROR: can't calculate my.pos, there is no B or S in row", row, "of", names(x)[i])
        max.long.pos <- max(max.long.pos, my.pos)
        max.shrt.pos <- min(max.shrt.pos, my.pos)
      }
      ## Calculations at the end of the list element
      all.max.long.pos <- c(all.max.long.pos, max.long.pos)  
      all.max.shrt.pos <- c(all.max.shrt.pos, max.shrt.pos)
    }
    
    ## Convert Trades and Cumulative PNL objects to XTS, bind together
    timestamps  <- as.POSIXct(x = trade.dates, tz = "America/Chicago", "%Y-%m-%d %H:%M:%OS" )  
    trades      <- as.xts(trade.pnl, order.by = timestamps)
    all.trades  <- rbind(all.trades, trades) 
    if (is.null(all.cm.pnl)) {    # First block of trades?
      all.cm.pnl      <- cm.pnl
      all.cm.returns  <- cm.returns
      all.returns     <- returns
    } else {
      previous.pnl    <- as.numeric(last(all.cm.pnl)[1])  # add last value to adjust current cm.pnl
      previous.return <- as.numeric(last(all.cm.returns)[1])
      cm.pnl          <- previous.pnl + cm.pnl[,1]
      cm.returns      <- previous.return + cm.returns[,1]
      all.cm.pnl      <- rbind(all.cm.pnl, cm.pnl)
      all.cm.returns  <- rbind(all.cm.returns, cm.returns)
      all.returns     <- rbind(all.returns, returns)
    }
    # cat("\nThis is the cm.pnl object\n"); print(cm.pnl)
    # cat("\nThis is the all.cm.pnl object\n"); print(all.cm.pnl)
    ## END PROCESSING OF LIST ELEMENT (i.e  trades element in a parameter set)
  }
  
  ## ----------------------------------------------- ##
  ## AFTER PROCESSING TRADES, RUN THESE GLOBAL STATS ##
  ## ----------------------------------------------- ##
  ## Calculate Stats from Cumulative PNL ##
  highest.pnl      <- NULL
  lowest.pnl       <- 0
  max.dd.dollars   <- 0
  max.dd.pct       <- 0
  for (row in seq(all.cm.pnl)) {
    current.value   <- as.numeric(all.cm.pnl[row, 1])
    highest.pnl    <- ifelse(is.null(highest.pnl), current.value, max(current.value, highest.pnl))   # need to do here to calc drawdown
    lowest.pnl     <- min(current.value, lowest.pnl)    # not necessary to do here
    drawdown       <- highest.pnl - current.value     # how far away from the highs?
    max.dd.dollars <- max(max.dd.dollars, drawdown)
  }
  
  ## Calculate Returns
  ## 2015-5-22 not needed, wasn't workign so i did it in using the modify qty function
  library(PerformanceAnalytics)
  ## to find returns, take all the trades, calculate the cumulative PNL
  ## then use log returns because they are additive:  average log returns are (.20 + .25) / 2
  ## TO DO: convert sharpe, sortino, sqn to use return on margin
  ##    take these returns, divide them up by margin used
  # suppressWarnings(my.returns  <- CalculateReturns(cumsum(all.trades), method = "log"))  # find cumulative returns
  
  ## Calculate Linear Regression of the time series
  ## NOTE:  might need to convert to daily to standardize?
  ##        Also should use cumulative return on margin instead
  x <- index(all.cm.returns)
  y <- all.cm.returns[,1]
  regression <- lm(y ~ x)
  slope <- 100000 * regression$coefficients[2]
  
  
  ## Using PNL instead of returns to calculate the following
  ## switch over to return on margin when i can...
  n.trades            <- nrow(all.trades)
  my.mean.pnl         <- mean(all.trades, na.rm = T)
  my.sd.pnl           <- sd(all.trades, na.rm = T)
  my.semidev.pnl      <- SemiDeviation(all.trades)
  # # # # # 
  n.trades.returns    <- length(all.returns[abs(all.returns) > 0])
  my.mean.returns     <- mean(all.returns)
  my.sd.returns       <- sd(all.returns)
  my.semidev.returns  <- SemiDeviation(all.returns)
  ulcer.index.returns <- UlcerIndex(all.returns)
  # # # # # 
  sharpe              <- my.mean.returns / my.sd.returns
  sortino             <- my.mean.returns / my.semidev.returns
  sqn.pnl             <- sqrt(n.trades) * my.mean.pnl / my.sd.pnl
  sqn.returns         <- sqrt(n.trades.returns) * my.mean.returns / my.sd.returns
  # # # # #
  robust.sharpe       <- slope / my.sd.returns
  robust.sqn          <- robust.sharpe * sqrt(n.trades.returns) 
  robust.sharpe.with.semideviation <- slope/my.semidev.returns 
  robust.sqn.with.semideviation    <- robust.sharpe.with.semideviation * sqrt(n.trades.returns) 
  robust.sqn.with.ulcer            <- sqrt(n.trades.returns) * slope / ulcer.index.returns 
  omega.ratio                      <- Omega(R = all.returns)[1]
  # # # # # 
  avg.recovery        <- PerformanceAnalytics::AverageRecovery(all.returns)
  avg.dd              <- PerformanceAnalytics::AverageDrawdown(all.returns)
  max.dd.pct          <- PerformanceAnalytics::maxDrawdown(R = all.returns, geometric = FALSE, invert = FALSE)
  
  
  # Misc Stats
  winning.blocks    <- length(all.end.pnl[all.end.pnl >= 0])
  losing.blocks     <- length(all.end.pnl[all.end.pnl < 0])
  pct.stopped.out   <- stopped.out / trading.periods
  gross.profit      <- sum(diff(all.cm.pnl)[diff(all.cm.pnl) >= 0])
  gross.loss        <- sum(all.cm.pnl[1,1], diff(all.cm.pnl)[diff(all.cm.pnl) <  0]) # add in the first commish
  best.eod.pnl      <- max(all.eod.pnl)
  worst.eod.pnl     <- min(all.eod.pnl) 
  biggest.win       <- max(0, all.trades[all.trades > 0])
  biggest.loss      <- min(0, all.trades[all.trades < 0]) 
  long.trades       <- long.win.ct + long.loss.ct 
  shrt.trades       <- shrt.win.ct + shrt.loss.ct
  total.trades      <- long.trades + shrt.trades
  avg.trade         <- net.pnl/total.trades
  avg.pnl.per.rt    <- net.pnl/total.roundtrips
  pct.long.trades   <- long.trades/total.trades
  pct.shrt.trades   <- shrt.trades/total.trades
  win.ct            <- long.win.ct + shrt.win.ct
  loss.ct           <- long.loss.ct + shrt.loss.ct
  pct.wins          <- 100 * win.ct/total.trades
  pct.loss          <- 100 * loss.ct/total.trades
  avg.win.pnl       <- sum(all.trades[all.trades >= 0]) / win.ct
  avg.loss.pnl      <- sum(all.trades[all.trades <  0]) / loss.ct
  avg.eod.pnl       <- mean(all.eod.pnl)
  net.pnl.max.dd    <- net.pnl / max.dd.dollars
  #cat("\ngross profit is:", gross.profit)
  #cat("\ngross loss is:", gross.loss)
  profit.factor     <- abs(gross.profit / gross.loss)
  max.long.pos      <- max(all.max.long.pos)
  avg.max.long.pos  <- mean(all.max.long.pos)
  max.shrt.pos      <- min(all.max.shrt.pos)
  avg.max.shrt.pos  <- mean(all.max.shrt.pos)
  all.max.pos       <- cbind(all.max.long.pos, all.max.shrt.pos)
  pct.stopped.out   <- 100 * pct.stopped.out
  pct.long.trades   <- 100 * pct.long.trades
  pct.shrt.trades   <- 100 * pct.shrt.trades
  win.days.pct      <- 100 * win.days.count / day.ct
  losing.days.pct   <- 100 * losing.days.count / day.ct
  
  cat("\nAssigning stats values to output")
  stats[, 1] <- c(net.pnl, total.roundtrips, total.trades, n.trades.returns, avg.trade, avg.pnl.per.rt, my.mean.returns, long.trades, pct.long.trades,
                  shrt.trades, pct.shrt.trades, win.ct, long.win.ct, shrt.win.ct, pct.wins, avg.win.pnl,
                  loss.ct, long.loss.ct, shrt.loss.ct, pct.loss, avg.loss.pnl, day.ct, avg.eod.pnl, 
                  best.eod.pnl, worst.eod.pnl, win.days.count, win.days.pct, losing.days.count, losing.days.pct,
                  stopped.out, trading.periods, winning.blocks, losing.blocks, pct.stopped.out, highest.pnl, lowest.pnl,
                  max.long.pos,  avg.max.long.pos, max.shrt.pos, avg.max.shrt.pos, 
                  max.dd.dollars, max.dd.pct, avg.recovery, avg.dd, net.pnl.max.dd, profit.factor, slope, my.sd.pnl, my.semidev.pnl, my.sd.returns, my.semidev.returns, ulcer.index.returns,                  
                  sharpe, sortino, sqn.pnl, sqn.returns,  omega.ratio,
                  robust.sharpe, robust.sqn, robust.sharpe.with.semideviation, robust.sqn.with.semideviation, robust.sqn.with.ulcer 
  )
  
  ## make output list ##
  colnames(all.trades)     <- paste("all.trades", paramset, sep = ".")
  colnames(all.cm.pnl)     <- paste("cm.pnl", paramset, sep = ".")
  colnames(all.returns)    <- paste("returns", paramset, sep = ".")
  colnames(all.cm.returns) <- paste("cm.returns", paramset, sep = ".")
  output[[1]]   <- round(stats, 5)
  output[[2]]   <- all.eod.pnl
  output[[3]]   <- all.trades
  output[[4]]   <- all.cm.returns
  output[[5]]   <- all.cm.pnl
  output[[6]]   <- all.max.pos
  output[[7]]   <- all.returns
  output[[8]]   <- all.end.pnl  # ending PNL from each time block
  my.names      <- paste(c("stats", "all.eod.pnl", "all.trades", "cm.returns", "cm.pnl", "all.max.pos", "all.returns", "all.timeblocks.endpnl"), paramset, sep = ".")
  names(output) <- my.names
  return(output)
}


#   -----------------------------------------------------------------------
GetParameterValues <- function(all.params, object.name) {
  # parses the object name for parametr values, returns a named vector of values
  # 
  # Args
  #   all.params      <- vector of params like "param1", "param2", etc
  #   object.name     <- either a single or several name(s) to parse
  # Returns
  #   either a matrix or a vector of values, rows are the values per object, columns are parameters
  # -----------------------------------------------
  ## Trap for errors
  stopifnot(is.character(object.name))
  if (length(all.params) == 0) stop("ERROR: length of all.params is 0")
  if (!is.character(all.params)) stop("ERROR: all.params needs to be a char vector of params")
  
  ## Prep the output matrix
  output <- matrix(nrow = length(object.name), ncol = (length(all.params)))
  colnames(output) <- all.params
  
  ## Process the name and parameters
  for (i in seq(object.name)) {
    # i <- 4
    name <- unlist(strsplit(object.name[i], "[.]"))      
    for (j in seq(all.params)) {
      # j <- 1
      x             <- name[grepl(all.params[j], name)]  # match against components
      output[i, j]  <- as.numeric(substr(x, nchar(all.params[j]) + 1, nchar(x)))
    } 
  }
  return(output)
}



#   -----------------------------------------------------------------------
Match.numeric <- function(x, table) {
  # Numeric Matching function to accomodate for floating point errors
  are.equal <- function(x, y) isTRUE(all.equal(x, y))
  match.one <- function(x, table)
    match(TRUE, vapply(table, are.equal, logical(1L), x = x))
  vapply(x, match.one, integer(1L), table)
}


#  ------------------------------------------------------------------------
ModifyTradeQuantities <- function(paramset.list, commissions=.50, stop.amt=-1500, price.multiplier=.01,
                                  value.per.dollar=1000, min.price.increment = .01, bid.sizes=rep(1, 15), 
                                  offer.sizes=rep(1,15), profit.ticks=5, order.spacing=1, initial.offset=1, end.times.per.block = NULL,
                                  cancel.entry.mode=NA, cancel.entry.times=NULL, margin=4250, keep.checking.for.flat=TRUE,
                                  logfile="", modify.qty=NULL, doParallel = FALSE,
                                  loglevel=1) {
  # Takes in a list of trades and desired bid & offer quantities, modifies the trades
  # and spits out a list with modified trade sizes. format is the same as a regular parmset.list
  #
  # Args
  #   paramset.list           character name of a list that contains trades
  #   commissions             commissions rate per-side, NOT roundtrip
  #   stop.amt                when to stop out, trades beyond this point are discarded
  #   price.multiplier        the correct multiplier to get minimum value, eg .01 for CL, .0001 for currencies, etc
  #   value.per.dollar        once multiplied, the price value for each dollar move
  #                           eg 1000 for CL, 50 for ES, etc
  #   min.price.increment     the change in price from one tick the next, eg .01 for CL, .25 for ES, etc
  #   bid.sizes               numeric vector of bid sizes, with [1] being closest to the market, etc
  #   offer.sizes             numeric vector of offer sizes, with [1] being closest to the market, etc
  #   profit.ticks            where the profit orders would go in relation to the entry order
  #   order.spacing           spacing for orders.  1 = every price, 2 = every other price, etc
  #   intial offset           0 = at the best bid/offer, 1 means 1 tick back, 2 = 2 ticks back, etc
  #   end.times.per.block     dataframe with block names in col 1 and end times in col 2.  like: block01, 06:45:00
  #   cancel.entry.mode       modifies behavior after cancel entry time has passed. 
  #                           0 = cancel all entry orders, 1 = cancel all entries on the same side as the position, 
  #                           -1 = cancel orders on the opposite side of the position
  #   cancel.entry.times      dataframe with block names in col 1 and entry order cancel times in col 2. like: block01, 06:45:00
  #   keep.checking.for.flat  if using cancel entry mode and this is TRUE, all entries are cancelled once the position is flat and the timer has ended
  #   margin                  margin of the product.  used to calculate return on margin and cumulative return on margin
  #   loglevel                1 = no logging, 2 = qty, pos updates, 3 = print order book at each row
  #
  # Returns
  #   list of dataframes with modified quantities and cumulative PNL
  # -------------------------------------------------------------------------
  ## Trap For Errors
  ## ----------------
  if (is.character(paramset.list)) paramset.list <- get(paramset.list)
  stopifnot(!is.null(names(paramset.list)),
            stop.amt < 0, 
            all(c(commissions, price.multiplier, value.per.dollar, initial.offset) > 0),
            is.list(paramset.list),
            order.spacing > 0,
            profit.ticks > 0,
            cancel.entry.mode %in% c(NA,0,1,-1),
            is.data.frame(end.times.per.block) || is.null(end.times.per.block),
            is.data.frame(cancel.entry.times) || is.null(cancel.entry.times)
  )
  if (ncol(paramset.list[[1]]) > 6) {
    stop("ERROR: this function has already been run on this parameter set! no need to re-run") 
  }
  
  ## Functions
  ## ----------------
  CalcProfitOrderPrice <- function(trade.side, order.number, trade.price, profit.ticks.mult, profit.ticks, min.price.increment) {
    profit.ticks.mult    <- ifelse(trade.side == "B", 1, -1)  # set multiplier based on trade side
    output <- format(trade.price + (profit.ticks.mult * profit.ticks * min.price.increment), nsmall = num.decimal.places)
    return(output)
  }
  MatchOrderPrice <- function(element, trade.qty, trade.price, trade.side, init.side.name, initial.side, opposite.side, offset.mult, opp.side.name=NULL, opp.side.book.set=NULL) {
    # looks up the trade price and returns the virtual trade qty
    #
    # returns
    #   a list with trade.qty, order.side, order.column, order.number, opp.side.book.set
    # -------------------------------------------------------
    ## Variables
    trade.price          <- format(trade.price, nsmall = num.decimal.places)
    unmodified.trade.qty <- trade.qty ## for debugging
    order.number         <- NA
    traded.entry.order   <- FALSE
    
    ## Set up 2 cases:  match against either entry orders or profit orders
    case1  <- (trade.side == "B" && init.side.name == "bids")   || (trade.side == "S" && init.side.name == "offers")  
    case2  <- (trade.side == "B" && init.side.name == "offers") || (trade.side == "S" && init.side.name == "bids")    
    
    ## Match Profit Targets
    if (case1 && opp.side.book.set) {
      # order.number <- Match.numeric(trade.price, table = opposite.side[, "profit.price"])
      order.number <- grep(paste0(trade.price, "$"), opposite.side[, "profit.price"])
      order.side   <- "opposite.side"
    } else if (case2) {
      #order.number <- Match.numeric(trade.price, table = initial.side[, "profit.price"])
      order.number <- grep(paste0(trade.price, "$"), initial.side[, "profit.price"])
      order.side   <- "initial.side"
    } 
    
    # Failure to match with grep results in "integer(0)", so reset for matching against entries
    order.number <- ifelse(length(order.number) == 0, NA, 
                           ifelse(length(order.number) == 1, order.number, 
                                  stop("ERROR: multiple matches for order number")))  
    
    ## Match Entries & Set Opposite Side Book if needed
    if (case1 && is.na(order.number)) {
      #order.number <- Match.numeric(trade.price, table = initial.side[, "order.price"])
      order.number <- grep(paste0(trade.price, "$"), initial.side[, "order.price"])            
      order.side   <- "initial.side"
      traded.entry.order <- TRUE
    } 
    if (case2 && is.na(order.number)) {
      if (opp.side.book.set == FALSE) {
        opposite.side <- InitializeOrderBook(trade.price, 
                                             order.book = opposite.side, 
                                             multiplier = -offset.mult)  
        opp.side.book.set <- TRUE          
      }
      # order.number <- Match.numeric(trade.price, table = opposite.side[, "order.price"])
      order.number <- grep(paste0(trade.price, "$"), opposite.side[, "order.price"])        
      order.side   <- "opposite.side"
      traded.entry.order <- TRUE
    }
    if (!is.numeric(order.number)) {  # order number did not get set
      if (loglevel > 1) {
        cat("\nERROR: Did not successfully match and modify the trade qty in row", row, "of", names(paramset.list)[element])
        cat("\n\n************************************")
        cat("\nThis is the trade price:", trade.price)
        cat("\nThis is the trade side:", trade.side)
        cat("\nThese are the", init.side.name, "\n")
        print(initial.side)
        cat("\nThese are the", opp.side.name, "\n")
        print(opposite.side)
      }
      stop("ERROR: Did not successfully modify the trade qty in row ", row, " of ", names(paramset.list)[element])          
    }
    ## Determine Trade Qty
    order.column <- ifelse(traded.entry.order, "order.price", "profit.price")
    trade.qty <- ifelse(order.side == "initial.side", initial.side[order.number, "order.qty"], opposite.side[order.number, "order.qty"])
    if (is.na(trade.qty)) browser() #stop("ERROR: trade.qty was not properly set at row", row, "of ", names(paramset.list)[element])
    
    ## Output
    output <- list(trade.qty, order.side, order.column, order.number, opp.side.book.set, initial.side, opposite.side)
    names(output) <- c("trade.qty", "order.side", "order.column", "order.number", "opp.side.book.set", "initial.side", "opposite.side")
    return(output)
  }
  ProcessTheTrade <- function(element, cm.return, cm.pnl, trade.price, trade.side, trade.qty, commissions, cm.pos, margin, max.position, average.price, value.per.dollar, row) {
    # process the trade and output PNL
    #
    # 5 cases
    # - nothing happend - order qty was 0
    # - flip a position (eg long to short)
    # - closed a position (position = 0)
    # - add to a position
    # - reduce a position
    # --------------------------------------
    ## Set sign for trade qty (helps standardize calculations)
    if (trade.side == "S") trade.qty <- (-1) * trade.qty
    trade.commish <- abs(trade.qty) * commissions
    new.cm.pos    <- cm.pos + trade.qty
    if (loglevel > 1) cat("\nNew CmPos for row", row, "is", new.cm.pos)
    
    # Case1: Order qty was 0, continue on to next row
    if (trade.qty == 0) {
      trade.pnl        <- 0
      return.on.margin <- 0
      if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
      if (loglevel > 1) cat("\nSynthetic Order Qty is 0, so everything stays the same")
      
      # Case2: Position Flipped from long to short, or vice versa.  there must be a simpler way to code this...
    } else if (cm.pos > 0 && new.cm.pos < 0 || cm.pos < 0 && new.cm.pos > 0 ) {  
      trade.pnl        <- trade.commish + (cm.pos * trade.price - cm.pos * average.price) * value.per.dollar
      return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
      average.price    <- trade.price
      if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
      if (loglevel > 1) cat("\nFlipped Position, New Average Price is", average.price, "for row", row)
      
      # Case3: Position Closed
    } else if (new.cm.pos == 0) {       
      trade.pnl        <- trade.commish + (trade.qty * average.price - trade.qty * trade.price) * value.per.dollar
      return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
      average.price    <- 0
      if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
      if (loglevel > 1) cat("\nClosed Position, new Average Price is", average.price, "for row", row)
      
      # Case4: Added to an existing position (either long or short)
    } else if (abs(new.cm.pos) > abs(cm.pos)) {  
      trade.pnl        <- trade.commish
      return.on.margin <- 0
      average.price <- (trade.qty * trade.price + cm.pos * average.price) / new.cm.pos
      if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
      if (loglevel > 1) cat("\nAdded to Position, new Average Price is", average.price, "for row", row)
      
      # Case5: Reduced an existing position (either long or short)  
    } else if (abs(new.cm.pos) < abs(cm.pos)) {  
      trade.pnl        <- trade.commish + (trade.qty * average.price - trade.qty * trade.price) * value.per.dollar
      return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
      if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
      if (loglevel > 1) cat("\nAverage Price stays the same because i reduced position:", average.price)
      
    } else stop("ERROR: issue with PNL calc at row ", row, " in ", names(paramset.list)[element])
    cm.pos    <- new.cm.pos
    cm.pnl    <- cm.pnl + trade.pnl
    cm.return <- cm.return + return.on.margin
    if (loglevel > 1) cat("\nCmPos for row", row, "is", cm.pos)
    if (loglevel > 1) cat("\nCmPNL for row", row, "is", cm.pnl)
    if (loglevel > 1) cat("\nCmReturn for row", row, "is", cm.return)
    trade.action <- ifelse(cm.pos == 0, "CLOSE:", "OPEN:")
    
    ## output 
    output <- list(cm.pos, cm.pnl, cm.return, trade.action, trade.pnl, return.on.margin, trade.side, average.price)
    names(output) <- c("cm.pos", "cm.pnl", "cm.return", "trade.action", "trade.pnl", "return.on.margin", "trade.side", "average.price")
    return(output)
  }
  InitializeOrderBook <- function(trade.price, order.book, multiplier) {
    # populate the order book based on the first trade price
    #
    # args
    #   trade.price     this is the first trade
    #   order.book      either the initial or opposite book
    #   multiplier      1 * offset multiplier if initial side, -1 if opposite side
    #
    # return
    #   order book matrix with prices 
    # -----------------------------------------
    order.book[1, "order.price"] <- format(trade.price, nsmall = num.decimal.places)
    for (i in 2:nrow(order.book)) {
      new.order <- as.numeric(order.book[i - 1, "order.price"]) + (multiplier * order.spacing * min.price.increment)
      order.book[i, "order.price"] <- format(new.order, nsmall = num.decimal.places)
    }
    return(order.book)
  }
  CreateOutputRow <- function(trade.side, trade.qty, trade.product, trade.price, cm.pnl, trade.action, trade.return, cm.return, order.number, entry.pnl) {
    # writes values to the output dataframe 
    #
    # args
    #
    # returns
    #   a 1-row dataframe to save to the output dataframe
    # ------------------------------------------------
    if (trade.side == "S") order.number <- as.numeric(order.number) * -1  # track which entry was closed.  negative for bids, positive for offer entries
    output <- as.data.frame(list(trade.side, trade.qty, trade.product, format(trade.price, nsmall = num.decimal.places), cm.pnl, trade.action, trade.return, cm.return, order.number, entry.pnl), stringsAsFactors = FALSE)
    return(output)
  }
  CancelRemainingEntries <- function(entries.are.cancelled, one.side.cancelled, element, cancel.entry.mode, cm.pos, initial.side, init.side.name, opposite.side, opp.side.name, keep.checking.for.flat) {
    # after the timer expires, runs the ClearOrderBook function depending on the cancel entry mode
    #
    # args
    #   cancel.entry.mode       either -1, 0, 1 (-1 cancels opposite side, 0 cancels both, 1 cancels same side)
    #                           cannot continue to accumulate a position if mode == 0
    #   cm.pos                  cumulative position when timer expires
    #   initial.side            a matrix representing the order book of entries and profit orders
    #   init.side.name          either "bids" or "offers"
    #   opposite.side           a matrix representing the order book of entries and profit orders
    #   opp.side.name           either "bids" or "offers"
    #   keep.checking.for.flat  T or F, whether or not to cancel orders after the position flattens
    #                           if True, cancel the one side and allow accumulation on the other side
    # returns
    #   a list containing the two order matrices
    # -----------------------------------------------------------------------------------
    if (loglevel > 1) cat("\nCancel Entries time has passed, cancelling entries now")
    ## 3 cases
    ## - mode 0
    ## - mode (-1,1) and position is flat
    ## - mode (-1,1) we've got a position, so clear one side and allow accumulation on the other
    ## -------------------------------------    
    case1 <- cancel.entry.mode == 0                            # cancel all, set flag
    case2 <- cancel.entry.mode %in% c(-1,1) && cm.pos == 0     # cancel all, set flag
    case3 <- cancel.entry.mode %in% c(-1,1) && cm.pos != 0     # cancel one side and keep checking position == 0 to cancel the other
    if (case1 || case2) {
      if (loglevel > 2) cat("\nCancelling all bids and offers - either flat or cancel mode is 0")
      initial.side          <- ClearOrderBook(initial.side)
      opposite.side         <- ClearOrderBook(opposite.side)
      entries.are.cancelled <- TRUE  
    } else if (case3 && one.side.cancelled == FALSE) {
      if (cancel.entry.mode == -1 && cm.pos > 0) {
        if (loglevel > 2) cat("\nCancelling all offers - I'm long and cancel mode is -1")
        if (init.side.name == "offers")     initial.side  <- ClearOrderBook(initial.side)
        else if (opp.side.name == "offers") opposite.side <- ClearOrderBook(opposite.side)
        else stop("ERROR: can't figure out which side of the book to clear out")
      } else if (cancel.entry.mode == -1 && cm.pos < 0) {
        if (loglevel > 2) cat("\nCancelling all bids - I'm short and cancel mode is -1")
        if (init.side.name == "bids")       initial.side  <- ClearOrderBook(initial.side)
        else if (opp.side.name == "bids")   opposite.side <- ClearOrderBook(opposite.side)
        else stop("ERROR: can't figure out which side of the book to clear out")    
      } else if (cancel.entry.mode ==  1 && cm.pos > 0) {
        if (loglevel > 2) cat("\nCancelling all bids - I'm long and cancel mode is 1")
        if (init.side.name == "bids")       initial.side  <- ClearOrderBook(initial.side)
        else if (opp.side.name == "bids")   opposite.side <- ClearOrderBook(opposite.side)
        else stop("ERROR: can't figure out which side of the book to clear out")            
      } else if (cancel.entry.mode ==  1 && cm.pos < 0) {
        if (loglevel > 2) cat("\nCancelling all offers - I'm short and cancel mode is 1")
        if (init.side.name == "offers")     initial.side  <- ClearOrderBook(initial.side)
        else if (opp.side.name == "offers") opposite.side <- ClearOrderBook(opposite.side)
        else stop("ERROR: can't figure out which side of the book to clear out")
      } else stop("ERROR, trying to cancel entries at ", row, "of ", names(paramset.list)[element], "but something is screwed up") 
      ## if i'm still checking for flat, we'll keep looping through this function until 
      ## position is flat, and then we'll meet case 2 criteria
      if (keep.checking.for.flat == FALSE) {
        entries.are.cancelled   <- TRUE   # nothing more to do with entries now, don't re-enter this function
      } else {
        one.side.cancelled   <- TRUE 
      }
    }
    output        <- list(initial.side, opposite.side, entries.are.cancelled, one.side.cancelled)
    names(output) <- c("initial.side", "opposite.side", "entries.are.cancelled", "one.side.cancelled")
    return(output)
  }
  ClearOrderBook <- function(orderbook.matrix) {
    # clears the order book for when a certain time period has passed
    #
    # args
    #   orderbook.matrix      matrix that has columns labeled "profit.price" and "order.qty"
    #
    # returns
    #   the orderbook.matrix with qty set to 0 if there is no profit order
    # -------------------
    for (i in 1:nrow(orderbook.matrix)) {
      ## checking for a profit order - if none, then set the qty to 0
      ## if a profit order exists, then the entry already traded and it doesn't matter
      if (is.na(orderbook.matrix[i, "profit.price"])) orderbook.matrix[i, "order.qty"] <- 0
    }
    return(orderbook.matrix)
  } 
  SortByTruncatedTimeStamp <- function(element, df, price.multiplier) {
    # takes in the trades dataframe and sorts it by bid prices and offer prices
    # 
    # args
    #  df                  the trades dataframe
    #  price.multiplier    price multiplier to convert 9301 to 93.01 (ex for CL prices)
    #
    # returns
    #  dataframe with replaced price column
    # --------------------------------------------------------
    # Occasionally, trades will come in out of order (lower bids will come in before higher bids, etc)
    # This causes issues when this is the first trade of a side, because the book will be set incorrectly
    # Method:
    #  - extract timestamps and truncate to 4 places instead of 6
    #  - make buy prices negative
    #  - order by timestamp and then price (increasing)
    #  - flip the buy prices back to positive
    ## ----------------------------------------------- 
    trunc.timestamps <- strptime(substr(rownames(df), 1, nchar(rownames(df)[1]) - 2), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
    FlipSignOfBidPrices <- function(row) ifelse(row["side"] == "B", as.numeric(row["price"]) * -1, as.numeric(row["price"]))      
    df[, "price"] <- apply(df, 1, FlipSignOfBidPrices) * price.multiplier
    #df <- df[with(df, order(trunc.timestamps, price)), ]
    df <- df[order(trunc.timestamps, df$price), ]
    
    df[, "price"] <- apply(df, 1, FlipSignOfBidPrices)  ## flip negative bid prices back to positive
    return(df)
  }
  CheckForVirtualStopOut <- function(x, row, cm.pos, cm.pnl, cm.return, stop.amt, commissions, trade.price, average.price, value.per.dollar, margin, max.position) {
    ## Virtual Stop-out    
    ## -------------------------------------------------------
    ## Based on the latest price, should I have been stopped out?
    ##  - calculate theoretical trade pnl if i exited at the latest price
    ##  - update theoretical cumulative PNL
    ##  - check if theo.cm.pnl is < stop amount
    ##  - if yes, exit position, update pnl, and break. if no, continue
    ## -------------------------------------------------------
    # Calculate Theoretical Trade PNL
    theo.trade.commish <- abs(cm.pos) * commissions
    if (loglevel > 2) cat("\nTheo Trade Commish (if i had to exit here) is", theo.trade.commish)
    theo.trade.pnl  <- theo.trade.commish + (cm.pos * trade.price - cm.pos * average.price) * value.per.dollar
    if (loglevel > 2) cat("\nTheo Trade PNL is", theo.trade.pnl)
    
    ## Calculate Return on Margin 
    ## - need to add commissions a second time since i'm only calculating
    ##    this on closing trades, so need the round trip commish not one-way
    ## - using log returns so they are additive, for cm.pnl
    theo.return.on.margin <- log( 1 + ((theo.trade.pnl + theo.trade.commish) / (margin * max.position)))
    if (loglevel > 2) cat("\nThe Return on Margin is", theo.return.on.margin)
    
    # Update Theoretical CM.PNL, theo.cm.returns & Check Stop Amount
    theo.cm.pnl    <- cm.pnl + theo.trade.pnl
    theo.cm.return <- cm.return + theo.return.on.margin
    if (theo.cm.pnl < stop.amt || row == nrow(x)) {  
      if (loglevel > 2) cat("\nEither last row or Virtual Stop-Out at row", row, "because", theo.cm.pnl, "<", stop.amt)
      trade.qty    <- cm.pos  
      trade.side   <- ifelse(cm.pos > 0, "S", "B")
      trade.action <- "CLOSE:"
      cm.pnl       <- theo.cm.pnl
      cm.return    <- theo.cm.return
      entry.pnl    <- NA
      order.number <- NA
      output        <- list(TRUE, trade.qty, trade.side, trade.action, cm.pnl, cm.return, entry.pnl, order.number, theo.return.on.margin)
      names(output) <- c("stopped.out", "trade.qty", "trade.side", "trade.action", "cm.pnl", "cm.return", "entry.pnl", "order.number", "theo.return.on.margin")
      return(output)
    } else {
      output        <- list(FALSE, theo.cm.pnl, theo.return.on.margin)
      names(output) <- c("stopped.out", "theo.cm.pnl", "theo.return.on.margin")
      return(output)
    }
  }
  ProcessTheListElement <- function(element, paramset.list, num.decimal.places, max.position, commissions, ...) {
    # Process a dataframe in a parameset list.  outputs the modified dataframe
    #
    # Args
    #   element               numeric.  this is the index value where the element is in the list.  
    #   paramset.list         list containing dataframes with trades in them
    #   num.decimal.places    (i think) used to convert from numeric to char
    #   max.position          used for return on margin calculations
    #   commissions           used internally for PNL calculations
    # 
    # Returns
    #   a modified dataframe
    # -----------------------------------------------------------------------------------
    if (loglevel > 1) cat("\n\n===========================")
    if (loglevel >= 1) cat("\nProcessing:", names(paramset.list)[element], file = logfile, append = TRUE)
    if (loglevel > 1) cat("\n\n===========================")
    
    ## Initialize some variables
    ## ------------------------------
    x             <- paramset.list[[element]]
    cm.pos        <- 0 
    cm.pnl        <- 0     
    average.price <- NULL
    cm.return     <- 0
    element.name  <- unlist(strsplit(names(paramset.list)[element], "[.]"))
    block         <- element.name[grep("block", element.name)]
    cancel.time   <- NULL
    
    ## Create a vector of timestamps and convert from char to POSIX
    timestamps    <- strptime(rownames(x), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
    if (any(is.na(timestamps))) stop("ERROR: unable to convert timestamps for ", names(paramset.list)[element])
    day           <- as.character(strptime(timestamps[1], format = "%Y-%m-%d"))
    
    ## Prep output dataframe
    new.dataframe            <- data.frame(matrix(nrow = nrow(x), ncol = 10), stringsAsFactors = FALSE)
    colnames(new.dataframe)  <- c(colnames(x), "return", "cm.return", "order.number", "entry.pnl")
    class(new.dataframe[, c("side", "product", "action", "price")]) <- "character"
    
    ## Create bid & offer dataframe to store entry prices and profit order prices
    bids <- cbind.data.frame(as.numeric(bid.sizes), as.character(rep(NA, length(bid.sizes))), as.character(rep(NA, length(bid.sizes))), stringsAsFactors = FALSE)
    colnames(bids)     <- c("order.qty", "order.price", "profit.price")
    offers <- cbind.data.frame(as.numeric(offer.sizes), as.character(rep(NA, length(offer.sizes))), as.character(rep(NA, length(offer.sizes))), stringsAsFactors = FALSE)
    colnames(offers)   <- c("order.qty", "order.price", "profit.price")
    opp.side.book.set  <- FALSE
    
    ## Get block number and set cancel entry and end.times if they exist
    if (!is.null(end.times.per.block)) {
      end.time <- as.character(end.times.per.block[charmatch(block, end.times.per.block[,1]), 2])
      end.time <- strptime(paste(day, end.time), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
    }
    if (!is.na(cancel.entry.mode)) {
      cancel.time <- as.character(cancel.entry.times[charmatch(block, cancel.entry.times[,1]), 2])
      cancel.time <- strptime(paste(day, cancel.time), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
      entries.are.cancelled <- FALSE
      one.side.cancelled    <- FALSE  
    }
    
    ## Process the dataframe
    ## ------------------------------
    x <- SortByTruncatedTimeStamp(element, x, price.multiplier)
    for (row in seq(nrow(x))) {
      if (loglevel > 1) cat("\n\n-------------------")
      if (loglevel > 1) cat("\nProcessing Row", row, "of element", names(paramset.list)[element])
      
      ## Initialize some variables
      ## ------------------------------
      trade.side    <- x[row, "side"]    #with(x, side[row])
      trade.price   <- x[row, "price"]   #with(x, price[row])
      trade.product <- x[row, "product"] #with(x, product[row]) 
      trade.qty     <- x[row, "qty"]     #with(x, qty[row])
      entry.pnl     <- NA
      if (is.null(average.price)) average.price <- as.numeric(trade.price)
      
      ## Cancel Entries:  check if
      ## - timer is expired
      ## - cancel mode is -1,0,1
      ## - entries are not yet cancelled
      ## -----------------------------------
      if (timestamps[row] > cancel.time && !is.na(cancel.entry.mode) && entries.are.cancelled == FALSE) { 
        orders <- CancelRemainingEntries(entries.are.cancelled, one.side.cancelled, element, cancel.entry.mode, cm.pos, 
                                         initial.side, init.side.name, opposite.side, 
                                         opp.side.name, keep.checking.for.flat)
        initial.side          <- orders$initial.side
        opposite.side         <- orders$opposite.side
        entries.are.cancelled <- orders$entries.are.cancelled
        one.side.cancelled    <- orders$one.side.cancelled
      }
      # ---------------------------------------------------------
      ## Three Cases Here:
      ## - strategy was stopped out or last row  --> clean up & exit
      ## - timer expired                         --> clean up & exit
      ## - process the trade as usual            --> clean up and continue
      # ---------------------------------------------------------
      # CASE 1:  Stopped out or last row
      stopped.out <- CheckForVirtualStopOut(x, row, cm.pos, cm.pnl, cm.return, 
                                            stop.amt, commissions, trade.price, 
                                            average.price, value.per.dollar, 
                                            margin, max.position)
      if (stopped.out[[1]]) {
        if (loglevel > 1) cat("\nExiting position from virtual stop-out")
        trade.qty    <- stopped.out$trade.qty
        trade.side   <- stopped.out$trade.side
        trade.action <- stopped.out$trade.action
        cm.pnl       <- stopped.out$cm.pnl
        cm.return    <- stopped.out$cm.return
        entry.pnl    <- stopped.out$entry.pnl
        order.number <- as.character(stopped.out$order.number)
        trade.return <- stopped.out$theo.return.on.margin
      } else {
        # these calculations are re-used in case2
        theo.cm.pnl  <- stopped.out$theo.cm.pnl
        trade.return <- stopped.out$theo.return.on.margin
      }
      # CASE 2:  Timer Expired (allows us to take 30min, 45min subsets of a 1hr backtester run)
      if (stopped.out[[1]] == FALSE && !is.null(end.times.per.block) && timestamps[row] > end.time) {
        if (loglevel > 1) cat("\nExiting position at end of timer")
        trade.qty     <- abs(cm.pos)  
        trade.side    <- ifelse(cm.pos > 0, "S", "B")
        trade.action  <- "CLOSE:"
        cm.pnl        <- theo.cm.pnl
        entry.pnl     <- "NA"
        order.number  <- "NA"
        timer.expired <- TRUE
      } else timer.expired <- FALSE
      
      # CASE 3:  Process The Row: Set Order Book, Set Qty, Process Trade Stats
      if (stopped.out[[1]] == FALSE && timer.expired == FALSE) {
        # -------------------------------------------------------
        # Setup the Virtual Order Book
        # - use first fill, intial offset, and order spacing
        #   to determine the strategy's order book
        # -------------------------------------------------------
        if (row == 1 && trade.side == "B") {
          initial.side      <- bids
          init.side.name    <- "bids"
          opposite.side     <- offers
          opp.side.name     <- "offers"
          offset.mult       <- (-1)
          profit.ticks.mult <- 1
          # Populate the "Initial Side" order book based on the first trade 
          initial.side <- InitializeOrderBook(as.numeric(trade.price), order.book = initial.side, multiplier = offset.mult)
        } else if (row == 1 && trade.side == "S") {
          initial.side      <- offers
          init.side.name    <- "offers"
          opposite.side     <- bids
          opp.side.name     <- "bids"
          offset.mult       <- 1
          profit.ticks.mult <- (-1)
          # Populate the "Initial Side" order book based on the first trade 
          initial.side <- InitializeOrderBook(as.numeric(trade.price), order.book = initial.side, multiplier = offset.mult)
        }
        # -------------------------------------------------------
        # Set the Virtual Order Quantity
        # -------------------------------------------------------
        MOP.output        <- MatchOrderPrice(element, trade.qty, trade.price, trade.side, init.side.name, initial.side, opposite.side, offset.mult, opp.side.name, opp.side.book.set)
        trade.qty         <- MOP.output$trade.qty
        order.side        <- MOP.output$order.side
        order.column      <- MOP.output$order.column
        opp.side.book.set <- MOP.output$opp.side.book.set
        initial.side      <- MOP.output$initial.side
        opposite.side     <- MOP.output$opposite.side
        order.number      <- MOP.output$order.number
        if (length(order.number) > 1) stop("ERROR: length of order.number is greater than 1 for trade.price", trade.price, "at row", row)
        ## Remove Traded Price
        if (order.side == "initial.side")   initial.side[order.number, order.column] <- NA
        if (order.side == "opposite.side") opposite.side[order.number, order.column] <- NA
        
        ## Set Profit Order Price 
        if (order.column == "order.price") {
          if (order.side == "initial.side")   initial.side[order.number, "profit.price"] <- CalcProfitOrderPrice(trade.side, order.number, trade.price, profit.ticks.mult, profit.ticks, min.price.increment)
          if (order.side == "opposite.side") opposite.side[order.number, "profit.price"] <- CalcProfitOrderPrice(trade.side, order.number, trade.price, profit.ticks.mult, profit.ticks, min.price.increment)
          #get(order.side)[order.number, "profit.price"] <- as.character(CalcProfitOrderPrice(trade.side, order.number, as.numeric(trade.price), profit.ticks.mult, profit.ticks, min.price.increment)
        }  
        if (loglevel > 2) cat("\nThese are the", init.side.name, "\n")
        if (loglevel > 2) print(initial.side)
        if (loglevel > 2) cat("\nThese are the", opp.side.name, "\n")
        if (loglevel > 2) print(opposite.side)
        
        # -------------------------------------------------------
        # Process the Trade
        # -------------------------------------------------------
        trade.output     <- ProcessTheTrade(element, cm.return, cm.pnl, trade.price, trade.side, trade.qty, commissions, cm.pos, margin, max.position, average.price, value.per.dollar, row)
        cm.pos           <- trade.output$cm.pos
        cm.pnl           <- trade.output$cm.pnl
        cm.return        <- trade.output$cm.return
        trade.action     <- trade.output$trade.action
        trade.pnl        <- trade.output$trade.pnl
        return.on.margin <- trade.output$return.on.margin
        average.price    <- trade.output$average.price
        entry.pnl        <- trade.pnl
      }
      
      # ---------------------------------------------------------
      ## Strategy Accounting
      # ---------------------------------------------------------
      new.dataframe[row, ] <- CreateOutputRow(trade.side, trade.qty, 
                                              trade.product, trade.price, 
                                              cm.pnl, trade.action, trade.return, 
                                              cm.return, order.number, entry.pnl)
      # end row-by-row processing
      if (any(stopped.out[[1]], timer.expired, row == nrow(x))) break
    }
    # trim dataframe down to size and return it
    row.names(new.dataframe) <- row.names(x)
    new.dataframe <- new.dataframe[1:row, ]
    return(new.dataframe)
  }
  ## END Function List 
  ## ----------------
  
  ## Process the paramset.list
  ## - pass in a sequence of element indexes, like 1,2,3,4,5...
  ## - pass in the source paramset list
  ## - the function uses the index to get the name and element from the source list
  ## ------------------------------
  num.decimal.places <- FindNumberOfDecimalPlaces(min.price.increment)
  max.position       <- max(sum(bid.sizes), sum(offer.sizes))  # used for return on margin calcs
  commissions        <- commissions * -1  # easier to calculate if this is negative
  element.index      <- seq_along(paramset.list) # the index number of the elements in paramset.list
  if (doParallel) {
    # Parallel Method 1
    library(parallel)
    library(foreach)
    if (Sys.info()['sysname'] == "Windows")
    {
      # install.packages("doParallel")
      library(doParallel)
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      output <- parLapply(cl, X = element.index, fun = function(element) ProcessTheListElement(element, paramset.list, num.decimal.places, max.position, commissions))
    } else {
      stop("ERROR: Haven't tested parallel processing with non-windows OS yet!")
      # install.packages("doMC")
      # library(doMC)
      # registerDoMC(cores = detectCores())
    }
    stopCluster(cl)
    ## Parallel Method2
    #  source("./Scripts/ITG/MCLapplyHackForWindows.R")
    #  output <- mclapply(X=element.index, fun= function(element) ProcessTheListElement(element, paramset.list, num.decimal.places, max.position, commissions))
  } else {
    output <- lapply(element.index, FUN = function(element) ProcessTheListElement(element, paramset.list, num.decimal.places, max.position, commissions))
  }
  names(output)      <- names(paramset.list)  
  return(output)
}

## Remove after 9/1/2015
# ModifyTradeQuantities <- function(paramset.list, commissions=.50, stop.amt=-1500, price.multiplier=.01,
#                                   value.per.dollar=1000, min.price.increment = .01, bid.sizes=rep(1, 15), 
#                                   offer.sizes=rep(1,15), profit.ticks=5, order.spacing=1, initial.offset=1, end.times.per.block = NULL,
#                                   cancel.entry.mode=NA, cancel.entry.times=NULL, margin=4250, keep.checking.for.flat=TRUE,
#                                   logfile="", modify.qty=NULL, doParallel = FALSE,
#                                   loglevel=1) {
#   # Takes in a list of trades and desired bid & offer quantities, modifies the trades
#   # and spits out a list with modified trade sizes. format is the same as a regular parmset.list
#   #
#   # Args
#   #   paramset.list           character name of a list that contains trades
#   #   commissions             commissions rate per-side, NOT roundtrip
#   #   stop.amt                when to stop out, trades beyond this point are discarded
#   #   price.multiplier        the correct multiplier to get minimum value, eg .01 for CL, .0001 for currencies, etc
#   #   value.per.dollar        once multiplied, the price value for each dollar move
#   #                           eg 1000 for CL, 50 for ES, etc
#   #   min.price.increment     the change in price from one tick the next, eg .01 for CL, .25 for ES, etc
#   #   bid.sizes               numeric vector of bid sizes, with [1] being closest to the market, etc
#   #   offer.sizes             numeric vector of offer sizes, with [1] being closest to the market, etc
#   #   profit.ticks            where the profit orders would go in relation to the entry order
#   #   order.spacing           spacing for orders.  1 = every price, 2 = every other price, etc
#   #   intial offset           0 = at the best bid/offer, 1 means 1 tick back, 2 = 2 ticks back, etc
#   #   end.times.per.block     dataframe with block names in col 1 and end times in col 2.  like: block01, 06:45:00
#   #   cancel.entry.mode       modifies behavior after cancel entry time has passed. 
#   #                           0 = cancel all entry orders, 1 = cancel all entries on the same side as the position, 
#   #                           -1 = cancel orders on the opposite side of the position
#   #   cancel.entry.times      dataframe with block names in col 1 and entry order cancel times in col 2. like: block01, 06:45:00
#   #   keep.checking.for.flat  if using cancel entry mode and this is TRUE, all entries are cancelled once the position is flat and the timer has ended
#   #   margin                  margin of the product.  used to calculate return on margin and cumulative return on margin
#   #   loglevel                1 = no logging, 2 = qty, pos updates, 3 = print order book at each row
#   #
#   # Returns
#   #   list of dataframes with modified quantities and cumulative PNL
#   # -------------------------------------------------------------------------
#   ## Trap For Errors
#   ## ----------------
#   if (is.character(paramset.list)) {
#     paramset.list <- get(paramset.list)
#   } 
#   if (is.null(names(paramset.list))) {
#     stop("ERROR: this list needs to have named elements")
#   } 
#   if (stop.amt >= 0) {
#     stop("ERROR: stop amount needs to be negative")
#   } 
#   if (any(c(commissions, price.multiplier, value.per.dollar, bid.sizes, initial.offset, offer.sizes) < 0)) {
#     stop("ERROR: all arguments besides stop.amt require >=0")
#   } 
#   if (!is.list(paramset.list)) {
#     stop("ERROR: paramset.list needs to be a list")
#   } 
#   if (order.spacing <= 0) {
#     stop("ERROR: order spacing must be 1 or greater")
#   }
#   if (profit.ticks <= 0) {
#     stop("ERROR: profit ticks must be 1 or greater")
#   }
#   if (!is.null(end.times.per.block) && !is.data.frame(end.times.per.block)) {
#     stop("ERROR: end.times.per.block must be a dataframe")
#   } 
#   if (!is.null(cancel.entry.times) && !is.data.frame(cancel.entry.times)) {
#     stop("ERROR: cancel.entry.times must be a dataframe")
#   }
#   if (ncol(paramset.list[[1]]) > 6) {
#     stop("ERROR: this function has already been run on this parameter set! no need to re-run")
#   }
#   if (cancel.entry.mode %in% c(-1,0,1) && is.null(cancel.entry.times)) {
#     stop("ERROR: cancel entry mode is set but there are no cancel times")
#   }
#   stopifnot(cancel.entry.mode %in% c(NA,0,1,-1))
#   
#   ## Functions
#   ## ----------------
#   CalcProfitOrderPrice <- function(trade.side, order.number, trade.price, profit.ticks.mult, profit.ticks, min.price.increment) {
#     profit.ticks.mult    <- ifelse(trade.side == "B", 1, -1)  # set multiplier based on trade side
#     output <- format(trade.price + (profit.ticks.mult * profit.ticks * min.price.increment), nsmall = num.decimal.places)
#     return(output)
#   }
#   MatchOrderPrice <- function(trade.price, trade.side, init.side.name, initial.side, opposite.side, opp.side.name=NULL, opp.side.book.set=NULL) {
#     # looks up the trade price and returns the virtual trade qty
#     #
#     # returns
#     #   a list with trade.qty, order.side, order.column, order.number, opp.side.book.set
#     # -------------------------------------------------------
#     ## Variables
#     #browser()
#     trade.price          <- format(trade.price, nsmall = num.decimal.places)
#     unmodified.trade.qty <- trade.qty ## for debugging
#     order.number         <- NA
#     traded.entry.order   <- FALSE
#     
#     ## Set up 2 cases:  match against either entry orders or profit orders
#     case1  <- (trade.side == "B" && init.side.name == "bids")   || (trade.side == "S" && init.side.name == "offers")  
#     case2  <- (trade.side == "B" && init.side.name == "offers") || (trade.side == "S" && init.side.name == "bids")    
#     
#     ## Match Profit Targets
#     if (case1 && opp.side.book.set) {
#       # order.number <- Match.numeric(trade.price, table = opposite.side[, "profit.price"])
#       order.number <- grep(paste0(trade.price, "$"), opposite.side[, "profit.price"])
#       order.side   <- "opposite.side"
#     } else if (case2) {
#       #order.number <- Match.numeric(trade.price, table = initial.side[, "profit.price"])
#       order.number <- grep(paste0(trade.price, "$"), initial.side[, "profit.price"])
#       order.side   <- "initial.side"
#     } 
#     
#     # Failure to match with grep results in "integer(0)", so reset for matching against entries
#     #if (row == 20) browser()
#     order.number <- ifelse(length(order.number) == 0, NA, 
#                            ifelse(length(order.number) == 1, order.number, 
#                                   browser()))
#     #stop("ERROR: multiple matches for order number")))  
#     
#     ## Match Entries & Set Opposite Side Book if needed
#     if (case1 && is.na(order.number)) {
#       #order.number <- Match.numeric(trade.price, table = initial.side[, "order.price"])
#       order.number <- grep(paste0(trade.price, "$"), initial.side[, "order.price"])            
#       order.side   <- "initial.side"
#       traded.entry.order <- TRUE
#     } 
#     if (case2 && is.na(order.number)) {
#       if (opp.side.book.set == FALSE) {
#         opposite.side <- InitializeOrderBook(trade.price, 
#                                              order.book = opposite.side, 
#                                              multiplier = -offset.mult)  
#         opp.side.book.set <- TRUE          
#       }
#       # order.number <- Match.numeric(trade.price, table = opposite.side[, "order.price"])
#       order.number <- grep(paste0(trade.price, "$"), opposite.side[, "order.price"])        
#       order.side   <- "opposite.side"
#       traded.entry.order <- TRUE
#     }
#     if (!is.numeric(order.number)) {  # order number did not get set
#       if (loglevel > 1) {
#         cat("\nERROR: Did not successfully match and modify the trade qty in row", row, "of", names(paramset.list)[element])
#         cat("\n\n************************************")
#         cat("\nThis is the trade price:", trade.price)
#         cat("\nThis is the trade side:", trade.side)
#         cat("\nThese are the", init.side.name, "\n")
#         print(initial.side)
#         cat("\nThese are the", opp.side.name, "\n")
#         print(opposite.side)
#       }
#       stop("ERROR: Did not successfully modify the trade qty in row ", row, " of ", names(paramset.list)[element])          
#     }
#     ## Determine Trade Qty
#     order.column <- ifelse(traded.entry.order, "order.price", "profit.price")
#     trade.qty <- ifelse(order.side == "initial.side", initial.side[order.number, "order.qty"], opposite.side[order.number, "order.qty"])
#     if (is.na(trade.qty)) stop("ERROR: trade.qty was not properly set at row", row, "of ", names(paramset.list)[element])
#     
#     ## Output
#     output <- list(trade.qty, order.side, order.column, order.number, opp.side.book.set, initial.side, opposite.side)
#     names(output) <- c("trade.qty", "order.side", "order.column", "order.number", "opp.side.book.set", "initial.side", "opposite.side")
#     return(output)
#   }
#   ProcessTheTrade <- function(trade.price, trade.side, trade.qty, commissions, cm.pos, margin, max.position, average.price, value.per.dollar, row) {
#     # process the trade and output PNL
#     #
#     # 5 cases
#     # - nothing happend - order qty was 0
#     # - flip a position (eg long to short)
#     # - closed a position (position = 0)
#     # - add to a position
#     # - reduce a position
#     # --------------------------------------
#     ## Set sign for trade qty (helps standardize calculations)
#     if (trade.side == "S") trade.qty <- (-1) * trade.qty
#     trade.commish <- abs(trade.qty) * commissions
#     new.cm.pos    <- cm.pos + trade.qty
#     if (loglevel > 1) cat("\nNew CmPos for row", row, "is", new.cm.pos)
#     
#     # Case1: Order qty was 0, continue on to next row
#     if (trade.qty == 0) {
#       trade.pnl        <- 0
#       return.on.margin <- 0
#       if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#       if (loglevel > 1) cat("\nSynthetic Order Qty is 0, so everything stays the same")
#       
#       # Case2: Position Flipped from long to short, or vice versa.  there must be a simpler way to code this...
#     } else if (cm.pos > 0 && new.cm.pos < 0 || cm.pos < 0 && new.cm.pos > 0 ) {  
#       trade.pnl        <- trade.commish + (cm.pos * trade.price - cm.pos * average.price) * value.per.dollar
#       return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
#       average.price    <- trade.price
#       if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#       if (loglevel > 1) cat("\nFlipped Position, New Average Price is", average.price, "for row", row)
#       
#       # Case3: Position Closed
#     } else if (new.cm.pos == 0) {       
#       trade.pnl        <- trade.commish + (trade.qty * average.price - trade.qty * trade.price) * value.per.dollar
#       return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
#       average.price    <- 0
#       if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#       if (loglevel > 1) cat("\nClosed Position, new Average Price is", average.price, "for row", row)
#       
#       # Case4: Added to an existing position (either long or short)
#     } else if (abs(new.cm.pos) > abs(cm.pos)) {  
#       trade.pnl        <- trade.commish
#       return.on.margin <- 0
#       average.price <- (trade.qty * trade.price + cm.pos * average.price) / new.cm.pos
#       if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#       if (loglevel > 1) cat("\nAdded to Position, new Average Price is", average.price, "for row", row)
#       
#       # Case5: Reduced an existing position (either long or short)  
#     } else if (abs(new.cm.pos) < abs(cm.pos)) {  
#       trade.pnl        <- trade.commish + (trade.qty * average.price - trade.qty * trade.price) * value.per.dollar
#       return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
#       if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#       if (loglevel > 1) cat("\nAverage Price stays the same because i reduced position:", average.price)
#       
#     } else stop("ERROR: issue with PNL calc at row ", row, " in ", names(paramset.list)[element])
#     cm.pos    <- new.cm.pos
#     cm.pnl    <- cm.pnl + trade.pnl
#     cm.return <- cm.return + return.on.margin
#     if (loglevel > 1) cat("\nCmPos for row", row, "is", cm.pos)
#     if (loglevel > 1) cat("\nCmPNL for row", row, "is", cm.pnl)
#     if (loglevel > 1) cat("\nCmReturn for row", row, "is", cm.return)
#     trade.action <- ifelse(cm.pos == 0, "CLOSE:", "OPEN:")
#     
#     ## output 
#     output <- list(cm.pos, cm.pnl, cm.return, trade.action, trade.pnl, return.on.margin, trade.side, average.price)
#     names(output) <- c("cm.pos", "cm.pnl", "cm.return", "trade.action", "trade.pnl", "return.on.margin", "trade.side", "average.price")
#     return(output)
#   }
#   InitializeOrderBook <- function(trade.price, order.book, multiplier) {
#     # populate the order book based on the first trade price
#     #
#     # args
#     #   trade.price     this is the first trade
#     #   order.book      either the initial or opposite book
#     #   multiplier      1 * offset multiplier if initial side, -1 if opposite side
#     #
#     # return
#     #   order book matrix with prices 
#     # -----------------------------------------
#     order.book[1, "order.price"] <- format(trade.price, nsmall = num.decimal.places)
#     for (i in 2:nrow(order.book)) {
#       new.order <- as.numeric(order.book[i - 1, "order.price"]) + (multiplier * order.spacing * min.price.increment)
#       order.book[i, "order.price"] <- format(new.order, nsmall = num.decimal.places)
#     }
#     return(order.book)
#   }
#   CreateOutputRow <- function(trade.side, trade.qty, trade.product, trade.price, cm.pnl, trade.action, trade.return, cm.return, order.number, entry.pnl, row.name) {
#     # writes values to the output dataframe 
#     #
#     # args
#     #
#     # returns
#     #   a 1-row dataframe to save to the output dataframe
#     # ------------------------------------------------
#     if (trade.side == "S") order.number <- as.numeric(order.number) * -1  # track which entry was closed.  negative for bids, positive for offer entries
#     #browser()
#     output <- as.data.frame(list(trade.side, trade.qty, trade.product, format(trade.price, nsmall = num.decimal.places), cm.pnl, trade.action, trade.return, cm.return, order.number, entry.pnl), stringsAsFactors = FALSE)
#     row.names(output)[1]  <- row.name
#     return(output)
#   }
#   CancelRemainingEntries <- function(cancel.entry.mode, cm.pos, initial.side, init.side.name, opposite.side, opp.side.name, keep.checking.for.flat) {
#     # after the timer expires, runs the ClearOrderBook function depending on the cancel entry mode
#     #
#     # args
#     #   cancel.entry.mode       either -1, 0, 1 (-1 cancels opposite side, 0 cancels both, 1 cancels same side)
#     #   cm.pos                  cumulative position when timer expires
#     #   initial.side            a matrix representing the order book of entries and profit orders
#     #   init.side.name          either "bids" or "offers"
#     #   opposite.side           a matrix representing the order book of entries and profit orders
#     #   opp.side.name           either "bids" or "offers"
#     #   keep.checking.for.flat  T or F, whether or not to cancel orders after the position flattens
#     # returns
#     #   a list containing the two order matrices
#     # -----------------------------------------------------------------------------------
#     ## Figure out what the position is and which side to cancel based on the cancel mode
#     if (cancel.entry.mode == 0 || cm.pos == 0) {
#       if (loglevel > 2) cat("\nCancelling all bids and offers - either flat or cancel mode is 0")
#       initial.side  <- ClearOrderBook(initial.side)
#       opposite.side <- ClearOrderBook(opposite.side)
#     } else if (cancel.entry.mode == -1 && cm.pos > 0) {
#       if (loglevel > 2) cat("\nCancelling all offers - I'm long and cancel mode is -1")
#       if (init.side.name == "offers")     initial.side  <- ClearOrderBook(initial.side)
#       else if (opp.side.name == "offers") opposite.side <- ClearOrderBook(opposite.side)
#       else stop("ERROR: can't figure out which side of the book to clear out")
#     } else if (cancel.entry.mode == -1 && cm.pos < 0) {
#       if (loglevel > 2) cat("\nCancelling all bids - I'm short and cancel mode is -1")
#       if (init.side.name == "bids")       initial.side  <- ClearOrderBook(initial.side)
#       else if (opp.side.name == "bids")   opposite.side <- ClearOrderBook(opposite.side)
#       else stop("ERROR: can't figure out which side of the book to clear out")    
#     } else if (cancel.entry.mode ==  1 && cm.pos > 0) {
#       if (loglevel > 2) cat("\nCancelling all bids - I'm long and cancel mode is 1")
#       if (init.side.name == "bids")       initial.side  <- ClearOrderBook(initial.side)
#       else if (opp.side.name == "bids")   opposite.side <- ClearOrderBook(opposite.side)
#       else stop("ERROR: can't figure out which side of the book to clear out")            
#     } else if (cancel.entry.mode ==  1 && cm.pos < 0) {
#       if (loglevel > 2) cat("\nCancelling all offers - I'm short and cancel mode is 1")
#       if (init.side.name == "offers")     initial.side  <- ClearOrderBook(initial.side)
#       else if (opp.side.name == "offers") opposite.side <- ClearOrderBook(opposite.side)
#       else stop("ERROR: can't figure out which side of the book to clear out")
#     } else stop("ERROR, trying to cancel entries at ", row, "of ", names(paramset.list)[element], "but something is screwed up")
#     output        <- list(initial.side, opposite.side)
#     names(output) <- c("initial.side", "opposite.side")
#     return(output)
#   }
#   ClearOrderBook <- function(orderbook.matrix) {
#     # clears the order book for when a certain time period has passed
#     #
#     # args
#     #   orderbook.matrix      matrix that has columns labeled "profit.price" and "order.qty"
#     #
#     # returns
#     #   the orderbook.matrix with qty set to 0 if there is no profit order
#     # -------------------
#     for (i in 1:nrow(orderbook.matrix)) {
#       ## checking for a profit order - if none, then set the qty to 0
#       ## if a profit order exists, then the entry already traded and it doesn't matter
#       if (is.na(orderbook.matrix[i, "profit.price"])) orderbook.matrix[i, "order.qty"] <- 0
#     }
#     return(orderbook.matrix)
#   } 
#   SortByTruncatedTimeStamp <- function(df, price.multiplier) {
#     # takes in the trades dataframe and sorts it by bid prices and offer prices
#     # 
#     # args
#     #  df                  the trades dataframe
#     #  price.multiplier    price multiplier to convert 9301 to 93.01 (ex for CL prices)
#     #
#     # returns
#     #  dataframe with replaced price column
#     # --------------------------------------------------------
#     # Occasionally, trades will come in out of order (lower bids will come in before higher bids, etc)
#     # This causes issues when this is the first trade of a side, because the book will be set incorrectly
#     # Method:
#     #  - extract timestamps and truncate to 4 places instead of 6
#     #  - make buy prices negative
#     #  - order by timestamp and then price (increasing)
#     #  - flip the buy prices back to positive
#     ## ----------------------------------------------- 
#     trunc.timestamps <- strptime(substr(rownames(df), 1, nchar(rownames(df)[1]) - 2), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#     FlipSignOfBidPrices <- function(row) ifelse(row["side"] == "B", as.numeric(row["price"]) * -1, as.numeric(row["price"]))      
#     df[, "price"] <- apply(df, 1, FlipSignOfBidPrices) * price.multiplier
#     df <- df[with(df, order(trunc.timestamps, price)), ]
#     df[, "price"] <- apply(df, 1, FlipSignOfBidPrices)  ## flip negative bid prices back to positive
#     return(df)
#   }
#   
#   
#   ## Set Up Parallel Processing
#   ## ------------------------------
# #   if (doParallel) {
# #     require(parallel)
# #     cl <- makeCluster(detectCores())
# #     #   library(parallel)
# #     #   library(foreach)
# #     #   if (Sys.info()['sysname'] == "Windows")
# #     #   {
# #     #     # install.packages("doParallel")
# #     #     library(doParallel)
# #     #     registerDoParallel(cores = detectCores())
# #     #   } else {
# #     #     stop("ERROR: Haven't tested parallel processing with non-windows OS yet!")
# #     #     # install.packages("doMC")
# #     #     # library(doMC)
# #     #     # registerDoMC(cores = detectCores())
# #     #   }
# #   }
#   
#   ## Process each trades element in the paramset.list, store in the output object
#   ## ------------------------------
#   my.output <- list()
#   num.decimal.places <- FindNumberOfDecimalPlaces(min.price.increment)
#   max.position <- max(sum(bid.sizes), sum(offer.sizes))
#   commissions  <- commissions * -1  # easier to calculate if this is negative
#   # browser()
#   # element <- 1
#   #foreach(element = seq(paramset.list)) %dopar%  {
#   for (element in seq(names(paramset.list))) {
#     if (loglevel > 1) cat("\n\n===========================")
#     if (loglevel >= 1) cat("\nProcessing:", names(paramset.list)[element], file = logfile, append = TRUE)
#     if (loglevel > 1) cat("\n\n===========================")
#     
#     ## Initialize some variables
#     ## ------------------------------
#     x             <- paramset.list[[element]]
#     #x$char.price  <- x$price
#     cm.pos        <- 0 
#     cm.pnl        <- 0     
#     average.price <- NULL
#     cm.return     <- 0
#     element.name  <- unlist(strsplit(names(paramset.list)[element], "[.]"))
#     block         <- element.name[grep("block", element.name)]
#     cancel.time   <- NULL
#     ## Create a vector of timestamps and convert from char to POSIX
#     timestamps    <- strptime(rownames(x), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#     if (any(is.na(timestamps))) stop("ERROR: unable to convert timestamps for ", names(paramset.list)[element])
#     day           <- as.character(strptime(timestamps[1], format = "%Y-%m-%d"))
#     ## Store modified trades in new.dataframe
#     new.dataframe            <- data.frame(matrix(nrow = nrow(x), ncol = 10), stringsAsFactors = FALSE)
#     colnames(new.dataframe) <- c(colnames(x), "return", "cm.return", "order.number", "entry.pnl")
#     class(new.dataframe[, c("side", "product", "action", "price")]) <- "character"
#     #class(new.dataframe[, c("qty", "cm.pnl", "cm.return")])         <- "numeric"
#     ## Create bid & offer matrices to store entry prices and profit order prices
#     bids <- cbind.data.frame(as.numeric(bid.sizes), as.character(rep(NA, length(bid.sizes))), as.character(rep(NA, length(bid.sizes))), stringsAsFactors = FALSE)
#     colnames(bids)     <- c("order.qty", "order.price", "profit.price")
#     offers <- cbind.data.frame(as.numeric(offer.sizes), as.character(rep(NA, length(offer.sizes))), as.character(rep(NA, length(offer.sizes))), stringsAsFactors = FALSE)
#     colnames(offers)   <- c("order.qty", "order.price", "profit.price")
#     opp.side.book.set  <- FALSE
#     ## Get block number and set cancel entry and end.times if they exist
#     if (!is.null(end.times.per.block)) {
#       end.time <- as.character(end.times.per.block[charmatch(block, end.times.per.block[,1]), 2])
#       end.time <- strptime(paste(day, end.time), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#     }
#     if (!is.na(cancel.entry.mode)) {
#       cancel.time <- as.character(cancel.entry.times[charmatch(block, cancel.entry.times[,1]), 2])
#       cancel.time <- strptime(paste(day, cancel.time), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#       entries.are.cancelled <- FALSE
#     }
#     
#     ## Process the dataframe
#     ## ------------------------------
#     x <- SortByTruncatedTimeStamp(x, price.multiplier)
#     for (row in seq(nrow(x))) {
#       # row <- 1
#       if (loglevel > 1) cat("\n\n-------------------")
#       if (loglevel > 1) cat("\nProcessing Row", row, "of element", names(paramset.list)[element])
#       
#       ## Initialize some variables
#       ## ------------------------------
#       trade.side    <- with(x, side[row])
#       #trade.price   <- as.character(with(x, price[row]))  # use character to get around floating point issues
#       trade.price   <- with(x, price[row])
#       trade.product <- with(x, product[row]) 
#       trade.qty     <- with(x, qty[row])
#       entry.pnl     <- NA
#       if (is.null(average.price)) average.price <- as.numeric(trade.price)
#       
#       ## Cancel Entries if the timer has expired
#       ## -------------------------------------------------------
#       if (timestamps[row] > cancel.time && !is.na(cancel.entry.mode) && entries.are.cancelled == FALSE) {  
#         # mode is (-1,0,1) and entries are not yet cancelled, and timer is expired
#         if (loglevel > 1) cat("\nCancel Entries time has passed, cancelling entries now")
#         orders <- CancelRemainingEntries(cancel.entry.mode, cm.pos, initial.side, init.side.name, opposite.side, opp.side.name, keep.checking.for.flat)
#         initial.side  <- orders[[1]]
#         opposite.side <- orders[[2]]
#         if (keep.checking.for.flat == FALSE) entries.are.cancelled <- TRUE  ## to allow some new positions after timer expires  
#       }
#       
#       ## Virtual Stop-out    
#       ## -------------------------------------------------------
#       
#       ## Based on the latest price, should I have been stopped out?
#       ##  - calculate theoretical trade pnl if i exited at the latest price
#       ##  - update theoretical cumulative PNL
#       ##  - check if theo.cm.pnl is < stop amount
#       ##  - if yes, exit position, update pnl, and break. if no, continue
#       ## -----------------------------------------
#       # Calculate Theoretical Trade PNL
#       theo.trade.commish <- abs(cm.pos) * commissions
#       if (loglevel > 2) cat("\nTheo Trade Commish (if i had to exit here) is", theo.trade.commish)
#       theo.trade.pnl  <- theo.trade.commish + (cm.pos * trade.price - cm.pos * average.price) * value.per.dollar
#       if (loglevel > 2) cat("\nTheo Trade PNL is", theo.trade.pnl)
#       ## Calculate Return on Margin 
#       ## - need to add commissions a second time since i'm only calculating
#       ##    this on closing trades, so need the round trip commish not one-way
#       ## - using log returns so they are additive, for cm.pnl
#       theo.return.on.margin <- log( 1 + ((theo.trade.pnl + theo.trade.commish) / (margin * max.position)))
#       if (loglevel > 2) cat("\nThe Return on Margin is", theo.return.on.margin)
#       
#       # Update Theoretical CM.PNL, theo.cm.returns & Check Stop Amount
#       theo.cm.pnl    <- cm.pnl + theo.trade.pnl
#       theo.cm.return <- cm.return + theo.return.on.margin
#       if (theo.cm.pnl < stop.amt || row == nrow(x)) {  # stopped out or last row?
#         if (loglevel > 1) {
#           if (theo.cm.pnl < stop.amt) cat("\nVirtual Stop-Out at row", row, "because", theo.cm.pnl, "<", stop.amt)
#           if (row == nrow(x))         cat("\nExiting position at last row")
#         } 
#         trade.qty    <- cm.pos  
#         trade.side   <- ifelse(cm.pos > 0, "S", "B")
#         trade.action <- "CLOSE:"
#         cm.pnl       <- theo.cm.pnl
#         cm.return    <- theo.cm.return
#         entry.pnl    <- NA
#         order.number <- NA
#         
#         ## SET ENTRY PNL AND ORDER NUMBER
#         ## figure out which entry orders closed when the position exited
#         ## three cases:
#         ## - long position - find all the rows with profit.prices in the bids matrix
#         ## - short position - find all the rows with profit.prices in the offers matrix
#         ## - no position (?) - set to 0
#         #if (cm.pos > 0 && init.side.name == "bids") {
#         #  order.number <- (-1 * which(!is.na(initial.side[,"profit.price"])))
#         #  entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")        
#         #} else if (cm.pos > 0 && opp.side.name == "bids") {
#         #  order.number <- (-1 * which(!is.na(opposite.side[,"profit.price"])))
#         #  entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         #} else if (cm.pos < 0 && init.side.name == "offers") {
#         #  order.number <- which(!is.na(initial.side[,"profit.price"]))
#         #  entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         #} else if (cm.pos < 0 && opp.side.name == "offers") {
#         #  order.number <- which(!is.na(opposite.side[,"profit.price"]))
#         #  entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")          
#         #} else order.number <- "0"
#         
#         ## Strategy Accounting        
#         # if (row == 30 || row == 31) browser()
#         new.dataframe[row, ] <- CreateOutputRow(trade.side, trade.qty, 
#                                                 trade.product, as.numeric(trade.price), 
#                                                 cm.pnl, trade.action, 
#                                                 trade.return = theo.return.on.margin, 
#                                                 cm.return, order.number, entry.pnl,
#                                                 row.name = row.names(x)[row])
#         my.output[[element]] <- new.dataframe[1:row, ]
#         names(my.output)[[element]] <- names(paramset.list)[element]
#         break  # Theoretical exit, move on to next element
#       }
#       
#       ## -------------------------------------------------------
#       ## Check Exit Time
#       ## - Based on the supplied end-times, should I exit?
#       ## -------------------------------------------------------
#       if (!is.null(end.times.per.block) && timestamps[row] > end.time) {
#         ## timer enabled and timer expired
#         if (loglevel > 1) cat("\nExiting position at end of timer")
#         trade.qty    <- cm.pos  
#         trade.side   <- ifelse(cm.pos > 0, "S", "B")
#         trade.action <- "CLOSE:"
#         cm.pnl       <- theo.cm.pnl
#         entry.pnl    <- NA
#         order.number <- NA
#         
#         ## SET ENTRY PNL AND ORDER NUMBER
#         ## figure out which entry orders closed when the position exited
#         ## three cases:
#         ## - long position - find all the rows with profit.prices in the bids matrix
#         ## - short position - find all the rows with profit.prices in the offers matrix
#         ## - no position (?) - set to 0
#         #if (cm.pos > 0 && init.side.name == "bids") {
#         #  order.number <- (-1 * which(!is.na(initial.side[,"profit.price"])))
#         #  entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         #} else if (cm.pos > 0 && opp.side.name == "bids") {
#         #  order.number <- (-1 * which(!is.na(opposite.side[,"profit.price"])))
#         #  entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         #} else if (cm.pos < 0 && init.side.name == "offers") {
#         #  order.number <- which(!is.na(initial.side[,"profit.price"]))
#         #  entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         #} else if (cm.pos < 0 && opp.side.name == "offers") {
#         #  order.number <- which(!is.na(opposite.side[,"profit.price"]))
#         #  entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         #} else order.number <- "0"
#         # order.number = paste(order.number, collapse = ",")
#         
#         ## Strategy Accounting    
#         #browser()
#         new.dataframe[row, ] <- CreateOutputRow(trade.side, trade.qty, 
#                                                 trade.product, as.numeric(trade.price), 
#                                                 cm.pnl, trade.action, 
#                                                 trade.return = theo.return.on.margin, 
#                                                 cm.return, order.number, entry.pnl,
#                                                 row.name = row.names(x)[row])
#         my.output[[element]] <- new.dataframe[1:row, ]
#         names(my.output)[[element]] <- names(paramset.list)[element]
#         break   # Theoretical exit, move on to next element
#       }        
#       
#       # -------------------------------------------------------
#       # Setup the Virtual Order Book
#       # - use first fill, intial offset, and order spacing
#       #   to determine the strategy's order book
#       # -------------------------------------------------------
#       if (row == 1 && trade.side == "B") {
#         initial.side      <- bids
#         init.side.name    <- "bids"
#         opposite.side     <- offers
#         opp.side.name     <- "offers"
#         offset.mult       <- (-1)
#         profit.ticks.mult <- 1
#         # Populate the "Initial Side" order book based on the first trade 
#         initial.side <- InitializeOrderBook(as.numeric(trade.price), order.book = initial.side, multiplier = offset.mult)
#       } else if (row == 1 && trade.side == "S") {
#         initial.side      <- offers
#         init.side.name    <- "offers"
#         opposite.side     <- bids
#         opp.side.name     <- "bids"
#         offset.mult       <- 1
#         profit.ticks.mult <- (-1)
#         # Populate the "Initial Side" order book based on the first trade 
#         initial.side <- InitializeOrderBook(as.numeric(trade.price), order.book = initial.side, multiplier = offset.mult)
#       }
#       
#       # -------------------------------------------------------
#       # Set the Virtual Order Quantity
#       # -------------------------------------------------------
#       #browser()
#       output <- MatchOrderPrice(trade.price, trade.side, init.side.name, initial.side, opposite.side, opp.side.name, opp.side.book.set)
#       trade.qty         <- output$trade.qty
#       order.side        <- output$order.side
#       order.column      <- output$order.column
#       order.number      <- output$order.number
#       if (length(order.number) > 1) stop("ERROR: length of order.number is greater than 1 for trade.price", trade.price, "at row", row)
#       opp.side.book.set <- output$opp.side.book.set
#       initial.side      <- output$initial.side
#       opposite.side     <- output$opposite.side
#       
#       ## Remove Traded Price
#       if (order.side == "initial.side")   initial.side[order.number, order.column] <- NA
#       if (order.side == "opposite.side") opposite.side[order.number, order.column] <- NA
#       
#       ## Set Profit Order Price 
#       if (order.column == "order.price") {
#         if (order.side == "initial.side")   initial.side[order.number, "profit.price"] <- CalcProfitOrderPrice(trade.side, order.number, trade.price, profit.ticks.mult, profit.ticks, min.price.increment)
#         if (order.side == "opposite.side") opposite.side[order.number, "profit.price"] <- CalcProfitOrderPrice(trade.side, order.number, trade.price, profit.ticks.mult, profit.ticks, min.price.increment)
#         #get(order.side)[order.number, "profit.price"] <- as.character(CalcProfitOrderPrice(trade.side, order.number, as.numeric(trade.price), profit.ticks.mult, profit.ticks, min.price.increment)
#       }  
#       if (loglevel > 2) cat("\nThese are the", init.side.name, "\n")
#       if (loglevel > 2) print(initial.side)
#       if (loglevel > 2) cat("\nThese are the", opp.side.name, "\n")
#       if (loglevel > 2) print(opposite.side)
#       
#       # -------------------------------------------------------
#       # Process the Trade
#       # -------------------------------------------------------
#       trade.output     <- ProcessTheTrade(trade.price, trade.side, trade.qty, commissions, cm.pos, margin, max.position, average.price, value.per.dollar, row)
#       cm.pos           <- trade.output$cm.pos
#       cm.pnl           <- trade.output$cm.pnl
#       cm.return        <- trade.output$cm.return
#       trade.action     <- trade.output$trade.action
#       trade.pnl        <- trade.output$trade.pnl
#       return.on.margin <- trade.output$return.on.margin
#       average.price    <- trade.output$average.price
#       ## Strategy Accounting        
#       #browser()
#       new.dataframe[row, ] <- CreateOutputRow(trade.side, trade.qty, 
#                                               trade.product, trade.price, 
#                                               cm.pnl, trade.action, 
#                                               trade.return = return.on.margin, 
#                                               cm.return, order.number = as.character(order.number),
#                                               entry.pnl = trade.pnl,
#                                               row.name = row.names(x)[row])
#       if (row == nrow(x)) { # if this is the last row
#         my.output[[element]]        <- new.dataframe[1:row, ]
#         names(my.output)[[element]] <- names(paramset.list)[element]
#       }
#     }
#   }
#   return(my.output)
# }

# #   -----------------------------------------------------------------------
# ## DELETE THIS AFTER 8/1/2015
# ModifyTradeQuantities <- function(paramset.list, modify.qty=T, commissions=.50, stop.amt=-1500, price.multiplier=.01,
#                                   value.per.dollar=1000, min.price.increment = .01, bid.sizes=rep(1, 15), 
#                                   offer.sizes=rep(1,15), profit.ticks=5, order.spacing=1, initial.offset=1, end.times.per.block = NULL,
#                                   cancel.entry.mode=NA, cancel.entry.times=NULL, margin=4250, keep.checking.for.flat=TRUE,
#                                   logfile="",
#                                   loglevel=1) {
#   
#   # Takes in a list of trades and desired bid & offer quantities, modifies the trades
#   # and spits out a list with modified trade sizes. format is the same as a regular parmset.list
#   #
#   # Args
#   #   paramset.list           character name of a list that contains trades
#   #   commissions             commissions rate per-side, NOT roundtrip
#   #   stop.amt                when to stop out, trades beyond this point are discarded
#   #   price.multiplier        the correct multiplier to get minimum value, eg .01 for CL, .0001 for currencies, etc
#   #   value.per.dollar        once multiplied, the price value for each dollar move
#   #                           eg 1000 for CL, 50 for ES, etc
#   #   min.price.increment     the change in price from one tick the next, eg .01 for CL, .25 for ES, etc
#   #   bid.sizes               numeric vector of bid sizes, with [1] being closest to the market, etc
#   #   offer.sizes             numeric vector of offer sizes, with [1] being closest to the market, etc
#   #   profit.ticks            where the profit orders would go in relation to the entry order
#   #   order.spacing           spacing for orders.  1 = every price, 2 = every other price, etc
#   #   intial offset           0 = at the best bid/offer, 1 means 1 tick back, 2 = 2 ticks back, etc
#   #   end.times.per.block     dataframe with block names in col 1 and end times in col 2.  like: block01, 06:45:00
#   #   cancel.entry.mode       modifies behavior after cancel entry time has passed. 
#   #                           0 = cancel all entry orders, 1 = cancel all entries on the same side as the position, 
#   #                           -1 = cancel orders on the opposite side of the position
#   #   cancel.entry.times      dataframe with block names in col 1 and entry order cancel times in col 2. like: block01, 06:45:00
#   #   keep.checking.for.flat  if using cancel entry mode and this is TRUE, all entries are cancelled once the position is flat and the timer has ended
#   #   margin                  margin of the product.  used to calculate return on margin and cumulative return on margin
#   #   loglevel                1 = no logging, 2 = qty, pos updates, 3 = print order book at each row
#   #
#   # Returns
#   #   list of dataframes with modified quantities and cumulative PNL
#   # -------------------------------------------------------------------------
#   ## Trap For Errors
#   if (is.character(paramset.list)) paramset.list <- get(paramset.list)
#   stopifnot(is.list(paramset.list))
#   if(is.null(names(paramset.list))) stop("ERROR: this list needs to have named elements")
#   if (stop.amt >= 0) stop("ERROR: stop amount needs to be negative")
#   if (any(c(commissions, price.multiplier, value.per.dollar, bid.sizes, initial.offset, offer.sizes) < 0)){
#     stop("ERROR: all arguments besides stop.amt require >=0")
#   }
#   if (order.spacing <=0) stop("ERROR: order spacing must be 1 or greater")
#   if (profit.ticks <=0) stop("ERROR: profit ticks must be 1 or greater")
#   commissions = -1 * commissions  # easier to calculate
#   output <- list()
#   if (is.null(bid.sizes))   bid.sizes<-rep(1, 30)
#   if (is.null(offer.sizes)) offer.sizes<-rep(1, 30)
#   if (!is.null(end.times.per.block) && !is.data.frame(end.times.per.block)) stop("ERROR: end.times.per.block must be a dataframe")
#   if (!is.null(cancel.entry.times) && !is.data.frame(cancel.entry.times)) stop("ERROR: cancel.entry.times must be a dataframe")
#   stopifnot(cancel.entry.mode %in% c(NA,0,1,-1))
#   if (ncol(paramset.list[[1]]) > 6) stop("ERROR: this function has already been run on this parameter set! no need to re-run")
#   
#   max.position <- max(sum(bid.sizes), sum(offer.sizes))
#   ## Process each trades element in the paramset.list, store in the output object
#   for (element in seq(names(paramset.list))) {
#     # element <- 3
#     if (loglevel > 1) cat("\n\n===========================")
#     if (loglevel >= 1) cat("\nProcessing:", names(paramset.list)[element], file = logfile, append = TRUE)
#     if (loglevel > 1) cat("\n\n===========================")
#     
#     ## -----------------------------------------------
#     ## Need to sort the list by timestamp
#     ## Occasionally, trades will come in out of order (lower bids will come in before higher bids, etc)
#     ## This causes issues when this is the first trade of a side, because the book will be set incorrectly
#     ## Method:
#     ##  - extract timestamps and truncate to 4? places instead of 6
#     ##  - make buy prices negative
#     ##  - order by timestamp and then price (increasing)
#     ##  - flip the buy prices back to positive
#     ## -----------------------------------------------
#     x <- paramset.list[[element]]
#     #x <- testing[[1]]
#     timestamps <- strptime(substr(rownames(x), 1, nchar(rownames(x)[1]) -2), format = "%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#     FlipSignOfBidPrices <- function(row) ifelse(row["side"] == "B", as.numeric(row["price"]) * -1, as.numeric(row["price"]))
#     #x[, "price"]  <- as.numeric(x[, "price"]) * price.multiplier   # not needed after sorting is added
#     x[, "price"] <- apply(x, 1, FlipSignOfBidPrices) * price.multiplier
#     x <- x[with(x, order(timestamps, price)), ]
#     x[, "price"] <- apply(x, 1, FlipSignOfBidPrices)  ## flip negative bid prices back to positive
#     
#     
#     ## initialize some variables
#     cm.pos        <- 0 
#     cm.pnl        <- 0     
#     average.price <- NULL
#     cm.return     <- 0
#     
#     ## Create a vector of timestamps and convert from char to POSIX
#     timestamps <- strptime(rownames(x), format ="%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#     if (any(is.na(timestamps))) stop("ERROR: unable to convert timestamps for ", names(paramset.list)[element])
#       
#     ## Get block number and set cancel entry and end.times if they exist
#     element.name <- unlist(strsplit(names(paramset.list)[element], "[.]"))
#     block <- element.name[grep("block", element.name)]
#     day <- as.character(strptime(timestamps[1], format = "%Y-%m-%d"))
#     if (!is.null(end.times.per.block)) {
#       end.time <- as.character(end.times.per.block[charmatch(block, end.times.per.block[,1]), 2])
#       end.time <- strptime(paste(day, end.time), format ="%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#     }
#     if (!is.na(cancel.entry.mode)) {
#       cancel.time <- as.character(cancel.entry.times[charmatch(block, cancel.entry.times[,1]), 2])
#       cancel.time <- strptime(paste(day, cancel.time), format ="%Y-%m-%d %H:%M:%OS", tz = "America/Chicago")
#       entries.are.cancelled <- FALSE
#     }
#       
#     ## Store modified trades in new.dataframe
#     ## should only be 6 columns in input frame, if there is 10, its a re-run
#     new.dataframe <- data.frame(matrix(NA, nrow = 1, ncol = 10))
#     colnames(new.dataframe) <- c(colnames(x), "return", "cm.return", "order.number", "entry.pnl")
#     
#     ## Create bid & offer matrices to store entry prices and profit order prices
#     bids <- cbind(bid.sizes, rep(NA, length(bid.sizes)), rep(NA, length(bid.sizes)))
#     colnames(bids) <- c("order.qty", "order.price", "profit.price")
#     offers <- cbind(offer.sizes, rep(NA, length(offer.sizes)), rep(NA, length(offer.sizes)))
#     colnames(offers) <- c("order.qty", "order.price", "profit.price")
#     opp.side.book.set  <- FALSE
#     
#     ## Process each row in the trades element
#     for (row in seq(nrow(x))) {
#       # row <- 7
#       if (loglevel > 1) cat("\n\n-------------------")
#       if (loglevel > 1) cat("\nProcessing Row", row, "of element", names(paramset.list)[element])
#       
#       ## Initialize some variables
#       trade.side    <- with(x, side[row])
#       trade.price   <- with(x, price[row]) 
#       trade.product <- with(x, product[row]) 
#       trade.qty     <- with(x, qty[row])
#       entry.pnl     <- NA
#       if (is.null(average.price)) average.price <- trade.price
#       
#       ## -------------------------------------------------------
#       ## Prevent New Entries
#       ## - Based on the supplied end-times, should I prevent new entries?
#       ## - 0 means cancel everything, -1 means cancel opposite side, 1 means cancel same side
#       ## -------------------------------------------------------
#       ## NOT SURE IF I NEED THIS entries.are.cancelled or not.
#       ## if i keep it, it means that i can trade again on after i get flat (side depends on the mode)
#       ## if i remove it, no new entries once i'm flat, after the timer expires. 
#       ## so, I can add to an existing position, but can't open a new position after the timer
#       if (cancel.entry.mode %in% c(-1,0,1) && !is.null(cancel.entry.times) && entries.are.cancelled == FALSE) {
#         if (timestamps[row] > cancel.time) {
#           if (loglevel > 1) cat("\nCancel Entries time has passed, cancelling entries now")
#           if (loglevel > 2) {
#             cat("\nTime:  ") 
#             print(timestamps[row]) 
#             cat("is greater than")
#             print(cancel.time)
#           }
#           ## Create Clear order book function
#           ClearOrderBook <- function(matrix) {
#             for (i in 1:nrow(matrix)) {
#               ## setting quantities to 0
#               ## checking for a profit order - if none, then set the qty to 0
#               ## if a profit order exists, then the entry already traded and it doesn't matter
#               if(is.na(matrix[i, "profit.price"])) matrix[i, "order.qty"] <- 0
#             }
#             return(matrix)
#           } 
#           ## Figure out what the position is and which side to cancel based on the cancel mode
#           if (cancel.entry.mode == 0 || cm.pos == 0) {
#             if (loglevel > 2) cat("\nCancelling all bids and offers - either flat or cancel mode is 0")
#             initial.side  <- ClearOrderBook(initial.side)
#             opposite.side <- ClearOrderBook(opposite.side)
#           } else if (cancel.entry.mode == -1 && cm.pos > 0) {
#             if (loglevel > 2) cat("\nCancelling all offers - I'm long and cancel mode is -1")
#             if (init.side.name == "offers")     initial.side  <- ClearOrderBook(initial.side)
#             else if (opp.side.name == "offers") opposite.side <- ClearOrderBook(opposite.side)
#             else stop ("ERROR: can't figure out which side of the book to clear out")
#           } else if (cancel.entry.mode == -1 && cm.pos < 0) {
#             if (loglevel > 2) cat("\nCancelling all bids - I'm short and cancel mode is -1")
#             if (init.side.name == "bids")       initial.side  <- ClearOrderBook(initial.side)
#             else if (opp.side.name == "bids")   opposite.side <- ClearOrderBook(opposite.side)
#             else stop ("ERROR: can't figure out which side of the book to clear out")
#           } else if (cancel.entry.mode ==  1 && cm.pos > 0) {
#             if (loglevel > 2) cat("\nCancelling all bids - I'm long and cancel mode is 1")
#             if (init.side.name == "bids")       initial.side  <- ClearOrderBook(initial.side)
#             else if (opp.side.name == "bids")   opposite.side <- ClearOrderBook(opposite.side)
#             else stop ("ERROR: can't figure out which side of the book to clear out")            
#           } else if (cancel.entry.mode ==  1 && cm.pos < 0) {
#             if (loglevel > 2) cat("\nCancelling all offers - I'm short and cancel mode is 1")
#             if (init.side.name == "offers")     initial.side  <- ClearOrderBook(initial.side)
#             else if (opp.side.name == "offers") opposite.side <- ClearOrderBook(opposite.side)
#             else stop ("ERROR: can't figure out which side of the book to clear out")
#           } else stop("ERROR, trying to cancel entries at ", row, "of ", names(paramset.list)[element], "but something is screwed up")
#           if (keep.checking.for.flat == FALSE) entries.are.cancelled <- TRUE  ## to allow some new positions after timer expires
#         }
#       }
#       
#       
#       ## -------------------------------------------------------
#       ## Virtual Stop-out
#       ## Based on the latest price, should I have been stopped out?
#       ##  - calculate theoretical trade pnl if i exited at the latest price
#       ##  - update theoretical cumulative PNL
#       ##  - check if theo.cm.pnl is < stop amount
#       ##  - if yes, exit position, update pnl, and break. if no, continue
#       ## -----------------------------------------
#       # Calculate Theoretical Trade PNL
#       theo.trade.commish <- abs(cm.pos) * commissions
#       if (loglevel > 2) cat("\nTheo Trade Commish (if i had to exit here) is", theo.trade.commish)
#       theo.trade.pnl  <- theo.trade.commish + (cm.pos * trade.price - cm.pos * average.price) * value.per.dollar
#       if (loglevel > 2) cat("\nTheo Trade PNL is", theo.trade.pnl)
#       ## Calculate Return on Margin 
#       ## - need to add commissions a second time since i'm only calculating
#       ##    this on closing trades, so need the round trip commish not one-way
#       ## - using log returns so they are additive, for cm.pnl
#       theo.return.on.margin <- log( 1 + ((theo.trade.pnl + theo.trade.commish) / (margin * max.position)))
#       if (loglevel > 2) cat("\nThe Return on Margin is", theo.return.on.margin)
#       
#       # Update Theoretical CM.PNL, theo.cm.returns & Check Stop Amount
#       theo.cm.pnl    <- cm.pnl + theo.trade.pnl
#       theo.cm.return <- cm.return + theo.return.on.margin
#       if (theo.cm.pnl < stop.amt || row == nrow(x)) {  # stopped out or last row?
#         if (loglevel > 1) {
#           if (theo.cm.pnl < stop.amt) cat("\nVirtual Stop-Out at row", row, "because", theo.cm.pnl, "<", stop.amt)
#           if (row == nrow(x))         cat("\nExiting position at last row")
#         } 
#         trade.qty    <- cm.pos  
#         trade.side   <- ifelse(cm.pos > 0, "S", "B")
#         trade.action <- "CLOSE:"
#         cm.pnl       <- theo.cm.pnl
#         cm.return    <- theo.cm.return
#         
#         ## figure out which entry orders closed when the position exited
#         ## three cases:
#         ## - long position - find all the rows with profit.prices in the bids matrix
#         ## - short position - find all the rows with profit.prices in the offers matrix
#         ## - no position (?) - set to 0
#         if (cm.pos > 0 && init.side.name == "bids") {
#           order.number <- (-1 * which(!is.na(initial.side[,"profit.price"])))
#           entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")        
#         } else if (cm.pos > 0 && opp.side.name == "bids") {
#           order.number <- (-1 * which(!is.na(opposite.side[,"profit.price"])))
#           entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         } else if (cm.pos < 0 && init.side.name == "offers") {
#           order.number <- which(!is.na(initial.side[,"profit.price"]))
#           entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#         } else if (cm.pos < 0 && opp.side.name == "offers") {
#           order.number <- which(!is.na(opposite.side[,"profit.price"]))
#           entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")          
#         } else order.number <- "0"
#         
#         ## Write values to new dataframe
#         if(trade.side == "S") order.number <- (-1 * order.number)  # track which entry was closed.  negative for bids, positive for offer entries
#         new.dataframe[row,]                <- c(NA, abs(trade.qty), NA, trade.price, cm.pnl, NA, NA, NA, NA, NA)
#         new.dataframe[row, "side"]         <- trade.side
#         new.dataframe[row, "product"]      <- trade.product
#         new.dataframe[row, "action"]       <- trade.action
#         new.dataframe[row, "return"]       <- theo.return.on.margin
#         new.dataframe[row, "cm.return"]    <- cm.return
#         new.dataframe[row, "order.number"] <- paste(order.number, collapse=",")   
#         new.dataframe[row, "entry.pnl"]    <- entry.pnl
#         row.names(new.dataframe)[row]      <- row.names(x)[row]  # leaving timestamp as char, same format as it came in
#         ## Write to the output list
#         output[[element]] <- new.dataframe
#         names(output)[[element]] <- names(paramset.list)[element]
#         break
#       }
# 
# 
#       ## -------------------------------------------------------
#       ## Check Exit Time
#       ## - Based on the supplied end-times, should I exit?
#       ## -------------------------------------------------------
#       if (!is.null(end.times.per.block)) {
#         ## check the time, and exit the position if necessary
#         if (timestamps[row] > end.time) {
#           ## Exit the position 
#           if (loglevel > 1) cat("\nExiting position at end of timer")
#           if (loglevel > 2) {
#             cat("\nTime:  ")
#             print(timestamps[row])
#             cat("is greater than")
#             print(end.time)
#           }
#           trade.qty    <- cm.pos  
#           trade.side   <- ifelse(cm.pos > 0, "S", "B")
#           trade.action <- "CLOSE:"
#           cm.pnl       <- theo.cm.pnl
# 
#           ## figure out which entry orders closed when the position exited
#           ## three cases:
#           ## - long position - find all the rows with profit.prices in the bids matrix
#           ## - short position - find all the rows with profit.prices in the offers matrix
#           ## - no position (?) - set to 0
#           if (cm.pos > 0 && init.side.name == "bids") {
#             order.number <- (-1 * which(!is.na(initial.side[,"profit.price"])))
#             entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#           } else if (cm.pos > 0 && opp.side.name == "bids") {
#             order.number <- (-1 * which(!is.na(opposite.side[,"profit.price"])))
#             entry.pnl    <- paste(bids[order.number, "order.qty"] * ((trade.price  - (bids[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#           } else if (cm.pos < 0 && init.side.name == "offers") {
#             order.number <- which(!is.na(initial.side[,"profit.price"]))
#             entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#           } else if (cm.pos < 0 && opp.side.name == "offers") {
#             order.number <- which(!is.na(opposite.side[,"profit.price"]))
#             entry.pnl    <- paste(offers[order.number, "order.qty"] * ((-trade.price  + (offers[order.number, "order.price"])) * value.per.dollar), collapse = ",")
#           } else order.number <- "0"
#           
#           ## Write values to new dataframe
#           if(trade.side == "S") order.number <- (-1 * order.number)  # track which entry was closed.  negative for bids, positive for offer entries
#           new.dataframe[row,]                <- c(NA, abs(trade.qty), NA, trade.price, cm.pnl, NA, NA, NA, NA, NA)
#           new.dataframe[row, "side"]         <- trade.side
#           new.dataframe[row, "product"]      <- trade.product
#           new.dataframe[row, "action"]       <- trade.action
#           new.dataframe[row, "return"]       <- theo.return.on.margin
#           new.dataframe[row, "cm.return"]    <- cm.return
#           new.dataframe[row, "order.number"] <- paste(order.number, collapse=",")  # set as character
#           new.dataframe[row, "entry.pnl"]    <- entry.pnl
#           row.names(new.dataframe)[row]      <- row.names(x)[row]  # leaving timestamp as char, same format as it came in
#           
#           ## Write to the output list
#           output[[element]] <- new.dataframe
#           names(output)[[element]] <- names(paramset.list)[element]
#           break
#         }        
#       }
#       
#       ## ----------------------------------------------
#       ## If no theoretical exit, process as usual: 
#       ## ----------------------------------------------
#       ## If the last row, just exit the position and break to next element
#       
#       ## Determine new quantity and commissions using bids and offers vectors
#       if (modify.qty) {
#         # -------------------------------------------------------
#         # Setup the Virtual Order Book
#         # - use first fill, intial offset, and order spacing
#         #   to determine the strategy's order book
#         # -------------------------------------------------------
#         if (row == 1) {
#           if (trade.side == "B") {
#             initial.side      <- bids
#             init.side.name    <- "bids"
#             opposite.side     <- offers
#             opp.side.name     <- "offers"
#             offset.mult       <- (-1)
#             profit.ticks.mult <- 1
#           } else {
#             initial.side      <- offers
#             init.side.name    <- "offers"
#             opposite.side     <- bids
#             opp.side.name     <- "bids"
#             offset.mult       <- 1
#             profit.ticks.mult <- (-1)
#           }
#           # Populate the "Initial Side" order book based on the first trade 
#           # - set the initial price
#           # - then populate the rest of the book based on order spacing
#           initial.side[1, "order.price"]  <- trade.price
#           for (i in 2:nrow(initial.side)){
#             initial.side[i, "order.price"] <- initial.side[i-1, "order.price"] + (offset.mult * order.spacing * min.price.increment)
#           }  
#         }
#         
#         # -------------------------------------------------------
#         # Set the Virtual Order Quantity
#         # -------------------------------------------------------
#         
#         for (i in 1) {  # run once
#           ## Track the original qty for debugging
#           unmodified.trade.qty <- trade.qty
#           
#           # -------------------------------------------------------
#           # STEP 1:  TRY MATCHING TRADE PRICE AGAINST PROFIT ORDER PRICES
#           # - if prices match, calc PNL and set price to NULL
#           # -------------------------------------------------------
#           order.number <- Match.numeric(trade.price, table=unclass(opposite.side[, "profit.price"]))
#           if (loglevel > 2) cat( "\nTrade Price is", trade.price, "\nLooking for a match in Profit Orders")   
#           if (loglevel > 2) cat( "\nMatch values are: \n")   
#           if (loglevel > 2) print(unclass(opposite.side[, "profit.price"]))
#           if (!is.na(order.number)) {
#             trade.qty <- opposite.side[order.number, "order.qty"]
#             opposite.side[order.number, "profit.price"] <- NA   # this order has been traded
#             if (loglevel > 1) cat( "\nModified trade qty based on profit qty of opposite side, old qty was", unmodified.trade.qty, "new qty is", trade.qty)
#             if (loglevel > 2) cat("\nThese are the", init.side.name, "\n")
#             if (loglevel > 2) print(initial.side)
#             if (loglevel > 2) cat("\nThese are the", opp.side.name, "\n")
#             if (loglevel > 2) print(opposite.side)
#             break
#           }
#           order.number <- Match.numeric(trade.price, table=unclass(initial.side [, "profit.price"]))
#           if (loglevel > 2) cat( "\nMatch values are: \n" )   
#           if (loglevel > 2) print(unclass(initial.side[, "profit.price"]))
#           if (!is.na(order.number)) {
#             trade.qty <- initial.side[order.number, "order.qty"]
#             initial.side[order.number, "profit.price"] <- NA   # this order has been traded
#             if (loglevel > 1) cat( "\nModified trade qty based on profit qty of initial side, old qty was", unmodified.trade.qty, "new qty is", trade.qty)
#             if (loglevel > 2) cat("\nThese are the", init.side.name, "\n")
#             if (loglevel > 2) print(initial.side)
#             if (loglevel > 2) cat("\nThese are the", opp.side.name, "\n")
#             if (loglevel > 2) print(opposite.side)
#             break
#           }
#           # -------------------------------------------------------
#           # STEP 2:  TRY MATCHING TRADE PRICE AGAINST ENTRY ORDER PRICES
#           # - if prices match, calc PNL and set price to NULL
#           # - calc profit ticks price and qty
#           # -------------------------------------------------------
#           profit.ticks.mult <- ifelse(trade.side == "B", 1, -1)  # set multiplier based on trade side
#           if (loglevel > 2) cat( "\nTrade Price is", trade.price, "\nLooking for a match in Entry Orders")  
#           ## note:  needed to convert prices to characters to avoid a floating point error that caused them to not match
#           ##        when they should.   could create a custom function to match using all.equal instead...
#           # order.number <- charmatch(as.character(trade.price), table=as.character(unclass(initial.side [, "order.price"])))
#           order.number <- Match.numeric(trade.price, table=unclass(initial.side [, "order.price"]))
#           if (loglevel > 2) cat( "\nMatch values are: \n" )   
#           if (loglevel > 2) print(unclass(initial.side[, "order.price"]))
#           if (!is.na(order.number)) {
#             if (loglevel > 1) cat( "\nOrder number from", init.side.name, "book is", order.number)        
#             trade.qty <- initial.side[order.number, "order.qty"]
#             initial.side[order.number, "order.price"] <- NA   # this order has been traded
#             if (loglevel > 1) cat( "\nModified trade qty based on order.qty of initial side, old qty was", unmodified.trade.qty, "new qty is", trade.qty)
#             # set profit ticks price
#             initial.side[order.number, "profit.price"] <- trade.price + (profit.ticks.mult * profit.ticks * min.price.increment)
#             if (loglevel > 1) cat( "\nSet Profit Order on initial side book for order.number", order.number)
#             if (loglevel > 2) cat("\nThese are the", init.side.name, "\n")
#             if (loglevel > 2) print(initial.side)
#             if (loglevel > 2) cat("\nThese are the", opp.side.name, "\n")
#             if (loglevel > 2) print(opposite.side)
#             break
#           }
#           ## Price didn't match profit ticks or intial side entry orders, so this must be an opposite side entry order
#           ## Check first if book is set, and if not, set it
#           if (opp.side.book.set == FALSE) {
#             # opposite.side[1, "order.price"] <- trade.price + (-offset.mult * min.price.increment * (2 * initial.offset + 1)) # there is inconsistency with order spacing that is causing this to not work
#             opposite.side[1, "order.price"] <- trade.price 
#             for (i in 2:nrow(opposite.side)) {
#               opposite.side[i, "order.price"] <- opposite.side[i-1, "order.price"] + (-offset.mult * order.spacing * min.price.increment)
#             }
#             if (loglevel > 1) cat( "\nThis must be the first order on the opposite side!" )   
#             if (loglevel > 1) cat( "\nOpposite entry book is set with", trade.price, "as the first order")   
#             opp.side.book.set <- TRUE
#           }
#           #order.number <- charmatch(as.character(trade.price), table=as.character(unclass(opposite.side [, "order.price"])))
#           order.number <- Match.numeric(trade.price, table=unclass(opposite.side[, "order.price"]))
#           if (loglevel > 2) cat( "\nMatch values are: \n" )   
#           if (loglevel > 2) print(unclass(opposite.side[, "order.price"]))
#           if (!is.na(order.number)) {
#             if (loglevel > 1) cat( "\nOrder number from", opp.side.name, "book is", order.number)   
#             trade.qty <- opposite.side[order.number, "order.qty"]
#             opposite.side[order.number, "order.price"] <- NA   # this order has been traded
#             if (loglevel > 1) cat( "\nModified trade qty based on order.qty of opposite side, old qty was", unmodified.trade.qty, "new qty is", trade.qty)
#             ## set profit ticks price
#             opposite.side[order.number, "profit.price"] <- trade.price + (profit.ticks.mult * profit.ticks * min.price.increment)
#             if (loglevel > 1) cat( "\nSet Profit Order on opposite side book for order.number", order.number)
#             if (loglevel > 2) cat("\nThese are the", init.side.name, "\n")
#             if (loglevel > 2) print(initial.side)
#             if (loglevel > 2) cat("\nThese are the", opp.side.name, "\n")
#             if (loglevel > 2) print(opposite.side)
#             break
#           }
#           # -------------------------------------------------------
#           # ERROR: Should not have gotten this far
#           # -------------------------------------------------------
#           if (loglevel > 1) {
#             cat("\nERROR: Did not successfully match and modify the trade qty in row", row, "of", names(paramset.list)[element])
#             cat("\n\n************************************")
#             cat("\nMight need to make the bid and offer quantities longer?")
#             cat("\nAlso could be that exit price did not match any stored prices and theo stop did not trigger")
#             cat("\n\n************************************")
#             cat("\nThis is the trade price:", trade.price)
#             cat("\nThis is the trade side:", trade.side)
#             cat("\nThese are the", init.side.name, "\n")
#             print(initial.side)
#             cat("\nThese are the", opp.side.name, "\n")
#             print(opposite.side)
#           }
#           stop("ERROR: Did not successfully modify the trade qty in row ", row, " of ", names(paramset.list)[element])
#         }
#         ## END MODIFY QTY
#       }
#       
#       # -------------------------------------------------------
#       # PROCESS THE TRADE: 5 cases
#       # - nothing happend - order qty was 0
#       # - flip a position (eg long to short)
#       # - closed a position (position = 0)
#       # - add to a position
#       # - reduce a position
#       # -------------------------------------------------------
#       ## Set sign for trade qty (helps standardize calculations)
#       if (trade.side == "S") trade.qty <- (-1) * trade.qty
#       trade.commish <- abs(trade.qty) * commissions
#       new.cm.pos    <- cm.pos + trade.qty
#       if (loglevel > 1) cat("\nNew CmPos for row", row, "is", new.cm.pos)
#       
#       # Case1: Order qty was 0, continue on to next row
#       if (trade.qty == 0) {
#         trade.pnl        <- 0
#         return.on.margin <- 0
#         if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#         if (loglevel > 1) cat("\nSynthetic Order Qty is 0, so everything stays the same")
#         
#       # Case2: Position Flipped from long to short, or vice versa.  there must be a simpler way to code this...
#       } else if (cm.pos > 0 && new.cm.pos < 0 || cm.pos < 0 && new.cm.pos > 0 ) {  
#         trade.pnl        <- trade.commish + (cm.pos * trade.price - cm.pos * average.price) * value.per.dollar
#         return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
#         average.price    <- trade.price
#         if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#         if (loglevel > 1) cat("\nFlipped Position, New Average Price is", average.price, "for row", row)
#         
#       # Case3: Position Closed
#       } else if (new.cm.pos == 0) {       
#         trade.pnl        <- trade.commish + (trade.qty * average.price - trade.qty * trade.price) * value.per.dollar
#         return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
#         average.price    <- 0
#         if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#         if (loglevel > 1) cat("\nClosed Position, new Average Price is", average.price, "for row", row)
#         
#       # Case4: Added to an existing position (either long or short)
#       } else if (abs(new.cm.pos) > abs(cm.pos)) {  
#         trade.pnl        <- trade.commish
#         return.on.margin <- 0
#         average.price <- (trade.qty * trade.price + cm.pos * average.price) / new.cm.pos
#         if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#         if (loglevel > 1) cat("\nAdded to Position, new Average Price is", average.price, "for row", row)
#         
#       # Case5: Reduced an existing position (either long or short)  
#       } else if (abs(new.cm.pos) < abs(cm.pos)) {  
#         trade.pnl        <- trade.commish + (trade.qty * average.price - trade.qty * trade.price) * value.per.dollar
#         return.on.margin <- log( 1 + ((trade.pnl + trade.commish) / (margin * max.position)))
#         if (loglevel > 1) cat("\nTrade.PNL for row", row, "is", trade.pnl)
#         if (loglevel > 1) cat("\nAverage Price stays the same because i reduced position:", average.price)
#         
#       } else stop("ERROR: issue with PNL calc at row ", row, " in ", names(paramset.list)[element])
#       cm.pos    <- new.cm.pos
#       cm.pnl    <- cm.pnl + trade.pnl
#       cm.return <- cm.return + return.on.margin
#       if (loglevel > 1) cat("\nCmPos for row", row, "is", cm.pos)
#       if (loglevel > 1) cat("\nCmPNL for row", row, "is", cm.pnl)
#       if (loglevel > 1) cat("\nCmReturn for row", row, "is", cm.return)
#       trade.action <- ifelse(cm.pos == 0, "CLOSE:", "OPEN:")
#       
#       ## Write values to new dataframe
#       if(trade.side == "S") order.number <- (-1 * order.number)  # track which entry was closed.  negative for bids, positive for offer entries
#       new.dataframe[row,] <- c(NA, abs(trade.qty), NA, trade.price, cm.pnl, NA, NA, NA, NA, NA)
#       new.dataframe[row, "side"]         <- trade.side
#       new.dataframe[row, "product"]      <- trade.product
#       new.dataframe[row, "action"]       <- trade.action
#       new.dataframe[row, "return"]       <- return.on.margin
#       new.dataframe[row, "cm.return"]    <- cm.return
#       new.dataframe[row, "order.number"] <- as.character(order.number)
#       new.dataframe[row, "entry.pnl"]    <- trade.pnl  # only one entry is closed
#       row.names(new.dataframe)[row]      <- row.names(x)[row]  # leaving timestamp as char, same format as it came in
#       
#       ## Write to the output list
#       if (row == nrow(x)) {
#         output[[element]] <- new.dataframe
#         names(output)[[element]] <- names(paramset.list)[element]
#       }
#     }
#   }
#   return(output)
# }



#  ------------------------------------------------------------------------
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


#  ------------------------------------------------------------------------
MakeBlockNames <- function() {
  # Makes a vector of block names, 1-18
  output <- c(paste0("block0", 1:9), paste0("block", 10:18))
  return(output)
}


#   -----------------------------------------------------------------------
MakeEndTimesDataFrame <- function(){
  # makes a dataframe of generic end.times to input into the ModifyQuantities
  # function
  block.names <- MakeBlockNames()
  end.times <- c("06:45:00",
                 "07:15:00",
                 "07:45:00",
                 "08:15:00",
                 "08:45:00",
                 "09:15:00",
                 "09:45:00",
                 "10:15:00",
                 "10:45:00",
                 "11:15:00",
                 "11:45:00",
                 "12:15:00",
                 "12:45:00",
                 "13:15:00",
                 "13:45:00",
                 "14:15:00",
                 "14:45:00",
                 "15:15:00")
  output <- as.data.frame(cbind(block.names, end.times), stringsAsFactors = F)
  return(output)
}


#   -----------------------------------------------------------------------
#' An alternative to \code{summaryRprof()}
#' 
#' \code{proftools} parses a profiling file and prints an easy-to-understand
#' table showing the most time-intensive function calls. 
#' 
#' Line numbers are #' included if \code{Rprof()} was run with 
#' \code{line.numbering=TRUE}. If it was run with \code{memory.profiling=TRUE},
#' this function will probably break.
#' 
#' Below the table are printed any files identified if line numbering is true,
#' the total time recorded by \code{Rprof()}, and the "parent call".  The
#' parent call consists of the parent call stack of all the call stacks in the\
#' table. Note that this is the parent call stack of only the printed lines,
#' not of all stacks recorded by \code{Rprof()}. This makes the table easier to. 
#' 
#' @export
#' @param file A profiling file generated by \code{Rprof()}
#' @param lines The number of lines (call stacks) you want returned. Lines are
#' printed from most time-intensive to least.  Only a few 
Proftable <- function(file, lines = 10) {
  profdata <- readLines(file)
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
  filelines <- grep("#File", profdata)
  files <- profdata[filelines]
  if (length(filelines) > 0)
    profdata <- profdata[-1:-filelines]
  total.time <- interval * length(profdata)
  ncalls <- length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)
  calls <- lapply(profdata, function(x) rev(unlist(strsplit(x, " "))))
  calls.len <- range(sapply(calls, length))
  parent.call <- unlist(lapply(seq(calls.len[1]), function(i) Reduce(intersect, lapply(calls,"[[", i))))
  calls <- lapply(calls, function(x) setdiff(x, parent.call))
  stacktable <- as.data.frame(table(sapply(calls, function(x) paste(x, collapse = " > "))) / ncalls * 100, stringsAsFactors = FALSE)
  stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
  colnames(stacktable) <- c("PctTime", "Call")
  stacktable <- head(stacktable, lines)
  if (length(parent.call) > 0) {
    parent.call <- paste(parent.call, collapse = " > ")
  } else {
    parent.call <- "None"
  }
  frac <- sum(stacktable$PctTime)
  attr(stacktable, "total.time") <- total.time
  attr(stacktable, "parent.call") <- parent.call
  attr(stacktable, "files") <- files
  attr(stacktable, "total.pct.time") <- frac
  cat("\n")
  print(stacktable, row.names=FALSE, right=FALSE, digits=3)
  cat("\n")
  cat(paste(files, collapse="\n"))
  cat("\n")
  cat(paste("\nParent Call:", parent.call))
  cat(paste("\n\nTotal Time:", total.time, "seconds\n"))
  cat(paste0("Percent of run time represented: ", format(frac, digits=3)), "%")
  
  invisible(stacktable)
}



#   -----------------------------------------------------------------------
ProcessBackTesterOutput <- function(folder.paramset, folder.rdata, folder.csvfiles, folder.processed, commish.per.side, log.filename="") {
  # Converts a paramset folder with trade CSVs into a list
  # containing trade dataframes
  #
  # Args
  #   folder.paramset           a paramset folder containing CSV files
  #   folder.destination        where to write the CSV files
  #
  # Returns
  #   a .rdata file containing the list
  # ---------------------------------------------------------------
  # folder.paramset <- list.dirs("c:/users/jharper/Documents/_TEMP/BackTester/NextBatch", recursive = F)[12]
  # folder.destination <- "./2015-06-28.StackerDE.CL-InitialTest/Rdata.files"
  # log.filename <- ""
  
  ## Copy paramset folder to project folder
  cat("\n--------------------------------------------------", file = log.filename, append = FALSE)
  cat("\nProcessing:", folder.paramset, file = log.filename, append = TRUE)
  name.paramset <- unlist(strsplit(folder.paramset, "[/]"))
  name.paramset <- subset(name.paramset, grepl("results", name.paramset))
  
  ## Process the data ##
  cat("\nLoading data for:", folder.paramset, file = log.filename, append = TRUE)
  x  <- ProcessCsvFiles(folder.paramset, commish.per.side, logfile = "")
  if (length(x) == 0) cat("ERROR:  parameter set folder ", project.folder, "has no trade data. Stopping...", file = log.filename, append = TRUE)
  assign(name.paramset, x); rm(x)
  cat("\nProcessing complete for:", folder.paramset, file = log.filename, append = TRUE)
  
  ## Create data table and csv ##
  name.endpnl  <- paste0("end.pnl.", name.paramset)
  filename     <- paste0(folder.csvfiles, "/", name.endpnl, ".csv")
  x            <- OutputEndingPNL(get(name.paramset), cm.pnl.column = 5, filename = filename)   # needs the "blockXX" in the name
  assign(name.endpnl, x); rm(x)
  cat("\nCreated end.pnl tables", file = log.filename, append = TRUE)
  
  ## Save to Rdata file ##
  name.rdatafile  <- paste0(folder.rdata, "/", name.paramset, ".Rdata")
  save(list = (ls(pattern = name.paramset)), file = name.rdatafile)
  cat("\nSaved Rdata files", file = log.filename, append = TRUE)
  
  ## Archive source files ##
  new.location <- paste0(folder.processed, "/", name.paramset)
  file.rename(from = folder.paramset, to = new.location)  
  cat("\nArchived source files to processed folder", file = log.filename, append = TRUE)  
  cat("\n--------------------------------------------------\n", file = log.filename, append = TRUE)
}


#   -----------------------------------------------------------------------
RemoveFileTagFromName <- function(name) {
  # Removes the tag at the end of a file, useful if this is now the object name
  # example:  object.rdata --> object
  x <- unlist(strsplit(name, "[.]"))
  file.tag <- length(x)
  x <- paste(paramset.list[-file.tag], collapse = ".")
  return(x)
}


#   -----------------------------------------------------------------------
FindMaxPosition <- function(paramset.list) {
  # takes in a paramset list and figures out what the maximum qty the strategy had 
  # used for return on margin calculations
  #
  # args
  #   paramset.list     character name of a list containing trades dataframes
  #
  # returns
  #   a single value representing the maximum qty per dataframe for the entire paramset list
  # ---------------------------------------------------------------------
  if (!is.list(paramset.list)) paramset.list <- get(paramset.list)
  stopifnot(is.list(paramset.list))
  
  ExtractMaxPosition <- function(element) {
    # supply trades data frame
    # return maximum position
    # -------------------------
    # element.name <- names(paramset.list)[1]
    # x   <- paramset.list[[element.name]]
    stopifnot(is.data.frame(element))
    if (nrow(element) > 0) {  # is there data in this frame?
      qty <- ifelse(element$side == "S", -1 * element$qty, element$qty)    
      cm.qty     <- cumsum(qty)
      max.qty    <- max(abs(cm.qty)) 
      return(max.qty)
    } else return(0)
  }
  
  ## calculate max.qty
  return(max(sapply(X = paramset.list, FUN = ExtractMaxPosition)))
}


#   -----------------------------------------------------------------------
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
  if (!all((c("qty", "side", "cm.pnl") %in% names(trades)))) stop("ERROR: colnames missing qty, side, or cm.pnl")
  if (all(c("returns", "cm.returns", "trade.pnl") %in% names(trades))) {
    warning("this is a re-run of the CalcReturnOnMargin function.  no harm, just wasting processing time :)")
    trades <- trades[, -match(c("returns", "cm.returns", "trade.pnl"), colnames(trades))]  # remove extra columns
  }
  
  ## calculate max.qty
  if (is.null(max.qty)) {
    qty        <- ifelse(trades$side == "S", -1 * trades$qty, trades$qty)
    cm.qty     <- cumsum(qty)
    max.qty    <- max(abs(cm.qty)) 
  }
  ## calculate returns
  trade.pnl      <- c(trades$cm.pnl[1], diff(trades$cm.pnl))
  returns        <- log(1 + (trade.pnl / (max.qty * margin)))
  cm.returns     <- cumsum(returns)
  
  ## append to original dataframe
  output <- cbind(trades, returns, cm.returns, trade.pnl)
  return(output)
}



#  ------------------------------------------------------------------------
FindStopLossAmount <- function(paramset.name, search.text = "StopLossAmt") {
  # Subsets the Stop Loss Amount from the paramset name
  # used in CalcEquityCurveSummaryStats to figure out if the strategy was
  # stopped out or not
  name     <- unlist(strsplit(paramset.name, "[.]"))
  stoptext <- name[grepl(search.text, name)]
  output   <- substr(stoptext, nchar(search.text) + 1, stop = nchar(stoptext))
  return(output)
}


#   -----------------------------------------------------------------------
ExtractObjectiveFunctions <- function(stats.lists, objective.functions = NULL) {
  # Take in the output from CalcEquityCurveStats, pulls out the important objective functions
  # and sorts them
  #
  # args
  #   stats.lists           character vector, names of the output of CalcEquityCurveStats
  #   objective.functions   character vector, corresponds to row names in CalcEquityCurveStats output
  # returns
  #   list containing named vectors for each objective function
  # --------------------------------------------------------
  if (is.null(objective.functions)) {
    objective.functions <- c("NetPNL", "Profit Factor", "NetPNL/Max Drawdown", "Omega Ratio",
                             "Sharpe Ratio", "Sortino Ratio", "System Quality Number(Returns)", "Robust SQN w/Ulcer")
  }
  output        <- vector("list", length(objective.functions))
  names(output) <- objective.functions
  for (i in seq(stats.lists)) {
    my.list <- get(stats.lists[i])
    for (fun in seq(objective.functions)) {
      output[[fun]][i] <- my.list[[1]][objective.functions[fun], ]
      names(output[[fun]])[i] <- stats.lists[i]
    }
  }
  output <- lapply(output, FUN = function(element) sort(element, decreasing = TRUE))
  return(output)
}


#   -----------------------------------------------------------------------
ExtractStats <- function(stats.lists, element) {
  # Generic function to extract stats from the output of CalcEquityCurveStats objects
  # and sorts them if possible
  #
  # args
  #   stats.lists           character vector, names of the output of CalcEquityCurveStats
  #   element               a number of which element to extract and combine
  #     1 = summary stats
  #     2 = eod pnl
  #     3 = trades
  #     4 = cm.returns
  #     5 = cm.pnl
  #     6 = max positions
  #     7 = returns
  #     8 = endpnl per time block
  #
  # returns
  #   if xts, a combined xts object, if a stats dataframe, a combined datafarme, otherwise a list
  # --------------------------------------------------------
  require(quantstrat)
  stopifnot(element %in% seq(names(get(stats.lists[[1]]))))  # make sure element value is valid
  output <- lapply(stats.lists, function(x) get(x)[[element]])
  if (element == 1) output <- do.call(cbind,output)
  if (element %in% c(4,5)) { # cm.pnl or cm.returns 
    output <- na.locf(do.call(cbind, output)) 
    ## for some reason, the above code changes negative signs to ".", need to reset colnames
    colnames(output) <- sapply(stats.lists, function(x) names(get(x))[element], USE.NAMES = FALSE)
  }
  if (is.list(output)) names(output) <- stats.lists
  return(output)
}



#  ------------------------------------------------------------------------
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

