###########################################################################

#   Charting Library for Stocks

#   Description:
#     Wrappers for quantmod and ggplot2 to plot stocks/futures/etc
#     and trades, indicators, drawdowns, etc
#
#   Inputs
#     - OHLC xts object with these extra columns:
#         long.entry, long.exit, short.entry, short.exit
#     - note: trade columns must have either prices or NA

#   Outputs
#     - chart object

#   Date
#     2016-12-06
###########################################################################
# install.packages("PerformanceAnalytics")
# install.packages("devtools")
# library(devtools)
# devtools::install_github("baptiste/gtable")
# install.packages("gridExtra")
# install.packages("quantmod")
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(gtable)
library(gridExtra)


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


PlotHarpCharts <- function(params, harp.output) {
  charts <- list()
  if (params$chart.method == "qm") {
    library(quantmod)
    charts$a <- if (params$chart.trades)   PlotPrices(harp.output$chart.df, chart.type = params$chart.type, chart.title = symbol, plot.trades = T, plot.stops.targets = T, plot.exitEMA = T, plot.trendInd = T)
    charts$b <- if (params$chart.vol)      suppressWarnings(add_TA(harp.output$chart.df$Volume,    type = 'h', col = HarpColors(8), lwd = 3, name = "Volume"))
    charts$c <- if (params$chart.position) add_TA(harp.output$chart.df$position,  type = "h", col = HarpColors(2), lwd = 3, name = "Position")
    charts$d <- if (params$chart.entrysig) add_TA(harp.output$chart.df$entry.ind.strat,  type = "l", col = HarpColors(4), lwd = 5, name = "Entry Indicator")
    charts$e <- if (params$chart.exitsig)  add_TA(harp.output$chart.df$exit.ind.strat,   type = "l", col = HarpColors(5), lwd = 5, name = "Exit Indictor")
    charts$f <- if (params$chart.atr)      add_TA(harp.output$chart.df$atr,  type = "l", col = HarpColors(6), lwd = 5, name = "ATR")
    charts$g <- if (params$chart.cmpnl)    add_TA(harp.output$chart.df$cm.pnl,  type = "l", col = HarpColors(3), lwd = 5, name = "Cumulative PNL")
    charts$h <- if (params$chart.cmret)    add_TA(harp.output$chart.df$cm.returns,  type = "l", col = HarpColors(3), lwd = 5, name = "Cumulative Returns")
    charts$i <- if (params$chart.dd)       add_TA(harp.output$chart.df$drawdown,  type = "l", col = HarpColors(1), lwd = 5, name = "Drawdown")
  } else {
    library(ggplot2)
    # library(devtools)
    # devtools::install_github("baptiste/gtable")
    library(gtable)
    library(gridExtra)
    charts$a <- if (params$chart.trades)   ggplotGrob(PlotPrices(harp.output$chart.df, chart.type = params$chart.type, method = "gg", chart.title = symbol, plot.trades = T, plot.stops.targets = T, plot.exitEMA = T, plot.trendInd = T))
    charts$b <- if (params$chart.vol)      ggplotGrob(PlotGGVolume(harp.output$chart.df, 30))
    charts$c <- if (params$chart.position) ggplotGrob(PlotGGIndicator(harp.output$chart.df$position, type = "h", color = 2, chart.title = "Position"))
    charts$d <- if (params$chart.entrysig) ggplotGrob(PlotGGIndicator(harp.output$chart.df$entry.ind.strat, type = "a", size = 1.5, color = 4, chart.title = "Entry Indicator", up.thresh = params$trggr.short.entry, dn.thresh = params$trggr.long.entry))
    charts$e <- if (params$chart.exitsig)  ggplotGrob(PlotGGIndicator(harp.output$chart.df$exit.ind.strat, type = "a", size = 1.5, color = 5, chart.title = "Exit Indicator", up.thresh = params$trggr.long.exit, dn.thresh = params$trggr.short.exit))
    charts$f <- if (params$chart.atr)      ggplotGrob(PlotGGIndicator(harp.output$chart.df$atr, type = "a", color = 6, size = 1.5, chart.title = "ATR"))
    charts$g <- if (params$chart.cmpnl)    ggplotGrob(PlotGGIndicator(harp.output$chart.df$cm.pnl, type = "a", color = 3, size = 1, chart.title = "Cumulative PNL"))
    charts$h <- if (params$chart.cmret)    ggplotGrob(PlotGGIndicator(harp.output$chart.df$cm.returns, type = "a", color = 3, size = 1, chart.title = "Cumulative Returns"))
    charts$i <- if (params$chart.dd)       ggplotGrob(PlotGGIndicator(harp.output$chart.df$drawdown, type = "a", color = 1, size = 1, chart.title = "Drawdown"))
    ## extract widths from each chart
    ## find the max chart width
    ## set all charts to this width
    ## set arrangement
    ## return gtable
    max.width <- do.call(unit.pmax, lapply(charts, function(element) element$widths))
    for (e in seq(charts)) charts[[e]]$widths <- max.width
    h <- c(2, rep(1, length(charts) - 1))
    charts$all.charts <- grid.arrange(grobs = charts, heights = h)
    ## another method, doesn't set the chart height, but simpler...
    #charts$all.charts <- do.call(rbind, charts)
    #grid.draw(do.call(rbind, charts))
    ## or:  Multiplot(plotlist = charts)
  }
  return(charts)
}
LoadPrices <- function(product, date = NULL, dir.prices = "//gv-wsjh1/ITGTickData", price.file = "Trades") {
  # Desc: Wrapper for load function, loads price files
  # Args
  #   product             like "CL" or "ES", etc
  #   date                char date,  like "2015-06-11"
  #   dir.prices          where to search for price files
  #   price.file          filter for Trades, Bids, Asks
  #
  # Returns
  #   returns a named list of prices
  # -----------------------------------------------------------------------
  ## Filter by date, then by product, then by trades/bids/offers
  files  <- list.files(dir.prices, full.names = F, recursive = FALSE)
  if (is.null(date)) {
    prices <- subset(files, grepl(product, files) & grepl(price.file, files))
  } else {
    prices <- subset(files, grepl(product, files) & grepl(date, files) & grepl(price.file, files))
  }

  ## Load prices
  prices <- sapply(prices, function(file) load(paste0(dir.prices, "/", file), envir = globalenv()))
  if (length(prices) == 0) stop("ERROR: No match for product ", product, " and date ", date, " and price.file = ", price.file)
  if (length(prices) >  1) stop("ERROR: Multiple matches for product ", product, " date ", date, " and price.file = ", price.file)
  x <- get(prices, envir = globalenv())
  return(x)
}
PlotQMTrades <- function(trades, chart.type) {
  ####################################
  ## Not sure if I need this:
  ### need to offset to get trades to line up with the bars correctly
  ### using a .04 multiplier times the periodicity.  .05 was too much, causing ordering issues
  # trades <- align.time(trades, 60 * periodicity)  # get trades to line up with prices
  # trades <- make.time.unique(trades, eps = (60 * periodicity * .04))
  ####################################
  if (chart.type == "candle" || chart.type == "line") PlotFun <- add_TA
  if (chart.type == "bar") {
    warning("Warning: can't plot trades with chart.type == bar")
    return(list()) # addTA
  }
  stopifnot("long.entry"  %in% colnames(trades),
            "long.exit"   %in% colnames(trades),
            "short.entry" %in% colnames(trades),
            "short.exit"  %in% colnames(trades))
  plot.LX <- PlotFun(trades$long.exit,   pch = 21, lwd = 7, type = "p", cex = 1.5, bg = HarpColors(3), col = HarpColors(3) , on = 1)
  plot.SX <- PlotFun(trades$short.exit,  pch = 21, lwd = 7, type = "p", cex = 1.5, bg = HarpColors(1), col = HarpColors(1)   , on = 1)
  plot.LE <- PlotFun(trades$long.entry,  pch = 24, lwd = 2, type = "p", cex = 1.5, bg = HarpColors(3), col = HarpColors(3) , on = 1)
  plot.SE <- PlotFun(trades$short.entry, pch = 25, lwd = 2, type = "p", cex = 1.5, bg = HarpColors(1), col = HarpColors(1)   , on = 1)
  return(list(plot.LE = plot.LE, plot.LX = plot.LX, plot.SE = plot.SE, plot.SX = plot.SX))
}
PlotQMStopsAndTargets <- function(trades, chart.type) {
  if (chart.type == "candle" || chart.type == "line") PlotFun <- add_TA
  if (chart.type == "bar") {
    warning("Warning: can't plot stops/targets with chart.type == bar")
    return(list()) # addTA
  }
  stopifnot("stopPrice.strat"  %in% colnames(trades),
            "targPrice.strat"    %in% colnames(trades))
  if (all(is.na(trades$stopPrice.strat))) {
    stops   <- NULL
  } else {
    stops   <- PlotFun(trades$stopPrice.strat, lty = 3, lwd = 1, type = "l", cex = 1.5, bg = "green", col = "black", on = 1)
  }
  if (all(is.na(trades$targ1Price.strat))) {
    targets <- NULL
  } else {
    targets <- PlotFun(trades$targ1Price.strat,   lty = 3, lwd = 1, type = "l", cex = 1.5, bg = "red",   col = "black", on = 1)
  }
  return(list(stops = stops, targets = targets))
}
# PlotQMIndicator <- function(ind.xts, type = "line", color = "black", size = 1, chart.type = "candle") {
#   if (chart.type == "candle" || chart.type == "line") PlotFun <- add_TA
#   if (chart.type == "bar") addTA
#   stopifnot(is.xts(ind.xts))
#   ind <- PlotFun(ind.xts, lty = quote(type), lwd = quote(size), type = quote(type), cex = quote(size), bg = quote(color), col = quote(color), on = NA)
#   return(ind)
# }
PlotGGStopsAndTargets <- function(df) {
  #if (!"stopPrice.strat"  %in% colnames(df)) cat("\nstopPrice.strat column is not in the chart.df")
  #if (!"targ1Price.strat" %in% colnames(df)) cat("\ntarg1Price.strat column is not in the chart.df")
  #stops   <- geom_line(data = df, aes(x = Index, y = stopPrice.strat),  na.rm = T, size = 1.5, shape = 24, alpha = .30, linetype = 1, colour = HarpColors(1)) #colour = "black")
  #targets <- geom_line(data = df, aes(x = Index, y = targ1Price.strat), na.rm = T, size = 1.5, shape = 21, alpha = .30, linetype = 1, colour = HarpColors(3)) #colour = "black")
  #return(list(stops = stops, targets = targets))
  if (!"longStopPrc"   %in% colnames(df)) cat("\nlongStopPrc column is not in the chart.df")
  if (!"shortStopPrc" %in% colnames(df))  cat("\nshortStopPrc column is not in the chart.df")
  if (!"longTargPrc" %in% colnames(df))   cat("\nlongTargPrc column is not in the chart.df")
  if (!"shortTargPrc" %in% colnames(df))  cat("\nshortTargPrc column is not in the chart.df")
  lStops   <- geom_line(data = df, aes(x = Index, y = longStopPrc),  na.rm = T, size = 1.5, shape = 24, alpha = .30, linetype = 1, colour = HarpColors(1)) #colour = "black")
  sStops   <- geom_line(data = df, aes(x = Index, y = shortStopPrc), na.rm = T, size = 1.5, shape = 24, alpha = .30, linetype = 1, colour = HarpColors(1)) #colour = "black")
  lTargs   <- geom_line(data = df, aes(x = Index, y = longTargPrc),  na.rm = T, size = 1.5, shape = 21, alpha = .30, linetype = 1, colour = HarpColors(3)) #colour = "black")
  sTargs   <- geom_line(data = df, aes(x = Index, y = shortTargPrc), na.rm = T, size = 1.5, shape = 21, alpha = .30, linetype = 1, colour = HarpColors(3)) #colour = "black")
  return(list(lStops = lStops, sStops = sStops, lTargs = lTargs, sTargs = sTargs))
}
PlotGGTrades <- function(df) {
  ####################################
  ## Not sure if I need this:
  ### need to offset to get trades to line up with the bars correctly
  ### using a .04 multiplier times the periodicity.  .05 was too much, causing ordering issues
  # trades <- align.time(trades, 60 * periodicity)  # get trades to line up with prices
  # trades <- make.time.unique(trades, eps = (60 * periodicity * .04))
  ####################################
  if (!"long.entry"  %in% colnames(df)) cat("\nlong.entry column is not in the chart.df")
  if (!"long.exit"   %in% colnames(df)) cat("\nlong.exit column is not in the chart.df")
  if (!"short.entry" %in% colnames(df)) cat("\nshort.entry column is not in the chart.df")
  if (!"short.exit"  %in% colnames(df)) cat("\nshort.exit column is not in the chart.df")
  plot.LE <- geom_point(data = df, aes(x = Index, y = long.entry),  na.rm = T, size = 4, shape = 24, alpha = .80, fill = HarpColors(3), colour = "black")
  plot.LX <- geom_point(data = df, aes(x = Index, y = long.exit),   na.rm = T, size = 6, shape = 21, alpha = .80, fill = HarpColors(3), colour = "black")
  plot.SE <- geom_point(data = df, aes(x = Index, y = short.entry), na.rm = T, size = 4, shape = 25, alpha = .80, fill = HarpColors(1),   colour = "black")
  plot.SX <- geom_point(data = df, aes(x = Index, y = short.exit),  na.rm = T, size = 6, shape = 21, alpha = .80, fill = HarpColors(1),   colour = "black")
  return(list(plot.LE = plot.LE, plot.LX = plot.LX, plot.SE = plot.SE, plot.SX = plot.SX))
}
PlotGGCandleChart <- function(symbol, chart.title = symbol, up.color = HarpColors(3), dn.color = HarpColors(1), plot.trades = FALSE, plot.stops.targets = FALSE, plot.exitEMA = FALSE, exitEMA.color = "black", exitEMA.size = 1, plot.trendInd = FALSE, trendInd.color = "black", trendInd.size = 1.2) {
  # Get prices and convert
  library(ggplot2)
  if (plot.trades) {
    #up.color = HarpColors(1, set = "ltcandle")
    #dn.color = HarpColors(3, set = "ltcandle")
    #up.color = HarpColors(7, set = "pastel")
    #dn.color = HarpColors(8, set = "pastel")
    up.color = "lightgray"
    dn.color = "lightslategray"
  }
  if (is.xts(symbol)) {
    x      <- symbol
    #chart.title <- quote(symbol)
  } else if (is.character(symbol)) {
    x <- get(symbol)
  }
  stopifnot(is.OHLC(x), is.xts(x))
  x.df <- fortify.zoo(x)

  # Make Candles
  fill.color <- rep("up", nrow(x.df))
  fill.color[x.df$Close < x.df$Open] <- "down"
  candle.info <- data.frame(candle.bottom = pmin(x.df$Open, x.df$Close),
                            candle.top = pmax(x.df$Open, x.df$Close),
                            fill.color = fill.color)
  x.df <- cbind(x.df, candle.info)

  # Construct graph
  head(x.df)
  output <- ggplot(environment = environment(), data = x.df, aes(x = Index, y = Close, ymin = Low,
                                    ymax = High, lower = candle.bottom,
                                    middle = 0, upper = candle.top)) +
    geom_boxplot(stat = 'identity', colour = "black",
                 aes(fill = fill.color, color = factor(fill.color),
                     group = Index)) +
    scale_fill_manual(values = c(dn.color, up.color, 'white')) +
    theme_bw() +
    guides(fill = FALSE, color = FALSE) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    ggtitle(chart.title) +
    coord_cartesian(ylim = c((min(x.df$Low) - .05 * min(x.df$Low)),
                             (max(x.df$Hi   + .05 * max(x.df$Hi))))) +
    theme(plot.title = element_text(hjust = 0))

  # Add trades
  if (plot.trades) {
    t <- PlotGGTrades(x.df)
    output <- output + t$plot.LX + t$plot.SX + t$plot.LE + t$plot.SE
  }
  # Add exit EMA
  if (plot.exitEMA) {
    if (!"exitEMA.strat" %in% colnames(x.df)) cat("\nexitEMA.strat column is not in the chart.df")
    plot.ema <- geom_line(data = x.df, aes(x = Index, y = exitEMA.strat), size = exitEMA.size, alpha = .50, line.type = 1, colour = exitEMA.color, na.rm = TRUE)
    output <- output + plot.ema
  }
  # Add moving average
  if (plot.trendInd) {
    if (!"trend.ind.strat" %in% colnames(x.df)) cat("\ntrend.ind.strat column is not in the chart.df")
    plot.ti <- geom_line(data = x.df, aes(x = Index, y = trend.ind.strat), size = trendInd.size, alpha = .50, line.type = 1, colour = trendInd.color, na.rm = TRUE)
    output <- output + plot.ti
  }
  # Add stops & targets
  if (plot.stops.targets) {
    st <- PlotGGStopsAndTargets(x.df)
    #output <- output + st$stops + st$targets
    output <- output + st$lStops + st$sStops + st$lTargs + st$sTargs
  }
  return(output)
}
PlotGGLineChart <- function(symbol, chart.title = symbol, line.color = HarpColors(5), plot.trades = FALSE, plot.stops.targets = FALSE, plot.exitEMA = FALSE, exitEMA.color = "black", exitEMA.size = 1, plot.trendInd = FALSE, trendInd.color = "black", trendInd.size = 1.2, line.type = "l") {
  # Get prices and convert
  library(ggplot2)
  if (is.xts(symbol)) {
    x <- symbol
    #chart.title <- quote(symbol)
  } else if (is.character(symbol)) {
    x <- Cl(get(symbol))
  }
  stopifnot(is.xts(x))
  x.df <- fortify.zoo(x)
  if (line.type == "l") {
    my.line <- geom_line(colour = line.color, size = 1.3)
  } else if (line.type == "a") {
    my.line <- geom_area(size = 1.1, alpha = 1, fill = line.color)
  }
  # Construct graph
  output <- ggplot(environment = environment(), x.df, aes(x = Index, y = Close)) +
    my.line +
    ggtitle(chart.title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.title = element_blank(), legend.position = "none")
  # Add trades
  if (plot.trades) {
    t <- PlotGGTrades(x.df)
    output <- output + t$plot.LX + t$plot.SX + t$plot.LE + t$plot.SE
  }
  # Add exit EMA
  if (plot.exitEMA) {
    if (!"exitEMA.strat" %in% colnames(x.df)) cat("\nexitEMA.strat column is not in the chart.df")
    plot.ema <- geom_line(data = x.df, aes(x = Index, y = exitEMA.strat), size = exitEMA.size, alpha = .50, line.type = 1, colour = exitEMA.color, na.rm = TRUE)
    output <- output + plot.ema
  }
  # Add moving average
  if (plot.trendInd) {
    if (!"trend.ind.strat" %in% colnames(x.df)) cat("\ntrend.ind.strat column is not in the chart.df")
    plot.ti <- geom_line(data = x.df, aes(x = Index, y = trend.ind.strat), size = trendInd.size, alpha = .50, line.type = 1, colour = trendInd.color, na.rm = TRUE)
    output <- output + plot.ti
  }
  # Add stops & targets
  if (plot.stops.targets) {
    st <- PlotGGStopsAndTargets(x.df)
    output <- output + st$stops + st$targets
    #output <- output + st$lStops +st$sStops + st$targets
  }
  return(output)
}
PlotGGVolume <- function(ohlcv, mov.avg = NULL) {
  x <- OHLCV(ohlcv)
  stopifnot(is.OHLCV(x))
  ## find out if close was up or down
  fill.color <- ifelse(Cl(x) >= Op(x), T, F)
  colnames(fill.color) <- "Fill"
  x <- cbind(x, fill.color)
  ## add moving average
  if (is.null(mov.avg)) {
    plot.ma <- NULL
  } else {
    ma <- EMA(Vo(x), n = mov.avg)
    colnames(ma) <- c("Mov.Avg")
    x <- cbind(x, ma)
    plot.ma <- geom_line(aes(x = Index, y = Mov.Avg), alpha = .7, na.rm = T, colour = "black", size = 1.5)
  }
  x <- fortify.zoo(x)
  plot.vol <- ggplot(environment = environment(), x, aes(x = Index, y = Volume, na.rm = T)) +
  geom_bar(aes(x = Index, y = Volume, fill = factor(Fill)), stat = "identity", size = .3, alpha = .8, colour = "black") +
    scale_fill_manual(values = c(HarpColors(3), HarpColors(1))) +
    theme_bw() +
    guides(fill = FALSE, color = FALSE) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    ggtitle("Volume") +
    theme(plot.title = element_text(hjust = 0)) +
    plot.ma
  return(plot.vol)
}
PlotGGIndicator <- function(x, chart.title = NULL, column = 2, type = "l", color = 1, size = 1, up.thresh = NULL, dn.thresh = NULL) {
  # plotting function for GG indicators
  # args
  #   type      either l for line, a for area, h for histogram
  #chart.title <- NULL
  library(xts)
  library(ggplot2)
  if (is.xts(x)) x <- fortify.zoo(x)
  up.thresh <- if (!is.null(up.thresh)) geom_hline(yintercept = up.thresh, alpha = 1, colour = "black")
  dn.thresh <- if (!is.null(dn.thresh)) geom_hline(yintercept = dn.thresh, alpha = 1, colour = "black")
  my.ind2   <- NULL
  if (type == "l") my.ind <- geom_line(size = size, alpha = 1, colour = HarpColors(color))
  if (type == "h") my.ind <- geom_bar(stat = "identity", size = .3, alpha = .8,  fill = HarpColors(color), colour = "black")
  if (type == "a") {
    my.ind <- geom_area(size = size, alpha = 1, fill = HarpColors(color))
    my.ind2 <- geom_line()
  }
  plot.ind <- ggplot(environment = environment(), x, aes(x = Index, y = x[, column], na.rm = T)) +
    my.ind + my.ind2 +
    theme_bw() +
    #guides(fill = FALSE, color = FALSE) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    ggtitle(chart.title) +
    theme(plot.title = element_text(hjust = 0)) +
    up.thresh + dn.thresh
  return(plot.ind)
}
PlotPrices <- function(symbol, chart.type = "candle", method = "qm",
                       chart.title = symbol, line.color = HarpColors(5),
                       up.color = HarpColors(3), dn.color = HarpColors(1),
                       plot.trades = FALSE, plot.stops.targets = FALSE,
                       plot.exitEMA = FALSE, exitEMA.color = HarpColors(4), exitEMA.size = 1,
                       plot.trendInd = FALSE, trendInd.color = HarpColors(2), trendInd.size = 1.5) {
  ## NOTE: was unable to pass in chart arguments to quantmod methods, even if i specified
  ## the environment as the calling environment
  if (method == "qm") {
    library(quantmod)
    my.theme               <- chart_theme()
    if (plot.trades) {
      my.theme$col$dn.col    <- 'lightslategray'
      my.theme$col$up.col    <- 'lightgray'
      my.theme$col$dn.border <- 'lightgray'
      my.theme$col$up.border <- 'lightgray'
    } else {
      my.theme$col$dn.col    <- HarpColors(1)
      my.theme$col$up.col    <- HarpColors(3)
      my.theme$col$dn.border <- 'lightgray'
      my.theme$col$up.border <- 'lightgray'
    }
if (chart.type == "candle") {
      x <- chart_Series(symbol, name = chart.title, theme = my.theme)
      plotTA <- add_TA
    } else if (chart.type == "line") {
      x <- chart_Series(x = Cl(symbol), type = "line", name = chart.title)
      plotTA <- add_TA
    } else {
      # bar chart
      x <- chartSeries(environemnt = environment(), x = OHLC(symbol), type = "bars", theme = "white", name = chart.title)
      plotTA <- addTA
    }
    if (plot.trades || plot.stops.targets || plot.exitEMA || plot.trendInd) {
      if (plot.exitEMA && "exitEMA.strat" %in% colnames(symbol) && all(is.na(symbol$exitEMA.strat)) == FALSE) {
        ema <- plotTA(symbol$exitEMA.strat, on = 1, type = "l", col = HarpColors(4),
                      lwd = 1)#, legend = NULL, name = NULL)
      } else ema <- NULL
      if (plot.trendInd && "trend.ind.strat" %in% colnames(symbol) && all(is.na(symbol$trend.ind.strat)) == FALSE) {
        ti <- plotTA(symbol$trend.ind.strat, on = 1, type = "l", col = HarpColors(2),
                      lwd = 2)#, legend = NULL, name = NULL)
      } else ti <- NULL
      t    <- ifelse(plot.trades,
                     PlotQMTrades(symbol, chart.type), list())
      stpt <- ifelse(plot.stops.targets,
                     PlotQMStopsAndTargets(symbol, chart.type), list())
      return(list(ohlcv = x, long.exit = t$plot.LX, short.exit = t$plot.SX,
                  long.entry = t$plot.LE, short.entry = t$plot.SE,
                  stops = stpt$stops, targets = stpt$targets, ExitEMA = ema, trendInd = ti))
    }
  } else {
    library(ggplot2)
    if (chart.type == "candle") {
      x <- PlotGGCandleChart(symbol, chart.title, up.color, dn.color, plot.trades, plot.stops.targets, plot.exitEMA, exitEMA.color, exitEMA.size, plot.trendInd, trendInd.color, trendInd.size)
    } else if (chart.type == "line") {
      x <- PlotGGLineChart(symbol, chart.title, line.color, plot.trades, plot.stops.targets, plot.exitEMA, exitEMA.color, exitEMA.size, plot.trendInd, trendInd.color, trendInd.size)
    } else {
      cat("\nggplot2 package only supports candle or line")
    }
  }
  return(x)
}

## END FUNCTIONS LIST ##




