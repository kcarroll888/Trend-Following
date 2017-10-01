# FROM THE SPECIFIED EXCEL FILE OF STOCK TICKERS
# Return a list of those which are hitting new 40 week
# Highs or Lows
#
# Excel File contains;
# * Ticker (Bloomberg finance format)
# * ISIN
# * Ticker (Yahoo finance format) Header
# * Various stocks to find out

# Libraries Needed
library(quantmod)   # for getting & working with equity data
library(dplyr)
library(ggplot2)

setDefaults(chartSeries, up.col="green", dn.col="blue",
            theme="black")

toWeekly <- function(xtsDF, keepLast=FALSE) {
  # xts object with Dates as the row index
  
  # convert it to a dataframe to work with base R functions
  DF <- data.frame(xtsDF)
  
  # pull out the dates from the dataframe
  dates <- as.Date(row.names(DF))
  
  # find the row numbers of the Fridays (end of the week)
  daysOfWeek <- weekdays(dates)
  indexOfFri <- which(daysOfWeek == "Friday")
  if(keepLast){
      # keepLast set to TRUE when want to keep the last date given
      # even if is not the last Friday. Useful for when most recent
      # Friday is a holiday but don't want to overlook the 4 days of
      # the week
      indexOfFri[length(indexOfFri) + 1] <- length(daysOfWeek)
  }
  
  # calculate the open, high, low, close and cumulative volume
  # between each Friday
  
  # create data frame to store result
  wDF <- data.frame("Date"=dates[indexOfFri],
                    "Open"=0,
                    "High"=0,
                    "Low"=0,
                    "Close"=0,
                    "Volume"=0)
  
  # indexs to keeptrack of daily & weekly dataframes
  dInd <- 1
  wInd <- 1
  
  for(x in indexOfFri){
    
    # map the data from daily to weekly
    wDF$Open[wInd] <- DF$Open[dInd]
    wDF$High[wInd] <- max(DF$High[dInd:x])
    wDF$Low[wInd] <- min(DF$Low[dInd:x])
    wDF$Close[wInd] <- DF$Close[x]
    wDF$Volume[wInd] <- sum(DF$Volume[dInd:x])
    
    # update indexs for their next locations
    dInd <- x + 1
    wInd <- wInd + 1
    
  }
  
  # Return the weekly dataframe as an xts object
  wDF <- xts(wDF[,c("Open", "High", "Low", "Close", "Volume")],
             order.by=wDF$Date)
  wDF
}

getLastFri <- function(dte){
  # Accepts a date in format. "%Y-%m-%d
  # Returns the date of the prior Friday unless the date specified is a Friday
  # Returns as class date
  
  dte <- as.Date(dte)
  
  subtract <- switch(weekdays(dte),
                     "Saturday"=1,
                     "Sunday"=2,
                     "Monday"=3,
                     "Tuesday"=4,
                     "Wednesday"=5,
                     "Thursday"=6,
                     "Friday"=0)
  return(dte - subtract)

}

chartBreakout <- function(symbol, period=40, timeF="W", ed=Sys.Date()) {
  # Function to graph the breakout using Max & Min bands
  # symbol - yahoo ticker
  # period - period over which to draw Max & Min lines. Integer.
  # time - timeframe. Character "W" for weekly, "D" for daily
  
  endDate <- getLastFri(ed)
  
  if(timeF=="W") {
    # Work out the how much data to get for the weekly period specified
    # Get the data & turn into a weekly period
    
    # Weekly, so work out how far back to go
    startDate <- endDate - (period * 28)
    startDate <- getLastFri(startDate) # as character string
    
    # Get the data from yahoo
    symbolData <- getSymbols(symbol, env=NULL, src = 'yahoo',
                             from=startDate, to=endDate, adjust=TRUE)
    
    # Convert to weekly format
    names(symbolData) <- c("Open", "High", "Low", "Raw Close", "Volume", "Close")
    symbolData <- toWeekly(symbolData[,c("Open", "High", "Low", "Close", "Volume")],
                           keepLast=FALSE)
    
  } else if(timeF=="D"){
    # Work out the how much data to get for the daily period specified
    startDate <- endDate - (period * 4)
    startDate <- getLastFri(startDate)
    
    # Get the data from yahoo
    symbolData <- getSymbols(symbol, env = NULL, src = 'yahoo',
                             from=sd, to=ed, adjust=TRUE)
    names(symbolData) <- c("Open", "High", "Low", "Raw Close", "Volume", "Close")
    
  } else stop("Timeframe must be 'W' for weekly or 'D' for daily")
  
  # Plot the chart

  chartSeries(symbolData, name=symbol, TA=NULL)
  
  # Add Donchian Channel '1' means on main chart
  don <- DonchianChannel(symbolData[,"Close"], n=period)
  addTA(don[,c("high", "low")], on = 1)
  
  # addATR(n=period)
  
  # Add 2*ATR to assess the volatility
  # vol <- 2 * ATR(symbolData[,c("High", "Low", "Close")], n=period)
  # addTA(vol$atr, type="h", legend="2 * Average True Range")
}

getStopLoss <- function(series, lookBack) {
  
  # Test if enough data to work out the ATR
  if(nrow(series) <= lookBack) {
    # No there isn't return NA and quit the function
    return("NA")
  }
  
  # Enough historical data to calculate the ATR
  # Calculate the ATR
  atr <- ATR(series[, c("High", "Low", "Close")], n=lookBack)
  
  # Return the most recent ATR, doubled, in the data frame
  as.numeric(2 * last(atr$atr))
}

returnSD <- function(symbol){
  # Calculate the change in % return week to week and comapre to
  # standard deviation of the 40 week average of return and ATR
  
  # How many standard deviations is the last weeks return and ATR compared to past 40 weeks?
  endDate <- getLastFri(Sys.Date())
  startDate <- endDate - 1120 # 40 * 28
  startDate <- getLastFri(startDate)
  
  symD <- getSymbols(symbol, env = NULL, src = 'yahoo',
                     from=startDate, to=endDate, adjust=TRUE)
  
  symW <- toWeekly(symD, keepLast=F)
  
  ret <- dailyReturn(symW)
  atr <- ATR(symW[,c("High", "Low", "Close")])[,2]
  
  # only need last 40 occurances, if we have them
  if(nrow(symW) > 40){
    symW <- symW[(nrow(symW)-40):nrow(symW)]
  }
  
  # standard deviation of the weekly returns
  zRtn <- scale(ret)
  zATR <- scale(atr)
  
  # Return the last std dev readings of Return & ATR, i.e this week
  r <- cbind(Symbol=symbol, Return=last(zRtn), ATR=last(zATR))
  return(r)
}

stopLossTriggered <- function(Tickers){
  # Given a vector of Tickers, find the standard deviation of the last
  # recording of either "Return" and "ATR".
  t <- lapply(Tickers, returnSD)
  
  # Extract each element of the list returned and turn into a datframe
  sym <- sapply(t, function(l) l[1])
  ret <- sapply(t, function(r) as.numeric(r[2]))
  atr <- sapply(t, function(a) as.numeric(a[3]))

  data.frame(Symbol=sym, Return=ret, ATR=atr)
}

isNewHighLow <- function(ticker, lookBack=40, ed=Sys.Date()){
  # Function to remove the for loop of testing for a breakout
  # For the given Yahoo ticker, lookBack period and end date test to see
  # if a new weekly High/Low was made.
  # Returns a dataframe with type of breakout (High/Low), the close and stop
  # loss if new high/low was made. Otherwise returns empty dataframe
  
  print(ticker)
  
  # Create a result vector to return after function has been run
  result <- vector("list", 1)
  
  # Work out start and end dates & prepare the format for quantmod functions
  # As of June 2017 format for Yahoo date calls is class date: YYYY-MM-DD
  # Which is default return format of Sys.Date() function
  endDate <- getLastFri(ed)
  
  startDate = endDate - (lookBack * 10)   # Enough daily data to convert into weeks
  
  # Get the data from Yahoo
  curStockData <- tryCatch(getSymbols(ticker, env = NULL, src = 'yahoo',
                                      from=startDate, to=endDate, adjust=TRUE),
                           error = function(e){
                                      "error"
                           })
  
  if(class(curStockData)[1] != "xts"){
    print("skipped")
    return(c(ticker, "skip", "skip", "skip"))
  }
  
  # Yahoo data often returns zeros or NA now so use na.fill and 'extend'
  # to interpolate missing values.
  # But need a decent number of rows to interpolate values from so check
  names(curStockData) <- c("Open", "High", "Low", "Unadjust Close", "Volume", "Close")
  naRows <- tabulate(as.factor(is.na(curStockData$Close)), nbins = 2)
  
  # Test that there is enough clean data
  if( (naRows[1] < 0.5 * (naRows[1] + naRows[2])) | (nrow(curStockData) < 40)){
    # Exit because more than 50% of the rows returned are NA
    print("Not enough data")
    return(c(ticker, "data", "data", "data"))
  }
  
  # Now test Yahoo hasn't returned dates > last date called
  curStockData <- curStockData[index(curStockData) < endDate,]
  
  # Don't want duplicate rows
  curStockData <- curStockData[index(curStockData)==unique(unique(index(curStockData))), ]
  
  # Interpolate missing values
  curStockData <- na.fill(curStockData, 'extend')

  # Store last row (most recent observation) to compare against
  curDateRow <- tail(curStockData, 1)
  
  # Convert this dataframe to weekly data
  curStockData <- toWeekly(curStockData[,c("Open", "High", "Low", "Close", "Volume")],
                           keepLast=TRUE)
  
  # Remove the last row from the series & save to compare
  curStockData <- curStockData[-nrow(curStockData),]
  
  # Now test for highs & lows of the previous weeks
  # First check if have enough data to look back 40 weeks
  sr <- ifelse(nrow(curStockData) > 40, nrow(curStockData) - 40, 1)
  curMax <- unique(seriesHi(as.numeric(curStockData$Close[sr:nrow(curStockData)])))
  curMin <- unique(seriesLo(as.numeric(curStockData$Close[sr:nrow(curStockData)])))
  
  # Does current weekly close make a new high or low?
  if(curDateRow$Close >= curMax){
    
    # Yes so save the ticker in the output data frame
    # Also calculate ATR and Weekly Return
    result <- c(ticker, "High", curDateRow$Close,
                getStopLoss(curStockData, lookBack))
    
  } else if(curDateRow$Close <= curMin){
    
    # If its made a new low then save in output data frame
    result <- c(ticker, "Low", curDateRow$Close,
                getStopLoss(curStockData, lookBack))
  } else {
    # No new high or low made. Return the ticker and empty values
    result <- c(ticker, NA, NA, NA)
  }
  
  return(result)
}

breakOuts <- function(fileName, lookBack=40, endDate){
  # read the file of Yahoo tickers
  l <- read.csv(fileName, stringsAsFactors = F)
  
  # test for a breakout in each
  r <- lapply(l$Yahoo, isNewHighLow, lookBack=lookBack, ed=endDate)
  
  # flatten into a dataframe
  df <- data.frame(matrix(unlist(r), nrow=length(r), byrow=T),
                   stringsAsFactors = F)
  colnames(df) <- c("Ticker", "Type", "Close", "Stop")
  
  # add the Bloomberg & Industry data from the original list
  df$Bloomberg <- l$Bloomberg
  df$Industry <- l$Industry
  
  # only want entries with results or 'skip'
  # meaning Yahoo data was incomplete
  i <- which(df$Type=="High" | df$Type=="Low" | df$Type=="skip" | df$Type=="data")
  df <- df[i,]
  
  # return the final result
  return(df)
}

weeklyRun <- function(risk=500, endDate=Sys.Date()){
  # Get the yahoo data and calculate ATR based stop-loss,
  # called  three times otherwise Yahoo servers stop allowing access
  print("Updating Weekly Breakouts")
  A <- breakOuts("Equity Universes/Equity Universe A 2017.csv", 40, endDate)
  B <- breakOuts("Equity Universes/Equity Universe B 2017.csv", 40, endDate)
  C <- breakOuts("Equity Universes/Equity Universe C 2017.csv", 40, endDate)
  
  print("Calculating ATR & Risk")
  # Make the ATR a numeric value so can arrange/manipulate
  A$Stop <- as.numeric(A$Stop)
  B$Stop <- as.numeric(B$Stop)
  C$Stop <- as.numeric(C$Stop)
  
  # Combine the three data frames
  Q <- rbind(A, B)
  final <- rbind(Q, C)
  
  # Work out the volatility risk of the breakout
  final$Risk <- as.numeric(final$Close) / final$Stop
  
  # Work out the pounds per point position size based on £500 (default) risk
  final$Stake <- risk / final$Stop
  
  # Sort by breakout type (High / Low), Risk and ATR
  final <- arrange(final, Type, Risk, Stop)
  final <- unique(final)
  row.names(final) <- NULL
  
  # Save the data frame
  print("Saving ideas to New Ideas.csv")
  fileName <- paste0("Weekly Ideas/New Ideas ",getLastFri(endDate),".csv")
  write.csv(final, fileName, row.names=FALSE)
  final
}


# DISPLAYING THE HIGH AND LOW RESULTS
# displayHigh <- function(i){
  # Requires a dataframe of results from the new results file generated earlier
#  highs <- filter(i, Type=="High")
#  nHigh <- group_by(highs, Industry)
#  nHigh <- summarise(nHigh, number=n())

  # Order by number of breakouts per industry, highest-lowest
#  nHigh$Industry <- factor(nHigh$Industry,
#                           levels=nHigh$Industry[order(nHigh$number, decreasing = T)])
  # Plot the result
#  ggplot(nHigh, aes(x=Industry, y=number, color=number)) + geom_count() +
#    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#    xlab("Industry Group") + ylab("Number of New Highs")
#}

displayHL <- function(i, HL="Low"){
  # Displaying Results Lows
  lows <- filter(i, Type==HL)
  ggplot(lows) + geom_bar(mapping=aes(x=Industry)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5,
                                     vjust = 0.5)) +
    xlab("Industry Group") + ylab(HL)
  
  # nLow <- group_by(lows, Industry)
  # nLow <- summarise(nLow, number=n())
  # Order by number of breakouts per industry, highest-lowest
  # nLow$Industry <- factor(nLow$Industry,
  #                         levels=nLow$Industry[order(nLow$number, decreasing = T)])
  # Plot the result
  # ggplot(nLow) + geom_bar(mapping=aes(x=Industry)) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #   xlab("Industry Group") + ylab(HL)
}
