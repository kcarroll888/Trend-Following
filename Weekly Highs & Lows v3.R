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
  # Accepts a date in character format. "%Y-%m-%d
  # If it is uses the prior Friday
  # Returns the date in numeric sequence suitable for Quantmod calls
  
  dte <- as.Date(dte)
  
  subtract <- switch(weekdays(dte),
                     "Saturday"=1,
                     "Sunday"=2,
                     "Monday"=3,
                     "Tuesday"=4,
                     "Wednesday"=5,
                     "Thursday"=6,
                     "Friday"=0)
  dte <- format(dte - subtract, format="%Y%m%d")
  
  # Check date isn't weekend
  # if(weekdays(dte)=="Saturday"){
  #    dte <- format(dte - 1, format="%Y%m%d")
  
  #} else if(weekdays(dte)=="Sunday"){
  #    dte <- format(dte - 2, format="%Y%m%d")
  
  #} else dte <- format(dte, format="%Y%m%d")
  
  # Convert and return
  as.numeric(dte)
}

returnSD <- function(symbol){
    # Calculate the change in % return week to week and comapre to
    # standard deviation of the 40 week average of return
    
    # How many standard deviations is the last weeks return compared to past 40 weeks?
    
    endDate <- getLastFri(Sys.Date())
    startDate <- as.Date(format(endDate), format="%Y%m%d") - 1120 # 40 * 28
    startDate <- getLastFri(startDate)
    
    symD <- getYahooData(symbol, startDate, endDate, adjust=TRUE)
    
    symW <- toWeekly(symD, keepLast=F)
    
    returns <- dailyReturn(symW)
    
    # only need last 40 occurances, if we have them
    if(nrow(symW) > 40){
        symW <- symW[(nrow(symW)-40):nrow(symW)]
    }
    
    # standard deviation of the weekly returns
    # sdRtn <- sd(returns, na.rm=TRUE)
    # Want z-scores (normalising) the data
    zRtn <- scale(returns)
    
    # how does the last week return compare to the sd?
    tail(zRtn, 1)[[1]]
}

chartBreakout <- function(symbol, period=40, timeF="W", endDate=NA) {
  # Function to graph the breakout using Max & Min bands
  # symbol - yahoo ticker
  # period - period over which to draw Max & Min lines. Integer.
  # time - timeframe. Character "W" for weekly, "D" for daily
  
  # Is the date specified? If not use todays date
  if(is.na(endDate)){
    # No specified end date so work out end and start dates
    endDate <- getLastFri(Sys.Date()) # as character string
    
  } else {
    # Date is specified so use this
    endDate <- getLastFri(endDate)
  }
  
  if(timeF=="W") {
    # Work out the how much data to get for the weekly period specified
    # Get the data & turn into a weekly period
    
    # Weekly, so work out how far back to go
    startDate <- as.Date(format(endDate), format="%Y%m%d") - (period * 28)
    startDate <- getLastFri(startDate) # as character string
    
    # Get the data from yahoo
    symbolData <- getYahooData(symbol, startDate,
                               endDate, adjust=TRUE)
    
    # Convert to weekly format
    symbolData <- toWeekly(symbolData[,c("Open", "High", "Low", "Close", "Volume")],
                           keepLast=FALSE)
    
  } else if(timeF=="D"){
    # Work out the how much data to get for the daily period specified
    startDate <- as.Date(format(endDate), format="%Y%m%d") - (period * 4)
    startDate <- getLastFri(startDate)
    
    # Get the data from yahoo
    symbolData <- getYahooData(symbol, startDate,
                               endDate, adjust=TRUE)
    
  } else stop("Timeframe must be 'W' for weekly or 'D' for daily")
  
  # Plot the chart
  setDefaults(chartSeries, up.col="green", dn.col="blue",
              theme="black")
  chartSeries(symbolData, name=symbol, TA=NULL)
  
  # Add Donchian Channel 1 means on main chart
  don <- DonchianChannel(symbolData[,2:3], n=period)
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
  
  # Return the most recent 2 x ATR in the data frame
  as.numeric(2 * last(atr$atr))
}

getBreakouts <- function(fileName, lookBack=40) {
    # Get the weekly breakouts and breakdowns of the stocks contained in 'filename'
    # based on looking back 'lookBack' weeks
    
    # Expects file to be type CSV in the following format;
    #   Bloomberg Code - Stocks Bloomberg Ticker
    #   Yahoo Code - Stocks Yahoo Ric
    
    # Returns the stocks making new highs/lows over the weekly 'lookBack' supplied
    # Read in the file of tickers
    stockList <- read.csv(fileName, stringsAsFactors=FALSE)
    
    numberStocks <- nrow(stockList)
    
    # And how far we look back will determine start date of
    # data retrieval. Make sure the dates are weekdays otherwise
    # yahoo returns errors. Find the last Friday
    
    endD <- getLastFri(Sys.Date())
    endDate <- as.character(endD)
    endDate <- as.Date(endDate, format="%Y%m%d")
    
    startDate = endDate - (lookBack * 14)
    startD <- format(startDate, format="%Y%m%d") # Convert start date to character
    startD <- as.numeric(startD)                 # and then numeric for quantmod call
    
    # Create dataframes to store the highs & lows
    newHighLow <- data.frame(1,1,1,1,1)
    colnames(newHighLow) <- c("Yahoo", "Bloomberg", "Type", "ATR", "Close")
    rowHL <- 1
    
    # Loop through all the stocks and check if they've
    # reached a high or low compared to current observation
    for(i in 1:numberStocks){
        
        # Get current symbol
        curSymbol <- stockList[i,2]
        print(curSymbol)
        if(curSymbol == "SAN.PA" | curSymbol == "JMAT.L"){
            # Problem with the Yahoo Data so skip this(ese) symbol(s)
            print("skipped")
            next
        }
        
        # Get daily data for current symbol from Yahoo
        curStockData <- getYahooData(curSymbol, startD, endD,
                                     adjust=TRUE)
        
        # Store todays date to compare against
        curDateRow <- tail(curStockData, 1)
        
        # Convert this dataframe to weekly data
        curStockData <- toWeekly(curStockData[,c("Open", "High", "Low", "Close", "Volume")],
                                 keepLast=TRUE)
        # names(curStockData) <- c("Open", "High", "Low", "Close")
        
        # Remove this row from the series & save to compare
        curStockData <- curStockData[-nrow(curStockData),]
        
        # Now test for highs & lows of the previous weeks
        # First check if have enough data to look back 40 weeks
        sr <- ifelse(nrow(curStockData) > 40, nrow(curStockData) - 40, 1)
        curMax <- unique(seriesHi(curStockData$Close[sr:nrow(curStockData)]))
        curMin <- unique(seriesLo(curStockData$Close[sr:nrow(curStockData)]))
        
        # Does current weekly close make a new high or low?
        if(as.numeric(curDateRow$Close) >= as.numeric(curMax)){
            
            # Yes so save the ticker in the output data frame
            newHighLow[rowHL, 1:3] <- c(curSymbol,
                                        stockList[i, "Bloomberg.Code"],
                                        "High")
            
            newHighLow$Close[rowHL] <- as.numeric(curDateRow$Close)
            
            # Put the stop loss in the data frame
            newHighLow$ATR[rowHL] <- getStopLoss(curStockData, lookBack)
            
            # Ready for the next row of output dataframe
            rowHL <- rowHL + 1
            
        } else if(as.numeric(curDateRow$Close) <= as.numeric(curMin)){
            
            # If its made a new low then save in output data frame
            newHighLow[rowHL, 1:3] <- c(curSymbol,
                                        stockList[i, "Bloomberg.Code"],
                                        "Low")
            
            newHighLow$Close[rowHL] <- as.numeric(curDateRow$Close)
            
            # Put the stop loss in the data frame
            newHighLow$ATR[rowHL] <- getStopLoss(curStockData, lookBack)
            
            # Ready for the next row of output dataframe
            rowHL <- rowHL + 1
        }
    }
    
    return(newHighLow)
}

stopLossTriggered <- function(){
    openPos <- c("AAPL", "ADM", "ADN.L", "ATK.L", "CBK.DE", "CNA.L", "DNLM.L", "ERM.L", "ETO.L",
                 "GKN.L", "HSIC", "HP", "IBM", "IMI.L", "IPF.L", "ITRK.L", "LLOY.L", "LHA.DE", "MBLY",
                 "NTG.L", "NXP", "PAG.L", "POP.MC", "RB.L", "RTO", "SYMC", "TDC", "TSCO.L", "TRV", "VWS.CO")
    
    closeOut <- data.frame(1, 1)
    colnames(closeOut) <- c("Symbol", "RtnStdDev")
    index <- 1
    for(symb in openPos){
        print(symb)
        sd <- returnSD(symb)
        if(sd >= 2){
            closeOut[index, 1] <- symb
            closeOut[index, 2] <- sd
            index <- index + 1
        }
    }
    closeOut
}

weeklyRun <- function(risk=500){
  # Get the yahoo data and calculate ATR based stop-loss in 2 calls
  # otherwise Yahoo servers stop allowing access
  print("Updating Weekly Breakouts")
  A <- getBreakouts("Equity Universe A 2015 Sep.csv", 40)
  B <- getBreakouts("Equity Universe B 2015 Sep.csv", 40)
  
  print("Calculating ATR & Risk")
  # Make the ATR a numeric value so can arrange/manipulate
  A$ATR <- as.numeric(A$ATR)
  B$ATR <- as.numeric(B$ATR)
  
  # Combine the two data frames
  final <- rbind(A, B)
  
  # Work out the volatility risk of the breakout
  final$Risk <- final$Close / final$ATR
  
  # Work out the pounds per point position size based on £500 (default) risk
  final$Stake <- risk / final$ATR
  
  # Sort by breakout type (High / Low), Risk and ATR
  final <- arrange(final, Type, Risk, ATR)
  unique(final)
  
  # Save the data frame
  print("Saving ideas to New Ideas.csv")
  write.csv(final, "New Ideas.csv", row.names=FALSE)
}