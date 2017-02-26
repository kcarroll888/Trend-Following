# Get all the files in the New Ideas
# and calculate and plot the highs vs lows
library(dplyr)
library(ggplot2)

getFiles <- function(path=".", extn=".csv"){
  # Return the csv files in a directory given
  # Get the list of files only in the current directory
  fInfo <- file.info(dir(path))
  fInfo <- mutate(fInfo, Filename=row.names(fInfo))
  
  # Remove directories
  fInfo <- filter(fInfo, isdir==FALSE)
  
  # Only want csv files
  fInfo <- filter(fInfo, grepl(extn, Filename))
  return(select(fInfo, Filename))
}

weeklyCount <- function(filename){
  # For the given file count the number of highs vs lows
  f <- read.csv(filename, stringsAsFactors = F)
  tbl <- table(f$Type)
  return(data.frame(High=tbl[[1]], Low=tbl[[2]], stringsAsFactors = F))
}

extractNames <- function(v){
  # Extract the numbers from the dataframe supplied
  
  # Turn into character vector
  n <- unlist(strsplit(f$Filename, split=" "))
  
  # remove the lines which don't have any numbers in them
  loc <- grepl("\\d", n)
  n <- n[loc]
  
  # now extract only the numbers as we know they're
  # in a sequence of 8
  n <- regmatches(n, gregexpr("\\d{8}", n))
  return(n)
}

getHighLowCounts <- function(){
  f <- getFiles()
  w <- lapply(f$Filename, weeklyCount)
  highs <- sapply(w, function(h) h[[1]][[1]])
  lows <- sapply(w, function(l) l[[2]][[1]])
  diff <- highs - lows
  dte <- extractNames(f$Filename)
  dte <- sapply(dte, function(d) d[[1]])
  return(data.frame(Date=dte, High=highs, Lows=lows, Diff=diff,
                    stringsAsFactors = F))
}

c <- getHighLowCounts()
ggplot(c, aes(x=Date, y=Diff)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
