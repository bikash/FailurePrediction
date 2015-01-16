
library(lattice)
library(plyr)
library(depmixS4)
library(TTR) # For downloading SP500 index
library(ggplot2)
library(reshape2)
library(xts)
library(data.table)
library(reshape)
#dir = "/Users/bikash/repos/FailurePrediction/R" # path for macbots1ok
dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
setwd(dir)
##Plot number of observation Error Vs time
Data1 = read.table("file/error_25.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data1$day <- cut(as.POSIXlt( Data1$date,  origin="1970-01-01" ), breaks = "day")
getcount <- function(Df) { c(count = as.numeric(length(Df$ErrorType)),
                             obs = paste(Df$ErrorType, collapse=","))  
}
ts1 <- ddply(Data1, .(day),getcount)

## Data from haisen 26
Data2 = read.table("file/error_26.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data2$day <- cut(as.POSIXlt( Data2$date,  origin="1970-01-01" ), breaks = "day")
ts2 <- ddply(Data2, .(day),getcount)

## Data from haisen 27
Data3 = read.table("file/error_27.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data3$day <- cut(as.POSIXlt( Data3$date,  origin="1970-01-01" ), breaks = "day")
ts3 <- ddply(Data3, .(day),getcount)

## Data from haisen 28
Data4 = read.table("file/error_28.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data4$day <- cut(as.POSIXlt( Data4$date,  origin="1970-01-01" ), breaks = "day")
ts4 <- ddply(Data4, .(day),getcount)

## Data from haisen 29
Data5 = read.table("file/error_29.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data5$day <- cut(as.POSIXlt( Data5$date,  origin="1970-01-01" ), breaks = "day")
ts5 <- ddply(Data5, .(day),getcount)
## d data from haisen22
Data6 = read.table("file/error_22.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data6$day <- cut(as.POSIXlt( Data6$date,  origin="1970-01-01" ), breaks = "day")
ts6 <- ddply(Data6, .(day),getcount)


## d data from haisen20
Data7 = read.table("file/error_20.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data7$day <- cut(as.POSIXlt( Data7$date,  origin="1970-01-01" ), breaks = "day")
ts7 <- ddply(Data7, .(day),getcount)

## function to get data
## @param merge: if True merge all the data from different cluster
getData = function(merge=TRUE, hour=FALSE)
{
  if(merge)
  {
      ## Merge all data 
      date = unlist(list(ts1$day, ts2$day, ts3$day, ts4$day, ts5$day,ts6$day, ts7$day))
      error = unlist(list(ts1$count,ts2$count,ts3$count,ts4$count,ts5$count, ts6$count, ts7$count))
      obs = unlist(list(ts1$obs,ts2$obs,ts3$obs,ts4$obs,ts5$obs, ts6$obs, ts7$obs))
      data <- data.frame(date,error,obs)
      ## Summing or grouping data of similar date
      data$error = strtoi(data$error)
      data = aggregate(error ~ date, data = data, sum)
      return (data)
  }
}

## function to get raw data
## @param merge: if True merge all the data from different cluster
getrawData = function(pivot =TRUE)
{
    ## Fill empty slot with unfailure data
    len = length(df$date)
    #tseq <- seq(as.POSIXlt(df$date[1], origin="1970-01-01"), as.POSIXlt(df$date[len], origin="1970-01-01"), by="secs")
    tseq <- seq(as.POSIXlt(1415724101, origin="1970-01-01"), as.POSIXlt(1417413321, origin="1970-01-01"), by="secs")
    tseq=as.numeric(as.POSIXct(tseq))
    error = 7
    data.nonerror <- data.frame(tseq,error)
    ## Merge all data 
    date = unlist(list(Data1$date, Data2$date, Data3$date, Data4$date, Data5$date,Data6$date, Data7$date,tseq))
    error = unlist(list(Data1$ErrorType,Data2$ErrorType,Data3$ErrorType,Data4$ErrorType,Data5$ErrorType, Data6$ErrorType, Data7$ErrorType,data.nonerror$error))
    
    data.errorType <- data.frame(date,error)
    ## Summing or grouping data of similar date
    #rawData <- by(error,date,function(x)paste(x,collapse=","))
    dt <- data.table(data.errorType )
    #rawData = dt[,paste(error,collapse=","),by=date]
    if(pivot)
      rawData = dcast(dt, date ~ dt$error, fun.aggregate = sum, value.var = "error") 
    else
      rawData = df
    #rawData = dt[,paste(error,collapse=","),by=date]
    return (rawData)
}

getErrors <- function(Df) 
{ 
  c(a1 = sum(Df$y1),
    a2 = sum(Df$y2),
    a3 = sum(Df$y3),
    a4 = sum(Df$y4),
    a5 = sum(Df$y5),
    a6 = sum(Df$y6)
    )  
}

PloterrorType <- function ()
{
  df = getrawData()
  
  ##change column name
  cols <- c("date","y1","y2","y3","y4","y5","y6")
  colnames(df) <- cols
  
  ### Aggregate per day
  df$day <- cut(as.POSIXlt( df$date,  origin="1970-01-01" ), breaks = "day")
  ErrorType <- ddply(df, .(day), getErrors)
  ErrorType <- data.frame(day=ErrorType$day[2:6], a1=ErrorType$a1[2:6], a2=ErrorType$a2[2:6], a3=ErrorType$a3[2:6], a4=ErrorType$a4[2:6], a5=ErrorType$a5[2:6], a6=ErrorType$a6[2:6])

  # visualize
  pdf("graph/toe.pdf",bg="white")
  df = matrix(c(ErrorType$a1, ErrorType$a2, ErrorType$a3, ErrorType$a4, ErrorType$a5, ErrorType$a6),
              nrow=length(ErrorType$day), ncol=6, 
              dimnames=list(ErrorType$day, c( "Error 1","Error 2", "Error 3", "Error 4", "Error 5", "Error 6")))
  
  df1 = t(df)                                       
  # SIMPLE BARPLOT
  barplot(df1,                             # Data (bar heights) to plot
          beside=TRUE,                            # Plot the bars beside one another; default is to plot stacked bars
          names.arg=ErrorType$day,
         # col=c( "black","black",  "gray", "red","green","blue"),                   # Color of the bars
          border="black",                         # Color of the bar borders
          #main=c("Performance with different type of input format"),      # Main title for the plot
          xlab="Date",                       # X-axis label
          ylab="# of errors",                      # Y-axis label
          ylim = c(0,200),
          density=c(120, 90, 70, 50, 30, 10),
          font.lab=3)                             # Font to use for the axis labels: 1=plain text, 2=bold, 3=italic, 4=bold italic
  
  legend("topright",                               # Add a legend to the plot
         legend=c( "Network connection","Memory overflow", "Security setting", "Java exception", "Java I/O error", "Namenode failure"),             # Text for the legend
         density=c(90, 70, 50, 40, 30, 20),
         #fill=c( "white","black",  "gray", "red","green","blue")
         )  
  
  dev.off()
}



df = getrawData()
## Change column name
cols <- c("date","y1","y2","y3","y4","y5","y6","y7") ## error 1 to 6 and 7 is non error event
colnames(df) <- cols
## Plot Error observation

### model HMM

