##Author: Bikash Agrawal
##Description: Create final result from log, or pre-processing of log file.


library(lattice)
library(plyr)
library(depmixS4)
library(TTR) # For downloading SP500 index
library(ggplot2)
library(reshape2)
library(xts)
Data1 = read.table("file/error_25.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data1$hour <- cut(as.POSIXlt( Data1$date,  origin="1970-01-01" ), breaks = "hour")
getobsveration <- function(Df) { c(paste(Df$ErrorType, collapse=","))  }
Output1 <- ddply(Data1, .(day),getobsveration)
Sys.setenv(tz = "UTC")

##Plot number of observation Error Vs time
Data1 = read.table("file/error_25.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data1$day <- cut(as.POSIXlt( Data1$date,  origin="1970-01-01" ), breaks = "day")
getcount <- function(Df) { c(count = length(Df$ErrorType))  }
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

# plot graph
x1 = c(1:21)
y1 = ts1$count
x2 = x1
y2 = ts2$count
x3 = x1
y3 = ts3$count
x4 = x1
y4 = ts4$count
x5 = x1
y5 = ts5$count
x = ts1$day

##create final data frame




## Plot graph for Error vs time
pdf("graph/obs.pdf",bg="white")
#g_range <- range(0, y1, y2, y3, y4, y5)

g_range <- range(0, 100)
x_range <- range(0, x1, x2, x3, x4, x5)
# Graph cars using a y axis that ranges from 0 to 12
plot(NULL, ylim=g_range, xlim=x_range, xlab="Date", ylab="# of errors", xaxt="n")

lines(ts1$day, y1, lwd=1, col="red")
lines(ts2$day, y2, lwd=1, col="black")
lines(ts3$day, y3, lwd=1, col="blue")
lines(ts4$day, y4, lwd=1, col="brown")
lines(ts5$day, y5, lwd=1, col="green")
#axis(1, at=ts1$day, lab=x)
#axis(1,at=NULL, labels=F)
a = as.POSIXct(ts1$day, origin="1970-01-01")
axis(side=1, at=ts1$day,   labels=format(a, '%Y-%m-%d'))

#tickpos <- seq(as.POSIXct("2014-11-11", tz="GMT"), as.POSIXct("2014-11-30", tz="GMT"), by="day")
#axis.POSIXct(side=1, at=tickpos)


grid(14,15,lty=2)
## add extra space to right margin of plot within frame
legend ("topleft", legend =c("Haisen25", "Haisen26", "Haisen27", "Haisen28", "Haisen29" ), 
        cex=0.8, col =c("red","black","blue","brown","green"),lwd=c(1,1,1,1,1), lty=c(1,1,1,1,1))
# Create a title with a red, bold/italic font
title(main="Observations", col.main="black", font.main=4)

dev.off()



#####################################################################
# Hidden Markov model for Failure Prediction
# Author: Bikash Agrawal
# Date: 2nd Dec 2014
# Description:..................
#
######################################################################
#dir = "/Users/bikash/repos/FailurePrediction/R" # path for macbook
dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
setwd(dir)
##Plot number of observation Error Vs time
Data1 = read.table("file/error_25.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data1$hour <- cut(as.POSIXlt( Data1$date,  origin="1970-01-01" ), breaks = "hour")
getcount <- function(Df) { c(count = length(Df$ErrorType))  }
ts1 <- ddply(Data1, .(hour),getcount)

## Data from haisen 26
Data2 = read.table("file/error_26.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data2$hour <- cut(as.POSIXlt( Data2$date,  origin="1970-01-01" ), breaks = "hour")
ts2 <- ddply(Data2, .(hour),getcount)

## Data from haisen 27
Data3 = read.table("file/error_27.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data3$hour <- cut(as.POSIXlt( Data3$date,  origin="1970-01-01" ), breaks = "hour")
ts3 <- ddply(Data3, .(hour),getcount)

## Data from haisen 28
Data4 = read.table("file/error_28.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data4$hour <- cut(as.POSIXlt( Data4$date,  origin="1970-01-01" ), breaks = "hour")
ts4 <- ddply(Data4, .(hour),getcount)

## Data from haisen 29
Data5 = read.table("file/error_29.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data5$hour <- cut(as.POSIXlt( Data5$date,  origin="1970-01-01" ), breaks = "hour")
ts5 <- ddply(Data5, .(hour),getcount)

## Plot graph for Error of haisen25
plot(ts1$hour, ts1$count, ylim=range(0, 100), type='o', xlim=range(0,44) , xlab="Date", ylab="# of errors", xaxt="n")

a = list(Data1$date)
b = list(Data1$ErrorType)
df = matrix(c(unlist(a),unlist(b)),ncol=2,byrow=F)
ep <- endpoints(Data1, on = "hours", k = 1)
dfLR <- df[ep[2:(length(ep)-1)],]
sp500LRdf <- data.frame(dfLR)
sp500LRdf$logret <- log(dfLR[,2]) - lag(log(dfLR[,2]))

sp500LRdf$X1 <- as.POSIXct(sp500LRdf$X1, tz="GMT")
sp500LRdf$Date <-as.Date(row.names(sp500LRdf$X1),"%Y-%m-%d")
ggplot( sp500LRdf, aes(Date) ) + 
  geom_line( aes( y = logret ) ) +
  labs( title = "S&P 500 log Returns")

mod <- depmix(response = ErrorType ~ 1, data = Data1, nstates = 2,trstart = runif(4))


