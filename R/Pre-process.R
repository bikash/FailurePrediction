##Author: Bikash Agrawal
##Description: Create final result from log, or pre-processing of log file.

library(lattice)
library(plyr)
library(depmixS4)
library(TTR) # For downloading SP500 index
library(ggplot2)
library(reshape2)
library(xts)
dir = "/Users/bikash/repos/FailurePrediction/R" # path for macbook
#dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
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
grid(14,15,lty=2)
## add extra space to right margin of plot within frame
legend ("topleft", legend =c("Haisen25", "Haisen26", "Haisen27", "Haisen28", "Haisen29" ), 
        cex=0.8, col =c("red","black","blue","brown","green"),lwd=c(1,1,1,1,1), lty=c(1,1,1,1,1))
# Create a title with a red, bold/italic font
title(main="Observations", col.main="black", font.main=4)

dev.off()


## plot error observation along with failure ***********************
## Merge all data 
date = unlist(list(ts1$day,ts6$day, ts7$day))
error = unlist(list(ts1$count, ts6$count, ts7$count))
data <- data.frame(date,error)
## Summing or grouping data of similar date
data$error = strtoi(data$error)
data = aggregate(error ~ date, data = data, sum)
obs=c(0,1,1,1,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,1,0) ## failure state
x = data$date
y = data$error
## Plot graph for Error vs time
pdf("graph/error_obs.pdf",bg="white")
g_range <- range(0, y)
x_range <- range(0, x)
plot(NULL, ylim=g_range, xlim=x_range, xlab="Date", ylab="# of errors", xaxt="n")
lines(ts1$day, y, lwd=1, col="black")
for(i in 1:x_range[2]) {
  if(obs[i] >0)
    points(x[i], obs[i], pch = 18, col = "red", bg = "yellow",  cex = 1.2)
}

#points(x,obs, col = "red",pch = 18, bg = "grey")
a = as.POSIXct(x, origin="1970-01-01")
axis(side=1, at=x,   labels=format(a, '%Y-%m-%d'))
grid(14,15,lty=2)
legend ("topleft", legend =c("Number of Errors", "Failure"), 
        cex=0.8, col =c("black","red"),lwd=c(1,0), pch=c(0,18))
dev.off()

## Calculate failure points





