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
dir = "/Users/bikash/repos/FailurePrediction/R" # path for macbook
#dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
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

## d data from haisen22
Data6 = read.table("file/error_22.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data6$hour <- cut(as.POSIXlt( Data6$date,  origin="1970-01-01" ), breaks = "hour")
ts6 <- ddply(Data6, .(hour),getcount)


## d data from haisen20
Data7 = read.table("file/error_20.txt", 
                   sep=";", 
                   col.names=c("date",  "status", "ErrorType", "Node"), 
                   fill=FALSE, 
                   strip.white=TRUE)
Data7$hour <- cut(as.POSIXlt( Data7$date,  origin="1970-01-01" ), breaks = "hour")
ts7 <- ddply(Data7, .(hour),getcount)

## Merge all data 
#date = unlist(list(ts1$hour, ts2$hour, ts3$hour, ts4$hour, ts5$hour, ts6$hour, ts7$hour))
date = unlist(list(ts1$day, ts2$day, ts3$day, ts4$day, ts5$day, ts6$day, ts7$day))
#error = unlist(list(Datat1$ErrorType, Data2$ErrorType, Data6$ErrorType, Data7$ErrorType))
error = unlist(list(ts1$count, ts2$count, ts3$count, ts4$count, ts5$count, ts6$count, ts7$count))


date = unlist(list(ts1$day, ts2$day, ts3$day))
error = unlist(list(ts1$count, ts2$count, ts3$count))
data <- data.frame(date,error)

head(data) ## date and error
fmx <- fit(depmix(error ~ 1, family = poisson(), nstates = 2, data = data), verbose = FALSE)
summary(fmx)
print(fmx)
probs <- posterior(fmx)             # Compute probability of being in each state
head(probs)
# Lets change the name
colnames(probs)[2:3] <- paste("P",1:2, sep="-")

# Create dta.frame
dfu <- cbind(data[,c(1,2)], probs[,2:3])
# to Long format
dfu <- melt(dfu,id="date", )

head(dfu)
# Get the states values
stts<-round(getpars(fmx)[seq(7, by=1, length=2)],1)

# Change the names
names(stts) <- paste("St", (1:2), sep="-")

# Plot the data along with the time series of probabilities
qplot(date,value,data=dfu,geom="point",
      main = paste("States", paste(names(stts), stts, collapse=": "), collapse="; "),
      ylab = "State Probabilities") + 
  facet_grid(variable ~ ., scales="free_y") + theme_bw() 

head(dfu)


#p <- ggplot(dfu, aes(x = date, y = value, color = variable))
#p + geom_line() + ylab("y")
