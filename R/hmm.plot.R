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

data <- data.frame(date,error)
## Summing or grouping data of similar date
data = aggregate(y ~ x, data = data, sum)

head(data) ## date and error
fmx <- fit(depmix(y ~ 1, family = poisson(), nstates = 2, data = data), verbose = FALSE)
summary(fmx)
print(fmx)
probs <- posterior(fmx)             # Compute probability of being in each state
head(probs)
# Lets change the name
colnames(probs)[2:3] <- paste("P",1:2, sep="-")

# Create dta.frame
dfu <- cbind(data[,c(1,2)], probs[,2:3])
# to Long format
dfu <- melt(dfu,id="x", )

head(dfu)
# Get the states values
stts<-round(getpars(fmx)[seq(7, by=1, length=2)],1)
head(dfu)
# Change the names
names(stts) <- paste("St", (1:2), sep="-")


# Plot the data along with the time series of probabilities
qplot(dfu$x,dfu$value,data=dfu,geom="line",
      main = paste("States", paste(names(stts), stts, collapse=": "), collapse="; "),
      ylab = "State Probabilities") + 
  facet_grid(variable ~ ., scales="free_y") + theme_bw() 

# Plot observation of error
pdf("graph/obs_error.pdf",bg="white")
## Summing or grouping data of similar date
data = aggregate(y ~ x, data = data, sum)
x = data$x
y = data$y
g_range <- range(0, 100)
x_range <- range(0, 470)
# Graph cars using a y axis that ranges from 0 to 12
plot(NULL, ylim=g_range, xlim=x_range, xlab="Date", ylab="# of errors", xaxt="n")
lines(x, y, lwd=1, col="black")
a = as.POSIXct(ts1$hour, origin="1970-01-01")
axis(side=1, at=ts1$hour,   labels=format(a, '%Y-%m-%d'))
grid(14,15,lty=2)
#title(main="Observations", col.main="black", font.main=4)
dev.off()
#######################################################


