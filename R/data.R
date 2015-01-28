
library(lattice)
library(plyr)
library(depmixS4)
library(TTR) # For downloading SP500 index
library(ggplot2)
library(reshape2)
library(xts)
library(data.table)
library(reshape)
dir = "/Users/bikash/repos/FailurePrediction/R" # path for macbots1ok
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
getrawData = function()
{
    ## Fill empty slot with unfailure data
    #len = length(df$date)
    #tseq <- seq(as.POSIXlt(df$date[1], origin="1970-01-01"), as.POSIXlt(df$date[len], origin="1970-01-01"), by="secs")
    tseq <- seq(as.POSIXlt(1415724101, origin="1970-01-01"), as.POSIXlt(1417413321, origin="1970-01-01"), by="days")
    tseq=as.numeric(as.POSIXct(tseq))
    error = 7
    data.nonerror <- data.frame(tseq,error)
    ## Merge all data 
    date = unlist(list(Data1$date, Data2$date, Data3$date, Data4$date, Data5$date,Data6$date, Data7$date, tseq))
    error = unlist(list(Data1$ErrorType,Data2$ErrorType,Data3$ErrorType,Data4$ErrorType,Data5$ErrorType, Data6$ErrorType, Data7$ErrorType, data.nonerror$error))
    
    #data.errorType <- data.frame(date[1:10000],error[1:10000])
    data.errorType <- data.frame(date,error)
    ## Summing or grouping data of similar date
    #rawData <- by(error,date,function(x)paste(x,collapse=","))
    cols <- c("date","error")
    colnames( data.errorType) <- cols
    dt <- data.table(data.errorType )
    #if(pivot)
      rawData = dcast(dt, date ~ dt$error, fun.aggregate = sum, value.var = "error") 
    #else
      #rawData = data.errorType
    
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
              dimnames=list(ErrorType$day, c( "Error1","Error2", "Error3", "Error4", "Error5", "Error6")))
  
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
         legend=c( "Network connection","Memory overflow", "Security setting", "Unknown", "Java I/O error", "Namenode failure"),             # Text for the legend
         density=c(90, 70, 50, 40, 30, 20),
         #fill=c( "white","black",  "gray", "red","green","blue")
         )  
  
  dev.off()
}


getDataHr <-function()
{
  ##Plot number of observation Error Vs time
  Data1 = read.table("file/error_25.txt", 
                     sep=";", 
                     col.names=c("date",  "status", "ErrorType", "Node"), 
                     fill=FALSE, 
                     strip.white=TRUE)
  Data1$hour <- cut(as.POSIXlt( Data1$date,  origin="1970-01-01" ), breaks = "hour")
  getcount <- function(Df) { c(count = as.numeric(length(Df$ErrorType)),
                               obs = paste(Df$ErrorType, collapse=","))  
  }
  ts1 <- ddply(Data1, .(hour),getcount)

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
  
  
  tseq <- seq(as.POSIXlt(1415724101, origin="1970-01-01"), as.POSIXlt(1417413321, origin="1970-01-01"), by="hours")
  tseq=as.numeric(as.POSIXct(tseq))
  error = 7
  data.nonerror <- data.frame(tseq,error)
  ## Merge all data 
  date = unlist(list(Data1$date, Data6$date, Data7$date, tseq))
  error = unlist(list(Data1$ErrorType, Data6$ErrorType, Data7$ErrorType, data.nonerror$error))
  data.errorType <- data.frame(date,error)
  ## Summing or grouping data of similar date
  #cols <- c("date","error")
  #colnames( data.errorType) <- cols
  dt <- data.table(data.errorType )
  rawData = dcast(dt, date ~ dt$error, fun.aggregate = sum, value.var = "error") 
  
  return (rawData)
  
}

PredictionGraph <- function()
{
## Print healthy and failure state
df = getDataHr()
## Change column name
cols <- c("date","y1","y2","y3","y4","y5","y6","y7") ## error 1 to 6 and 7 is non error event
colnames(df) <- cols
## Plot Error observation
df$state[df$y1 >= 1] <- "Failure"
df$state[df$y1 < 1] <-  "Healthy"

## observation
df$statev[df$y1 >= 1] <- 1
df$statev[df$y1 < 1] <-  0


x <- seq_along(df$date)
y <- df$statev
g_range <- range(0, 1)
x_range <- range(0, 840)
plot(NULL, ylim=g_range, xlim=x_range, xlab="Date", ylab="State", xaxt="n")
points(x, df$state, lwd=1, col="red")

## plot graph for failure and healthy
ggplot(aes(x = seq_along(df$date)), data = df) +
  geom_point(aes(y = df$state)) + 
  ylab("State") + xlab("Time (In hr)") + ylab("State") +
  ggtitle("Actual Results")
### Plot predictions with true sequence
p1 <- ggplot(aes(x = seq_along(df$date)), data = df) +
  geom_point(aes(y = df$state)) + 
  ylab("State") + xlab("Time (In hour)") + ylab("States")

p2 <- ggplot(aes(x = seq_along(df$date)), data = df) +
  geom_point(aes(y = df$state), color = "#F8766D") + 
  geom_point(aes(y = df$viterbi), color = "#00BFC4") +
  xlab("Dice Roll (In Sequence)") + ylab("State") +
  ggtitle("Viterbi Predictions")

p3 <- ggplot(aes(x = seq_along(df$date)), data = df) +
  geom_point(aes(y = df$state), color = "#F8766D") + 
  geom_point(aes(y = df$posterior), color = "#00BFC4") +
  xlab("Dice Roll (in sequence)") + ylab("State") +
  ggtitle("Posterior Predictions")

load.packages('RHmm')
df$obs[df$y7 >= 1] <- 7
df$obs[df$y7 < 1] <-  0
df$obs[df$y1 >= 1] <- 1
df$obs[df$y2 >= 1] <- 2
df$obs[df$y3 >= 1] <- 3
df$obs[df$y4 >= 1] <- 4
df$obs[df$y5 >= 1] <- 5
df$obs[df$y6 >= 1] <- 6

library(HMM)
#Define HMM Model
TPM <- matrix(c(.95, .05, 
                .1, .9), 2, byrow = TRUE)
EPM <- matrix(c(0.000, 0.02, 0.02, 0.02, 0.02, 0.02, 0.99,
                0.98, 0.01, 0.01, 0.01, 0.00, 0.001, 0.001), 2, byrow = TRUE)


### Modeling
# Create hmm using our TPM/EPM
hmm <- initHMM(c("Healthy", "Failure"), c(1, 2, 3, 4, 5, 6, 7),
               transProbs = TPM, emissionProbs = EPM)
# Pull in results from the simulation
obs <- df$obs
# Save Viterbi/Posterior predictions as a new column
df$viterbi <- viterbi(hmm, obs)
#hmmFit = baumWelch(hmm, obs) 

## training sample
df$state[1:150] = sample(c("Failure","Healthy"),150,T)


## Calcualte no of observation
for(i in 1:length(df$state))
{
  df$seq[i] = df$y1[i]+df$y2[i]+df$y3[i]+df$y4[i]+df$y5[i]+df$y6[i]
}
## observation for error sequen
for(i in 1:length(df$state))
{
  if(df$y1[i]>0)
    df$errseq[i] = 1
  else if(df$y2[i]>0)
    df$errseq[i] = 2
  else if(df$y3[i]>0)
    df$errseq[i] = 3
  else if(df$y4[i]>0)
    df$errseq[i] = 4
  else if(df$y5[i]>0)
    df$errseq[i] = 5
  else if(df$y6[i]>0)
    df$errseq[i] = 6
  else
    df$errseq[i] = 7
}




## plot of graph
#pdf("graph/failurePredictionN.pdf",bg="white")
readline("Plot simulated failure:\n")
x <- seq_along(df$date)
y <- df$statev
y_range <- range(0, 40)
x_range <- range(0, 840)
nSim = length(df$state)
xlb = "Time (in hours)"
ylb = "# of error sequence"
plot(NULL, ylim=c(-15.5,20), xlim=x_range, xlab=xlb, ylab=ylb, xaxt="n" ,pch=3, bty="n", axes=FALSE)
lines(x, df$seq, lwd=1, col="black")
#axis(2,at=1:30)

readline("Actual Failure State:\n")
text(410,-1.4,cex=.8,col="black", pos=4, labels = "Actual Failure State")
j =1
failure = rep(c(0:10))
for(i in 1:nSim)
{
  if(df$state[i] == "Failure")
  {
    rect(i,-5,i+1,-2, col = "black", border = NA)
    failure[j]=i
    j = j+1
  }
  else
    rect(i,-5,i+1,-2, col = "grey", border = NA) 
}

readline("Predicted Failure State (viterbi):\n")
text(410,-6.4,cex=.8,col="black", pos=4, labels = "Predicted Failure State")
for(i in 1:nSim)
{ 
  if(df$viterbi[i] == "Failure")
    rect(i,-10,i+1,-7, col = "red", border = NA)
  else
    rect(i,-10,i+1,-7, col = "grey", border = NA)  
}

readline("Error in Prediction:\n")
text(410,-11.4,cex=.8, col="black",pos=4, labels ="Error in Prediction")
differing = !(df$state == df$viterbi)
for(i in 1:nSim)
{
  if(differing[i])
    rect(i,-15,i+1,-12, col = "blue", border = NA)
  else
    rect(i,-15,i+1,-12, col = rgb(.9, .9, .9), border = NA)  
}
axis(1, col = "black", col.axis = "black", lwd = 2)
axis(2, 0:20, col = "black", col.axis = "black", lwd = 2)

#dev.off()
}

PredictionGraph()

pdf("graph/failurehist.pdf",bg="white")
##Plot histogram for time between failures (TBF)
len = length(failure)
j = 1
failure1 = rep(c(0:10))
for(i in 2:len)
{
  failure1[j] = failure[i] - failure[i-1]
  j = j+1
}
failure1[len]= 10
hist(failure1, freq=FALSE, main="", xlab = "Time Between Failure (hour)", density = 10, angle = 45)

dev.off()

### Autocorrelation of Failure occurence
pdf("graph/acf_failure.pdf",bg="white")
acf(df$statev, xlab="Lag (hours)", main="")
dev.off()