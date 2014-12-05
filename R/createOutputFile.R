dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
setwd(dir)


file.out <- file('/file/out.txt', 'wt')


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

#ts1$hour = as.numeric(as.POSIXct(ts1$hour, origin="1970-01-01"))
len = length(ts1$day)
for(i in 1:len)
{
  
}
cat(ts1, sep = ';',  file = file.out, fill = TRUE)
close(file.out)
