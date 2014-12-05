dir = "/home/bikash/repos/FailurePrediction/R" # path in linux machine
setwd(dir)

source = "file/error_25.txt"
file.out <- file('/file/out.txt', 'wt')



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
ts1$hour = as.numeric(as.POSIXct(ts1$hour, origin="1970-01-01"))
k =  1415721600 
for(i in 1:470)
{
   if(ts1$hour[i]!= k){
    ts$obs[i]="0,0,0,0,0"
    ts$hour[i]=k
   }
   else
   {
     ts$obs[i]=ts1$obs[i]
     ts$hour[i]=ts1$hour[i]
   }
   k= k+3600
}

file.in <- file(source, 'rt')
line <- readLines(file.in, n=1)
while (length(line)) {
  line <- gsub("  ", " ", line)
  line.split <- strsplit(line, " ")
  ts <- strsplit(line.split[[1]][2], ",")
  status = line.split[[1]][3]
  if (length(line.split[[1]]) > 2) {
    if(status == "ERROR")
    {
      da = paste(line.split[[1]][1], ts[[1]][1])
      date = as.POSIXct(da, origin="1970-01-01")
      #text = paste(line.split[[1]][4], line.split[[1]][5])
      text = paste(line.split[[1]][4], line.split[[1]][5], "Haisen25\n")
      cat(date, status, text, sep = ';',  file = file.out1, fill = TRUE)
    }
  }
  line<- readLines(file.in, n=1)
}
close(file.in)