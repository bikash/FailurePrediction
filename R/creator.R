
## This file is used to create dumpy input <timestamp><errortype> almost of 20 GB


file.out <- file('/home/bikash/repos/FailurePrediction/R/file/out.txt', 'wt')


  ## Merge all data 

  ##Create random error sequence sample(1:7,100, replace=T)
  tseq <- seq(as.POSIXlt(948642137, origin="1970-01-01"), as.POSIXlt(1415724100, origin="1970-01-01"), by="mins")
  errType = sample(1:7,7784700, replace=T)
  data.nonerror <- data.frame(tseq,errType)
  date = unlist(list(Data1$date, Data2$date, Data3$date, Data4$date, Data5$date,Data6$date, Data7$date, tseq))
  error = unlist(list(Data1$ErrorType,Data2$ErrorType,Data3$ErrorType,Data4$ErrorType,Data5$ErrorType, Data6$ErrorType, Data7$ErrorType, data.nonerror$errType))
  data.errorType <- data.frame(date,error)
  dt <- data.table(data.errorType )
  dt$hour <- cut(as.POSIXlt( dt$date,  origin="1970-01-01" ), breaks = "hour")
  getcount <- function(Df) { c(obs = paste(Df$error, collapse="," ))  
  }
  ts1 <- ddply(dt, .(hour),getcount)
  ts1$date <-  as.numeric(as.POSIXlt(ts1$hour, origin="1970-01-01"))

  len <- length(ts1$date)
  for(i in 1:len) {
     text = paste(c(ts1$date[i], ts1$obs[i]), collapse=",")
     cat(text, sep = '',  file = file.out, fill = TRUE)
     i = i+1
  }

close(file.out)