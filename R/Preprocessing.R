source = "/home/bikash/Dropbox/data/hadoop-cmf-hdfs1-DATANODE-haisen25.ux.uis.no.log.out.1"
source1 = "/home/bikash/Dropbox/data/hadoop-cmf-hdfs1-DATANODE-haisen25.ux.uis.no.log.out"
file.out1 <- file('/home/bikash/Dropbox/data/file/error_25.txt', 'wt')
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

file.in <- file(source1, 'rt')
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
      text = paste(line.split[[1]][4],line.split[[1]][5],  "Haisen25\n")
      cat(date, status, text, sep = ';',  file = file.out1, fill = TRUE)
    }
  }
  line<- readLines(file.in, n=1)
}

close(file.in)
close(file.out1)