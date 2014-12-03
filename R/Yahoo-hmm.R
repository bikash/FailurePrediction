

for ( i in 2:6){
  set.seed(1)
  fmx <- fit(depmix(uempmed ~ 1, family = gaussian(), nstates = i, data = economics), verbose = FALSE)
  summary(fmx)
  print(fmx)
  
}

# Take i=6 an example.. The last one
probs <- posterior(fmx)             # Compute probability of being in each state
head(probs)

# Lets change the name
colnames(probs)[2:7] <- paste("P",1:6, sep="-")
# Create dta.frame
dfu <- cbind(economics[,c(1,5)], probs[,2:7])
# to Long format
dfu <- melt(dfu,id="date", )

head(dfu)

# Get the states values
stts<-round(getpars(fmx)[seq(43, by=2, length=6)],1)
# Change the names
names(stts) <- paste("St", (1:6), sep="-")
### St-1 St-2 St-3 St-4 St-5 St-6 
### 4.7  5.6  7.0  9.6  8.3  6.2 
# Plot the data along with the time series of probabilities
qplot(date,value,data=dfu,geom="line",
      main = paste("States", paste(names(stts), stts, collapse=": "), collapse="; "),
      ylab = "State Probabilities") + 
  facet_grid(variable ~ ., scales="free_y") + theme_bw()