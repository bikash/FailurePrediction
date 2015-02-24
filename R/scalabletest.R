#scalability test

## Plot graph for Error vs time
pdf("graph/scalable1.pdf",bg="white")

y1 = c(377, 386, 430, 451, 513 ,531, 547)
y2 = c(415, 539, 580, 649, 753, 823, 861 )
x1 = c(1:7)
#x2 = c(71, 122, 168, 278, 331, 385)
g_range <- range(300, 900)
x_range <- range(2, x1)
# Graph cars using a y axis that ranges from 0 to 12
plot(NULL, ylim=g_range, xlim=x_range, xlab="Data Size (GB)", ylab="Execution Time (sec)", xaxt="n")
lines(x1, y1, lwd=1, col="brown")
points(x1, y1, lwd=1, col="brown",pch = 20)

lines(x1, y2, lwd=1, col="black")
points(x1, y2, lwd=1, col="black",pch = 21)

axis(side=1, at=x1, line=0,  labels=x1)
#axis(1,at=x1,line=0,labels=x1)
#mtext("Data Size (GB)",1,line=1,at=0.2)
#axis(1,at =x1,line=3,col="blue", labels=x2)
#mtext("Data Size (million)",1,line=3,col="blue",at=0.2)
grid(14,15,lty=2)
## add extra space to right margin of plot within frame
legend ("topleft", legend =c("1 map task", "5 map task" ), 
        cex=0.8, col =c("black","brown"),lwd=c(1,1), lty=c(1,1))

dev.off()



##scalability with node size with 5 GB data
## Plot graph for Error vs time
pdf("graph/scalablenode.pdf",bg="white")
y1 = c(1183, 938, 804, 727, 695)
x1 = c(1:5)
#x2 = c(71, 122, 168, 278, 331, 385)
g_range <- range(600, 1200)
x_range <- range(1, x1)
# Graph cars using a y axis that ranges from 0 to 12
plot(NULL, ylim=g_range, xlim=x_range, xlab="Nodes", ylab="Execution Time (sec)", xaxt="n")
lines(x1, y1, lwd=1, col="black")
#points(x1, y1, lwd=1, col="black",pch = 20)

axis(side=1, at=x1, line=0,  labels=x1)
grid(14,15,lty=2)
dev.off()
