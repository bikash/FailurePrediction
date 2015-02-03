#scalability test

## Plot graph for Error vs time
pdf("graph/scalable.pdf",bg="white")

y = c(571, 591, 619, 647)
x1 = c(2, 4, 6, 8)
x2 = c(71, 122, 168, 278, 331, 385)
g_range <- range(0, 700)
x_range <- range(2, x1)
# Graph cars using a y axis that ranges from 0 to 12
plot(NULL, ylim=g_range, xlim=x_range, xlab="Data Size (GB)", ylab="Execution Time (sec)", xaxt="n")
lines(x1, y, lwd=1, col="red")
points(x1, y, lwd=1, col="red",pch = 20)
axis(side=1, at=x1, line=0,  labels=c("2","4","6","8"))
#axis(1,at=x1,line=0,labels=x1)
#mtext("Data Size (GB)",1,line=1,at=0.2)

#axis(1,at =x1,line=3,col="blue", labels=x2)
#mtext("Data Size (million)",1,line=3,col="blue",at=0.2)


grid(14,15,lty=2)

# Create a title with a red, bold/italic font
#title(main="Observations", col.main="black", font.main=4)

dev.off()
