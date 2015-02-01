#scalability test

## Plot graph for Error vs time
pdf("graph/scalable.pdf",bg="white")

y = (231, 246, 253, 259, 263, 271)
x1 = c(1, 2, 3, 4, 5, 6)
g_range <- range(0, 300)
x_range <- range(0, x1)
# Graph cars using a y axis that ranges from 0 to 12
plot(NULL, ylim=g_range, xlim=x_range, xlab="Data Size", ylab="Execution Time (sec)", xaxt="n")
lines(ts1$day, y1, lwd=1, col="red")

axis(side=1, at=x1,   labels=c("1GB","2GB","3GB","4GB","5GB","6GB"))
grid(14,15,lty=2)

# Create a title with a red, bold/italic font
#title(main="Observations", col.main="black", font.main=4)

dev.off()
