library(ggplot2)

ggplot(servo, aes(x = error, y = motor)) + theme_bw() + geom_bar(stat = "identity")

library(lattice)
p <- xyplot(motor~error, group = motor, data = servo)
meanColours = c("blue", "purple", "green", "red", "orange")
xyplot(motor~error, group = motor, data = servo, cex = 0.9,
       panel=function(x,y,...) { 
         panel.xyplot(x,y,...) 
         panel.abline(v=agr$motorMean, col=meanColours, lwd="2") 
       })

library(plyr)
agr <- ddply(servo, .(motor), summarize, motorMean=mean(error))

comb <- merge(servo, agr)

plot(servo$error, servo$motor)
