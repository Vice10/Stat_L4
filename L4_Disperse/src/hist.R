histFunc <- function(){
  x <- oneway$residuals
  h<-hist(x, breaks=15, col="red", xlab="Resid",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
}

histFunc()



hist(oneway$residuals, freq = FALSE)
x <- seq(min(oneway$residuals), max(oneway$residuals), length.out=10)
y <- with(oneway, dnorm(x, mean(residuals), sd(residuals)))
lines(x, y, col = "red")
