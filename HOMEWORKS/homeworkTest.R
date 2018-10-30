###

PL <- read.csv("/Users/emilypiche/Desktop/plotlevel.csv", as.is=TRUE)

SOM <- PL$LOI
summary(SOM)


Conifers <- rnorm(23, .33, .33)
e <- rnorm(23, .4, .2)
SOM <- 0 + .3*Conifers + e
plot(Conifers,SOM, xlab="Proportion of Conifers in Overstory", ylab="% SOM")








gammaPars <- fitdistr(SOM,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

myGamma <- rgamma(n=length(SOM), shape=shapeML, rate=rateML)