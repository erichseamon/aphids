library(ape)
library(spatstat)
library(spatial)


#--EDA on aphid data
#--load the data
dat <- read.csv("/nethome/erichs/2007PanTraps.csv")
dat = dat[-1,]
dat$Site <- NULL
#--transpose just in case
dattrans <- data.frame(t(dat))

#--using spatstat - convert dat to a ppp format
W <- ripras(dat)
dat1 <- as.ppp(dat,W=W)

#plot the ppp formated data
plot(dat1, border="white") #plot Ripleys K function
n5 <- nnwhich(dat1,k=1:5) #nearest neighbor


#Ripleys K function  In exploratory analyses, the estimate of K 
#is a useful statistic summarising aspects of inter-point “dependence” 
#and “clustering”. For inferential purposes, the estimate of K 
#is usually compared to the true value of K for a completely 
#random (Poisson) point process, which is K(r) = pi * r^2. 
#Deviations between the empirical and theoretical K curves 
#may suggest spatial clustering or spatial regularity.

kc <- Kest(dat1)
plot(kc)

plot(kc, cbind(r, sqrt(iso/pi)) ~ r)

plot(kc, cbind(trans,iso,border) - theo ~ r)

#--

Fc <- Fest(dat1)
plot(Fc)
plot(Fc, cbind(km,trans,border) ~ theo)

x <- markcorr(dat1)



----
  

KS <- cdf.test(dat1, function(x, y) {
   x
   })

plot(KS)

--
  
  
  fitx <- ppm(dat1, ~x)



datsmooth <- density.ppp(dat1)
plot(datsmooth)

#-distance map
datmap <- distmap(dat)
plot(datmap, main = "Empty space distances")
plot(cells, add = TRUE)


#--Multiple histograms
barplot(as.matrix(dat[4:19]), main="All data grouped by time, 2007", ylab="Total", beside=TRUE, 
        col=terrain.colors(20))



#--inverse distance matrix creation
ozone.dists <- as.matrix(dist(cbind(dat$LAT, dat$LONG)))
ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0

ozone.dists.inv

collist <- c(X5_14_07, X5_17_07)
moranmatrix <- matrix(NA, nrow = 1, ncol = 17)

n = 0
for (i in collist){
bb <- paste("dat", "$X5_14_07", sep="")
n = n + 1
x <- Moran.I(c(dat[i]), ozone.dists.inv)$p.value
moranmatrix
}
