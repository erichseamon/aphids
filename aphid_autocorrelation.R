library(ape)
library(spatstat)
library(spatial)


#N1 <- readline("enter last two digits of Pantrap year to analyze: ")
N1 <- "07"
#--EDA on aphid data
#--load the data

dat <- read.csv(paste("/home/git/data/aphids/", "pantrap", N1, "_refined.csv", sep=""))
dat = dat[-1,]
dat$Site <- NULL
dat$site <- NULL
#--transpose just in case
dattrans <- data.frame(t(dat))

dat <- dat[1:18]
#--using spatstat - convert dat to a ppp format
W <- ripras(dat)
dat1 <- as.ppp(dat,W=W)

#plot the ppp formated data
plot(dat1, border = "white", main="Ripley's K function for each time period, 2007") #plot Ripleys K function

#--nearest neighbor function
n5 <- nnwhich(dat1,k=1:5) #nearest neighbor


#Ripleys K function  In exploratory analyses, the estimate of K 
#is a useful statistic summarising aspects of inter-point “dependence” 
#and “clustering”. For inferential purposes, the estimate of K 
#is usually compared to the true value of K for a completely 
#random (Poisson) point process, which is K(r) = pi * r^2. 
#Deviations between the empirical and theoretical K curves 
#may suggest spatial clustering or spatial regularity.

kc <- Kest(dat1)
plot(kc, main="Ripley's K(r)r vs r - aphids 2007")

plot(kc, cbind(r, sqrt(iso/pi)) ~ r, main="estimates of L function, aphids 2007")

plot(kc, cbind(trans,iso,border) - theo ~ r)

#--neighborhood density function
localL <- localL(dat1)
plot(localL, main="Local L function - neighborhood density function", legend=FALSE)

localK <- localK(dat1)
plot(localK, main="Local K function - neighborhood density function", legend=FALSE)

#--

#The empty space function (also called the ``spherical contact distribution'' 
#or the ``point-to-nearest-event'' distribution) of a stationary point process X 
#is the cumulative distribution function F of the distance from a fixed point in 
#space to the nearest point of X.

#An estimate of F derived from a spatial point pattern dataset can be used in 
#exploratory data analysis and formal inference about the 
#pattern (Cressie, 1991; Diggle, 1983; Ripley, 1988). In exploratory analyses, 
#the estimate of F is a useful statistic summarising the sizes of gaps in the 
#pattern. For inferential purposes, the estimate of F is usually compared to 
#the true value of F for a completely random (Poisson) point process, which is
#F(r) = 1 - exp( - lambda * pi * r^2)

#where lambda is the intensity (expected number of points per unit area). 
#Deviations between the empirical and theoretical F curves may suggest spatial 
#clustering or spatial regularity.

Fc <- Fest(dat1)
plot(Fc, main="Ripley's F(r) vs. r for aphids 2007")


#--P-P style plot- comparing estimate of F to true value of F
plot(Fc, cbind(km, theo) ~ theo, main="Ripley's F comparing estimate to true F - aphids 2007")

#dat1<- as.factor(dat1)
x <- markcorr(dat1)


----
  

KS <- cdf.test(dat1, function(x, y) {
   x
   })

plot(KS)

--
  
  
  fitx <- ppm(dat1, ~x)

#--used for kernel density smoothing

datsmooth <- density.ppp(dat1)
plot(datsmooth, main="kernel density smoothing for aphids 2007")


#-reduced second moment measure - takes the raw Bartlett periodogram,
#multiplies by the Fourier transform of the bivariate normal density, 
#then takes the inverse FFT to yield the smoothed density

secondmoment <- Kmeasure(dat1, .05)
plot(secondmoment, main="Estimated Reduced second moment measure for aphids 2007, sigma=.05")


#-distance map
datmap <- distmap(dat1)
plot(datmap, main = "Empty space distances, aphids 2007")
plot(cells, add = TRUE)


#--Multiple histograms
barplot(as.matrix(dat[4:19]), main="All data grouped by time, 2007", ylab="Total", beside=TRUE, 
        col=terrain.colors(20))



#--inverse distance matrix creation
ozone.dists <- as.matrix(dist(cbind(dat$LAT, dat$LONG)))
ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0

ozone.dists.inv

endcolnumber <- ncol(dat)
endcolnumber_lessone <- endcolnumber -1
endcolnumber_lessthree <- endcolnumber -3
collist <- c(colnames(dat[3:endcolnumber_lessone]))
moranmatrix <- matrix(NA, nrow = 1, ncol = endcolnumber_lessthree)
colnames(moranmatrix) <- collist

dat_nofid <- dat[3:endcolnumber_lessone]

n = 0
for (i in collist){
n = n + 1
y <- assign(paste("dat$", i, sep=""), dat[,n])
x <- Moran.I(y, ozone.dists.inv)$p.value
moranmatrix[,n] <- x
colnames(moranmatrix[,n])
}

barplot(moranmatrix, main="Moran P Value for Aphids 2007", ylab="Moran I p value", col=c("darkblue"), names.arg = colnames(moranmatrix), beside=TRUE)
