## This file contains 4 functions that model a set of data in a 
## progressively more complicated fashion.  There is a linear fit,
## parabolic (quadratic) fit, sinuoidal fit, and harmonic fit.  
## The code also plots the residuals and returns statistical information
## about the fit

## The file can be tested on the attached Mauna Loa CO2 data file or 
## downloaded from:  http://esrl.noaa.gov/gmd/ccgg/trends/

## This code modified from Grant Foster's book:  "Analyzing Light Curves"


## FUNCTION 1:  Import data and generate a plot and fit a linear trend

linear <- function(data_file){
  
  data <- read.table(data_file)
  t <- data[,3]
  x <- data[,5] 
  
  plot(t, x, ylim = c(300,400), pch=".", cex=3, 
              xlab="Time (years)", 
              ylab="CO2 concentration (ppmv)", 
              main="Mauna Loa", 
              cex.lab=1.5,cex.axis=1.5, cex.main=2)
  
  xfit <- lm(x ~ t)
  lines(t,xfit$fitted.values,col="red", lwd=3)
  
  plot(t, xfit$residuals,pch=".", cex=3,
       xlab="Time (years)",
       ylab="Residuals (ppmv)",
       main="Mauna Loa",
       cex.axis=1.5,cex.lab=1.5,cex.main=2)
  
  summary(xfit)
}


# FUNCTION 2:  fit a parabola (quadratic curve)
parabola <- function(data_file){
  
  data <- read.table(data_file)
  t <- data[,3]
  x <- data[,5] 
  
  plot(t, x, ylim = c(300,400), pch=".", cex=3, 
       xlab="Time (years)", 
       ylab="CO2 concentration (ppmv)", 
       main="Mauna Loa", 
       cex.lab=1.5,cex.axis=1.5, cex.main=2)
  
  
  tsquared <- t^2
  xfit <- lm(x ~ t+tsquared)
  lines(t,xfit$fitted.values,col="red", lwd=3)
  
  plot(t, xfit$residuals,pch=".", cex=3,
       xlab="Time (years)",
       ylab="Residuals (ppmv)",
       main="Mauna Loa",
       cex.axis=1.5,cex.lab=1.5,cex.main=2)
  
  summary(xfit)
}

# FUNCTION 3:  fit a parabolic + sinusoid curve
sinusoid <- function(data_file){
  
  data <- read.table(data_file)
  t <- data[,3]
  x <- data[,5] 
  
  plot(t, x, ylim = c(300,400), pch=".", cex=3, 
       xlab="Time (years)", 
       ylab="CO2 concentration (ppmv)", 
       main="Mauna Loa", 
       cex.lab=1.5,cex.axis=1.5, cex.main=2)
      
  tsquared <- t^2
  c1 <- cos(2*pi*t)
  s1 <- sin(2*pi*t)
  xfit <- lm(x ~ t+tsquared+c1+s1)
  lines(t,xfit$fitted.values,col="red", lwd=2)
  
  plot(t, xfit$residuals,pch=".", cex=3,
       xlab="Time (years)",
       ylab="Residuals (ppmv)",
       main="Mauna Loa",
       cex.axis=1.5,cex.lab=1.5,cex.main=2)
  
  summary(xfit)
}

# FUNCTION 4:  fit a parabolic + sinsuoidal + 2 harmonic curve
harmonic <- function(data_file){
  
  data <- read.table(data_file)
  t <- data[,3]
  x <- data[,5] 
  
  plot(t, x, ylim = c(300,400), pch=".", cex=3, 
       xlab="Time (years)", 
       ylab="CO2 concentration (ppmv)", 
       main="Mauna Loa", 
       cex.lab=1.5,cex.axis=1.5, cex.main=2)
  
  
  tsquared <- t^2
  c1 <- cos(2*pi*t)
  s1 <- sin(2*pi*t)
  c2 <- cos(4*pi*t)
  s2 <- sin(4*pi*t)
  c3 <- cos(6*pi*t)
  s3 <- sin(6*pi*t)
  
  xfit <- lm(x ~ t+tsquared+c1+s1+c2+s2+c3+s3)
  lines(t,xfit$fitted.values,col="red", lwd=2)
  
  plot(t, xfit$residuals,type="o", pch=".", cex=3,
       xlab="Time (years)",
       ylab="Residuals (ppmv)",
       main="Mauna Loa",
       cex.axis=1.5,cex.lab=1.5,cex.main=2)
  
  summary(xfit)
}