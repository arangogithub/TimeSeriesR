# install.packages("vars")
# install.packages("dplyr")
library("dplyr")
library("vars")

# Setting environments
setwd("/home/jaime/prog/R")
getwd()

# Load, prepare and obtain an overview of data
info <- read.csv("data.csv", header=TRUE, sep=",", dec=",")
dim(info)
len <-  dim(info)[1]
str(info)

# Converting to time series
dat <- ts(as.Date(info$date, format = "%d/%m/%Y"))
dna <- ts(info$dane)
inx <- ts(info$index) 


# Now the hack....
correct_inx <- function(t){
  l_dna <- lag(dna, k=1)[2:len ]
  c_inx <- inx[2:len]
  c_inx[len] <- t
  l_dna[len] <- dna[len]
  dall = cbind(l_dna, c_inx)
  VDA = VAR(dall, lag.max=17, type = "both")
  predict(VDA)$fcst$l_dna[1]
}

# Scenario forecasting
values <- seq(from =-5.0, to = 5.0, by = 0.1) 
prd_scn <- sapply(values, correct_inx)
plot(prd_scn ~ values, pch = 16)

 
