#
# Technical Trading in the Oil Market
#
# (c) Thorben Lubnau
#     2013
#


# Load additional libraries
library(pracma)
library(tseries)
library(PerformanceAnalytics)
library(TTR)
library(urca)
library(xtable)
library(dlm)
library(lubridate)

#setwd("D:/Dokumente/Energy_Futures/Heating_Oil")


data<-read.table("Heating_Oil.txt",header=TRUE);
date<-as.Date(data$Date,format="%d.%m.%Y")


#Function to calculate cont. returns

calcReturn<-function(x,first="Zero"){
	
	ret<-numeric(length(x));

	#logX <- log(x);
	#for(i in seq(2,length(x),1)){
	#	ret[i]<-logX[i]-logX[i-1];
	#}

	ret[2:length(ret)]<- diff(x)/x[1:length(x)-1];

	if(first=="Zero"){
		ret[1]<-0;}
	if(first=="Mean"){
		ret[1]<-mean(ret[2:length(ret)]);}

	return(ret);
}


#Lag operator with default lag 1
lagB<-function(x,k=1)
{
	n <- length(x);
	result <- numeric(n);

	for(i in 1:k){
		result[i]<-NA;
	}

	for(i in seq((k+1),n,1)){
		result[i] <- x[i-k];
	}

	return(result);	

}	


#Function to calculate the rolling standard deviation of a time series

rolling_std<-function(x,lookback=20)
{
	n <- length(x);
	result<-numeric(n);
	for(i in 1:(lookback-1)){
		result[i]<-NA;
	}

	for(i in seq(lookback,n,1)){
		result[i]<-sd(x[(i-lookback+1):i])*sqrt((lookback-1)/lookback);
	}

	return(result);
}



fillMissingData<-function(x)
{
	n<-length(x);
	for(i in 2:n){
		if(is.na(x[i])) {x[i]<-x[i-1];}
	}

	return(x);
}








