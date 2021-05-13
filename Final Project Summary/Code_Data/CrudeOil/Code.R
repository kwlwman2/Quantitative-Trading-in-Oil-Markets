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

setwd("G:/SAVE_D/Dokumente/Energy_Futures/Crude_Oil")


data<-read.table("Crude_Oil.txt",header=TRUE);
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

#Prepare summary statistics with the Performance Analystics package
date<-as.Date(data$Date,format="%d.%m.%Y")
summaryStats<-data.frame(RCLC1=table.Stats(data[date>=as.Date("1991-01-01"),2]));
summaryStats[,2]<-table.Stats(data[date>=as.Date("1991-01-01"),3])
names(summaryStats)[2]<-"RCLC2";

summaryStats[,3]<-table.Stats(data[date>=as.Date("1991-01-01"),4])
names(summaryStats)[3]<-"RCLC3";

summaryStats[,4]<-table.Stats(data[date>=as.Date("1991-01-01"),5])
names(summaryStats)[4]<-"RCLC4";

summaryStats[,5]<-table.Stats(data[date>=as.Date("1991-01-01"),6])
names(summaryStats)[5]<-"RCLC5";

#Brent Crude Summary Stats

summaryStats2<-data.frame(LLCC1=table.Stats(data[date>=as.Date("1991-01-01"),9]));

summaryStats2[,2]<-table.Stats(data[date>=as.Date("1991-01-01"),10])
names(summaryStats2)[2]<-"LLCC2";

summaryStats2[,3]<-table.Stats(data[date>=as.Date("1991-01-01"),11])
names(summaryStats2)[3]<-"LLCC3";

summaryStats2[,4]<-table.Stats(data[date>=as.Date("1991-01-01"),12])
names(summaryStats2)[4]<-"LLCC4";

summaryStats2[,5]<-table.Stats(data[date>=as.Date("1991-01-01"),13])
names(summaryStats2)[5]<-"LLCC5";

sink("SummaryStats_Oil.txt");
print(xtable(summaryStats,digits=c(0,5,5,5,5,5)));
print(xtable(summaryStats2,digits=c(0,5,5,5,5,5)));
sink();
