
#setwd("G:/Save_D/Dokumente/Energy_Futures/Natural_Gas")
#source("Code.R")
names(data);

assets<-c("NNGC1","NNGC5");

crude_prices<-data[,c("Date","NNGC1","NNGC5")];
crude_prices[,1]<-as.Date(crude_prices[,1],format="%d.%m.%Y")

#Specify the lookback period for rolling mean and rolling sd
lb<-200;

dates<-c("1991-01-01","1992-01-01","1993-01-01","1994-01-01","1995-01-01","1996-01-01",
"1997-01-01","1998-01-01","1999-01-01","2000-01-01","2001-01-01","2002-01-01","2003-01-01",
"2004-01-01","2005-01-01","2006-01-01","2007-01-01","2008-01-01",
"2009-01-01","2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01");

#Set the number of bootstrap samples
boot_no<-100;
testperiod<-5;

startingCash<-100;
fees<-0.001;

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Dynamic Hedge Ratio with Kalman Filter		+
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Start by calculating the hedge ratio with daily updates
offset<-101;
kalman_hedge_ratio<-data.frame(Date=crude_prices$Date,dynamic_hedge=numeric(length(crude_prices$Date)));

#Calculation starts with five years of data
for(i in (offset+1):length(crude_prices$Date)){
	dlmSample<-crude_prices[1:i,];
	dlmSample[,1]<-as.Date(dlmSample[,1]);

	buildHedge<-function(u){
		dlmModReg(dlmSample[,3],dV=0.0025,dW=exp(u[2:3]))
	}

	outMLE <- dlmMLE(dlmSample[,2],parm=rep(0,3),buildHedge,
		method = c("SANN"),control=
		list(trace=1, REPORT = 10, maxit = 100))
		
	reg<-dlmModReg(dlmSample[,3],dV=exp(outMLE$par[1]),
		dW=c(exp(outMLE$par[2]),exp(outMLE$par[3])),m0=c(0,1.0));
	outF<-dlmFilter(dlmSample[,2],reg);

	parameters<-outF$m;
	
	#Debugging
	print(i);
		
	forecasted_hedge_ratio<-parameters[length(parameters[,2]),2];
	kalman_hedge_ratio$dynamic_hedge[i]<-forecasted_hedge_ratio;
}

#Copy to keep the following code unchanged
dynamic_hedge<-kalman_hedge_ratio;


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Create plot of the dynamic hedge ratio					+
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pdf("DynamicHedgeRatio.pdf");
plot(dynamic_hedge[dynamic_hedge[,1]>="1994-01-01",1],
dynamic_hedge[dynamic_hedge[,1]>="1994-01-01",2],type="l",xlab="",
ylab="Hedge Ratio Brent/WTI",ylim=c(0.6,1.2),lwd=1.5);
grid(lwd=1.5);
dev.off();


#Set up the new dynamic hedge portfolio

#How many years are tested in the rolling back test?

obs<-length(dates[2:(length(dates)-testperiod)]);

#Prepare the data frame for the results
results_DLM<-data.frame(year=year(dates[2:(obs+1)]),ma=numeric(obs),exitZ=numeric(obs),
pnl=numeric(obs),Trades=numeric(obs),Winners=numeric(obs),Losers=numeric(obs),
MeanHold=numeric(obs),TimeInMarket=numeric(obs),pvalue=numeric(obs),MeanR=numeric(obs),
pMean=numeric(obs),SD=numeric(obs),pSD=numeric(obs),Sharpe=numeric(obs),pSharpe=numeric(obs),
SharpeBH=numeric(obs));



profit<-numeric(length(dates)-2);

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Test rolling 5-year periods						+
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

for(year in 2:(length(dates)-testperiod)){

#Move ahead, runMean and runSD are from TTR package
trade_subset<-crude_prices[crude_prices[,1]>=as.Date(dates[year])&crude_prices[,1]<as.Date(dates[year+testperiod]),];
names(trade_subset)<-c("Date","Fut_1","Fut_2");

hedge_ratio<-dynamic_hedge[dynamic_hedge[,1]>=as.Date(dates[year])&dynamic_hedge[,1]<as.Date(dates[year+testperiod]),2];
yport <- trade_subset$Fut_1 - hedge_ratio*trade_subset$Fut_2;

zscore<- (yport - runMean(yport,n=lb)) / runSD(yport,n=lb,sample=FALSE);
entryZscore <- 2.0;
exitZscore <- -0.2;

longsEntry <- zscore < -entryZscore;
longsExit <- zscore >= -exitZscore;
shortsEntry <- zscore > entryZscore;
shortsExit <- zscore <= exitZscore;

numUnitsLong <- numeric(length(yport));
numUnitsShort <- numeric(length(yport));

for(i in 1:length(numUnitsLong)){numUnitsLong[i]<-NA;numUnitsShort[i]<-NA;}

numUnitsLong[1]<-0;
numUnitsLong[longsEntry]<- 1;
numUnitsLong[longsExit]<- 0;
numUnitsLong<-fillMissingData(numUnitsLong);

numUnitsShort[1]<-0;
numUnitsShort[shortsEntry]<- -1;
numUnitsShort[shortsExit] <- 0;
numUnitsShort<-fillMissingData(numUnitsShort);

numUnits<-numUnitsLong + numUnitsShort;

#Check daily trades and calculate profit and loss
longPos<-0;
shortPos<-0;
pnl<-0;
commission<-0;
equity<-numeric(length(yport));
equity[1]<-startingCash;
cash<-startingCash;

#Boolean to check whether system is in the market

numTrades<-0;
numLong<-0;	#for significance testing
numShort<-0;	#for sginificance testing
winners<-0;
losers<-0;

for(i in 2:length(yport)){
	#Calculate daily return
	if(numUnits[i-1]==1){
		equity[i]<- cash + yport[i];}
	if(numUnits[i-1]==(-1)){
		equity[i]<- cash - yport[i];}

	if(numUnits[i-1]==0){
		equity[i]<-cash;}

	#Enter long position
	if(numUnits[i-1]!=1 & numUnits[i]==1){
		longPos<-yport[i];
		numTrades<-numTrades+1;
		numLong<-numLong+1;
		#Cash and Equity position
		cash<-cash - yport[i];

		#Transaction Costs
		cash<-cash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		commission<-commission+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
	}

	#Exit long position
	if(numUnits[i-1]==1 & numUnits[i]!=1){
		pnl <- pnl + (yport[i]-longPos);

		#Was it a winning trade?
		if((yport[i]-longPos)>=0){winners<-winners+1;}
		if((yport[i]-longPos)<0){losers<-losers+1;}
		cash<-cash + yport[i];
		#Transaction Costs
		cash<-cash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		commission<-commission+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		longPos<-0;
	}

	#Enter short position	
	if(numUnits[i-1]!=-1 & numUnits[i]==-1){
		shortPos<-yport[i];
		numTrades<-numTrades+1;
		numShort<-numShort+1;
		cash<-cash + yport[i];
		#Transaction Costs
		cash<-cash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		commission<-commission+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
	}

	#Exit short position
	if(numUnits[i-1]==-1 & numUnits[i]!=-1){
		pnl <- pnl + (shortPos - yport[i]);
		#Was it a winning trade?
		if((shortPos - yport[i])>=0){winners<-winners+1;}
		if((shortPos - yport[i])<0){losers<-losers+1;}
		cash<-cash - yport[i];
		#Transaction Costs
		cash<-cash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		commission<-commission+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		shortPos<-0;		
	}

	if(i==length(yport)){
		if(longPos!=0){
			pnl <- pnl + (yport[i]-longPos);			
			if((yport[i]-longPos)>=0){winners<-winners+1;} 
			if((yport[i]-longPos)<0){losers<-losers+1;}
			cash<-cash + yport[i];
			#Transaction Costs
			cash<-cash-(trade_subset$Fut_1[i]*fees + 
				(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
			commission<-commission+(trade_subset$Fut_1[i]*fees + 
				(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
			longPos<-0;
		}
		
		if(shortPos!=0){
			pnl <- pnl + (shortPos - yport[i]);
			
			if((shortPos - yport[i])>=0){winners<-winners+1;}
			if((shortPos - yport[i])<0){losers<-losers+1;} 
			cash<-cash - yport[i];
			#Transaction Costs
			cash<-cash-(trade_subset$Fut_1[i]*fees + 
				(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
			commission<-commission+(trade_subset$Fut_1[i]*fees + 
				(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
			shortPos<-0;
		}
	}	

}	#end i-loop

#print("Profit / Loss");
print(dates[year]);
#print(pnl);
#profit[year-1]<-pnl;
#print(numTrades);

pnl<-pnl-commission;

#Save results in dataframe
meanHold<-0;
dailyreturn<-calcReturn(equity);

results_DLM$pnl[year-1]<-pnl;
results_DLM$ma[year-1]<-lb;
results_DLM$exitZ[year-1]<-exitZscore;
results_DLM$Trades[year-1]<-numTrades;
results_DLM$Winners[year-1]<-winners;
results_DLM$Losers[year-1]<-losers;
if(numTrades>0){
	results_DLM$MeanHold[year-1]<- (sum(numUnitsLong)-sum(numUnitsShort))/numTrades;
	meanHold<-results_DLM$MeanHold[year-1];}

results_DLM$TimeInMarket[year-1]<-(sum(numUnitsLong)-sum(numUnitsShort)) / length(yport);
results_DLM$Sharpe[year-1]<-mean(dailyreturn)/sd(dailyreturn)*sqrt(250);
results_DLM$MeanR[year-1]<-mean(dailyreturn);
results_DLM$SD[year-1]<-sd(dailyreturn);


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create random trades							+ 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Needed for p-value calculations
numSamplesBetterThanStrategy<-0;
numSharpeBetterThanStrategy<-0;
numReturnsBetterThanStrategy<-0;
numSDHigherThanStrategy<-0;

samplePNLS<-numeric(0);
for(testrun in 1:boot_no){
	simLong<-randperm(1:length(yport),numLong);
	simShort<-randperm(1:length(yport),numShort);
	
	#Create simulated numUnits
	simUnitsLong<-numeric(length(yport));
	simUnitsShort<-numeric(length(yport));
	
	if(length(simLong)>0){
	for(i in 1:length(simLong)){
		if((simLong[i]+meanHold)<=length(yport)){
			simUnitsLong[simLong[i]:(simLong[i]+meanHold)]<-1;
		} else {simUnitsLong[simLong[i]:length(yport)]<-1;}
	}
	}
	
	if(length(simShort)>0){
	for(i in 1:length(simShort)){
		if((simShort[i]+meanHold)<=length(yport)){
			simUnitsShort[simShort[i]:(simShort[i]+meanHold)]<- -1;
		} else {simUnitsShort[simShort[i]:length(yport)]<- -1;}
	}
	}
	
	simUnits<-simUnitsLong + simUnitsShort;	
	simEquity<-numeric(length(yport));
	simEquity[1]<-startingCash;
	simCash<-startingCash;
	simCom<-0;		#Commission	

	for(i in 2:length(yport)){
	#Calculate daily return
	if(simUnits[i-1]==1){
		simEquity[i]<- simCash + yport[i];}
	if(simUnits[i-1]==(-1)){
		simEquity[i]<- simCash-yport[i];}
	if(simUnits[i-1]==0){
		simEquity[i]<- simCash;}
	
	#Record entries and exits
	if(simUnits[i-1]!=1 & simUnits[i]==1) {
		simCash<-simCash - yport[i];
		#Transaction Costs
		simCash<-simCash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		simCom<-simCom+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);		
	}
	if(simUnits[i-1]==1 & simUnits[i]!=1) {
		simCash<-simCash + yport[i];
		#Transaction Costs
		simCash<-simCash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		simCom<-simCom+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);}
	if(simUnits[i-1]!=(-1) & simUnits[i]==(-1)) {
		simCash<-simCash + yport[i];
		#Transaction Costs
		simCash<-simCash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		simCom<-simCom+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
	}
	if(simUnits[i-1]==(-1) & simUnits[i]!=(-1)) {
		simCash<-simCash - yport[i];
		#Transaction Costs
		simCash<-simCash-(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);	
		simCom<-simCom+(trade_subset$Fut_1[i]*fees + 
			(-hedge_ratio[i])*trade_subset$Fut_2[i]*fees);
	}
	
	}	#end i-loop


	if(numTrades>0){
		meanHold<-(sum(numUnitsLong)-sum(numUnitsShort))/numTrades;

		simPNL<-0;
		lastTradingDay<-length(yport);
		
		#Simulated Long Trades
		if(length(simLong)>0){	
			for(simTrades in 1:length(simLong)){
				enter<-simLong[simTrades];
				if((enter+meanHold)>lastTradingDay){
					simPNL<- simPNL + (yport[lastTradingDay]-yport[enter]); 
				} else{
					simPNL<- simPNL + (yport[enter+meanHold]-yport[enter]);
				}
			}
		}
	
		if(length(simShort)>0){
			for(simTrades in 1:length(simShort)){
				enter<-simShort[simTrades];
				if((enter+meanHold)>lastTradingDay){
					simPNL<- simPNL + (yport[enter]-yport[lastTradingDay]); 
				} else{
					simPNL<- simPNL + (yport[enter]-yport[enter+meanHold]);
				}
			}
		}

	}	#end if(numTrades>0)-loop

	simReturn<-calcReturn(simEquity);

	simPNL<-simPNL-simCom;

	if(simPNL>pnl) {numSamplesBetterThanStrategy<-numSamplesBetterThanStrategy+1;}
	if(mean(simReturn)>results_DLM$MeanR[year-1]){
		numReturnsBetterThanStrategy<-numReturnsBetterThanStrategy+1;}	
	if(sd(simReturn)>results_DLM$SD[year-1]){
		numSDHigherThanStrategy<-numSDHigherThanStrategy+1;}
	if((mean(simReturn)/sd(simReturn)*sqrt(250))>results_DLM$Sharpe[year-1]){
		numSharpeBetterThanStrategy<-numSharpeBetterThanStrategy+1;}
	
	#Just for Debugging
	#samplePNLS<-rbind(samplePNLS,simPNL);
}

results_DLM$pvalue[year-1]<-numSamplesBetterThanStrategy/boot_no;
results_DLM$pMean[year-1]<-numReturnsBetterThanStrategy/boot_no;
results_DLM$pSD[year-1]<-numSDHigherThanStrategy/boot_no;
results_DLM$pSharpe[year-1]<-numSharpeBetterThanStrategy/boot_no;

results_DLM$SharpeBH[year-1]<-mean(calcReturn(yport))/sd(calcReturn(yport))*sqrt(250);

}	#end of the year-loop

meanresults_DLM<-data.frame(apply(results_DLM,2,mean));
print(results_DLM);

filename<-paste("results_new_DLM",assets[1],assets[2],lb,"2",sep="_");
filetype<-"txt";

sink(paste(filename,filetype,sep="."));
table_DLM<-xtable(results_DLM,digits=c(0,0,0,1,4,0,0,0,2,4,4,4,4,4,4,4,4,4));
print(table_DLM,include.rownames=FALSE);

tablemean_DLM<-xtable(meanresults_DLM,digits=c(6));
print(tablemean_DLM,include.rownames=TRUE);
sink();

#Save Hedge Ratios
filename1<-paste("Hedge_Ratio_new_DLM",assets[1],assets[2],lb,"2",sep="_");
sink(paste(filename1,filetype,sep="."));
kalman_hedge_ratio;
sink();
