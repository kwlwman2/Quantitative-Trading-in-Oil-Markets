# Quantitative-Trading-in-Oil-Markets 
(USC Class FBE 551, A-grade project)

**Team member**:
Imani Mufti, Jacbo Lee, Leon Man, Roman Sielewicz, Selene Xiang

## Project Description
*********************
This repository is to show case our class project in which we leveraged quantitative trading coccepts and python programming to design a workable quantitative trading strategies in the energy market


## Introduction
*********************
During the pandemic, energy markets have become extremely volatile due to the government-issued stay-at-home orders as well as depressed demand for many petroleum-based goods. In our final project, our team drew inspiration from a paper by Lubnau and Todorova (2015) and will focus on energy futures markets. As Ramirez and Rodriguez (2008) stated, commodity price patterns, especially in oil markets, are considered relatively efficient in the long term but subject to political and economic shocks in the short term. Also, based on the findings of Tabak and Cajueiro (2007), the energy markets might not be as efficient in the short term as they seem to be. Therefore our team will try to explore trading opportunities in energy markets by applying momentum, volatility and mean-reverting strategies. 


## Data Description
*********************
Datasets used in this article are obtained from the paper by Lubnau and Todorova (2015), and these datasets include the settlement price for the futures contracts with different maturity for WTI Crude Oil, Brent Crude Oil, Natural Gas, Heating Oil, and Gasoline. All futures in our samples are tradable at the CME (at both Globex and ClearPort Electronic platforms). Therefore our trading strategies are viable in reality.


## Implementation
*********************
### 1. Mean reversion strategy
The mean reversion strategy assumes that asset prices and returns eventually will revert to the long-run mean or average level; in other words, mean-reverting trading tries to capitalize on extreme changes in the pricing of a particular security, assuming that the asset price will revert to its previous state (e.g. if the asset price outperforms, it will underperform in the future and vice versa.). However, a single asset that is mean-reverting is hard to find. Therefore, instead of applying a mean-reverting trading strategy on any single energy future, we decided to focus on the spread between energy futures, as the spreads between some pairs of futures are more likely to be stationary and show a mean-reverting pattern. Such stationary spread can be constructed by buying and selling two cointegrated futures with an appropriate hedge ratio. 

### 2. Momentum
In addition to the mean-reversion strategies, we also tested a cross sectional momentum based strategy. To implement our momentum strategy, we calculated the return momentum of all commodities over the previous 15, 30, 45 and 60 days. For each time horizon we then separated the assets into quintiles, with quintile 1 having the lowest return momentum, and quintile 5 having the highest. Essentially, quintile 5 indicates assets we want to long, and quintile 1 indicates assets we want to short. Overall, this resulted in 4 similar momentum based trading strategies, with each differing only in the number of days used to calculate momentum for the entry and exit signals. This strategy aims to capitalize on existing momentum in the returns of the futures contracts. 

### 3. Volatility
To implement the volatility strategy, we calculated the volatility of the returns for all commodities over the previous week, month and the relative volatility. Similar to the momentum strategy, we used the quintile approach to devise our trading strategy. We tried two approaches within volatility:
Buy when volatility is low, sell when volatility is high.
* We buy futures contracts which are in quintile 1 and sell contracts which are in quintile 5. Buy when volatility is high, sell when volatility is low.
* We buy futures contracts which are in quintile 5 and sell contracts which are in quintile 1.

## Highlight of Results
*********************
### 1. Mean reversion strategy
![meanrevertingstrategy]()
### 2. Momentum
![meanrevertingstrategy]()
### 3. Vol
![meanrevertingstrategy]()


