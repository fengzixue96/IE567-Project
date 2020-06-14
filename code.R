library('xts')
library('fOptions')

setwd("C:/Users/HP/Desktop/567 project/data")
index_vol <- read.csv("djx_vol.csv")
stock_vol <- read.csv("stocks_vol.csv")
index_prc <- read.csv("DJI.csv")
stock_prc <- read.csv("stocks.csv")

index_vol<-index_vol[index_vol$days==30,c('date','ticker','impl_volatility','impl_strike','delta','cp_flag')]
call_index_vol<-index_vol[index_vol$delta==50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]
put_index_vol<-index_vol[index_vol$delta==-50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]

stock_vol<-stock_vol[stock_vol$days==30,c('date','ticker','impl_volatility','impl_strike','delta','cp_flag')]
call_stock_vol<-stock_vol[stock_vol$delta==50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]
put_stock_vol<-stock_vol[stock_vol$delta==-50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]

call_vol<-rbind(call_index_vol,call_stock_vol)
put_vol<-rbind(put_index_vol,put_stock_vol)
unique(call_vol$ticker)

index_prc<-index_prc[,c('Date','Adj.Close')]
colnames(index_prc)<-c('date','close')
index_prc$ticker<-'DJX'
date<-call_index_vol$date
index_prc$date<-date
index_prc$close<-index_prc$close/100

stock_prc<-stock_prc[,c('date','PRC','TICKER')]
colnames(stock_prc)<-c('date','close','ticker')
stock_prc$ticker[stock_prc$ticker=='DWDP']<-'DD'
b<-c(unique(call_vol$ticker))
stock_prc<-stock_prc[stock_prc$ticker %in% b,]

prc<-rbind(index_prc,stock_prc)
a<-unique(prc$ticker)

c<-merge(prc,call_vol,by=c('date','ticker'))
p<-merge(prc,put_vol,by=c('date','ticker'))

colnames(c)<-c('date','ticker','S','vol','K','flag')   # c -- call options
colnames(p)<-c('date','ticker','S','vol','K','flag')   # p -- put options

stock_z<-unique(stock_prc$ticker)
#-----------------------------------------------------------------------------
#2018
index_vol2018 <- read.csv("djx_vol2018.csv")
stock_vol2018 <- read.csv("stocks_vol2018.csv")
index_prc2018 <- read.csv("DJI2018.csv")
stock_prc2018 <- read.csv("stocks2018.csv")

index_vol2018<-index_vol2018[index_vol2018$days==30,c('date','ticker','impl_volatility','impl_strike','delta','cp_flag')]
call_index_vol2018<-index_vol2018[index_vol2018$delta==50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]
put_index_vol2018<-index_vol2018[index_vol2018$delta==-50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]

stock_vol2018<-stock_vol2018[stock_vol2018$days==30,c('date','ticker','impl_volatility','impl_strike','delta','cp_flag')]
call_stock_vol2018<-stock_vol2018[stock_vol2018$delta==50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]
put_stock_vol2018<-stock_vol2018[stock_vol2018$delta==-50,c('date','ticker','impl_volatility','impl_strike','cp_flag')]

call_vol2018<-rbind(call_index_vol2018,call_stock_vol2018)
put_vol2018<-rbind(put_index_vol2018,put_stock_vol2018)
unique(call_vol2018$ticker)

index_prc2018<-index_prc2018[,c('Date','Adj.Close')]
colnames(index_prc2018)<-c('date','close')
index_prc2018$ticker<-'DJX'
date2018<-call_index_vol2018$date
index_prc2018$date<-date2018
index_prc2018$close<-index_prc2018$close/100

stock_prc2018<-stock_prc2018[,c('date','PRC','TICKER')]
colnames(stock_prc2018)<-c('date','close','ticker')
stock_prc2018$ticker[stock_prc2018$ticker=='DWDP']<-'DD'
b2018<-c(unique(call_vol2018$ticker))
stock_prc2018<-stock_prc2018[stock_prc2018$ticker %in% b2018,]

prc2018<-rbind(index_prc2018,stock_prc2018)
a2018<-unique(prc2018$ticker)

c2018<-merge(prc2018,call_vol2018,by=c('date','ticker'))
p2018<-merge(prc2018,put_vol2018,by=c('date','ticker'))

colnames(c2018)<-c('date','ticker','S','vol','K','flag')   # Only need S and vol changes to simulate the data in 2019
colnames(p2018)<-c('date','ticker','S','vol','K','flag')   

stock_z2018<-unique(stock_prc2018$ticker)

# Change rate calculation
change_c_S<-data.frame(date2018)
change_c_S[,c(b2018)]<-NA
change_c_vol<-data.frame(date2018)
change_c_vol[,c(b2018)]<-NA
change_p_S<-data.frame(date2018)
change_p_S[,c(b2018)]<-NA
change_p_vol<-data.frame(date2018)
change_p_vol[,c(b2018)]<-NA
for(i in 1:length(b2018))
{
  change_c_S[,b2018[i]]<-c2018$S[c2018$ticker==b2018[i]]
  change_c_vol[,b2018[i]]<-c2018$vol[c2018$ticker==b2018[i]]
  change_p_S[,b2018[i]]<-p2018$S[p2018$ticker==b2018[i]]
  change_p_vol[,b2018[i]]<-p2018$vol[p2018$ticker==b2018[i]]
}
change_c_S<-xts(x = change_c_S[,-1], order.by = as.Date(change_c_S[,1], format = "%Y/%m/%d")) 
change_c_vol<-xts(x = change_c_vol[,-1], order.by = as.Date(change_c_vol[,1], format = "%Y/%m/%d"))
change_p_S<-xts(x = change_p_S[,-1], order.by = as.Date(change_p_S[,1], format = "%Y/%m/%d")) 
change_p_vol<-xts(x = change_p_vol[,-1], order.by = as.Date(change_p_vol[,1], format = "%Y/%m/%d")) 
for(i in 1:length(b2018))
{
  change_c_S[,b2018[i]]<-1+diff(change_c_S[,b2018[i]])/change_c_S[,b2018[i]]           # call S's change rates
  change_c_vol[,b2018[i]]<-1+diff(change_c_vol[,b2018[i]])/change_c_vol[,b2018[i]]     # call vol's change rates
  change_p_S[,b2018[i]]<-1+diff(change_p_S[,b2018[i]])/change_p_S[,b2018[i]]           # put S's change rates
  change_p_vol[,b2018[i]]<-1+diff(change_p_vol[,b2018[i]])/change_p_vol[,b2018[i]]     # put vol¡®s change rates
}
change_c_S<-change_c_S[-1,]
change_c_vol<-change_c_vol[-1,]
change_p_S<-change_p_S[-1,]
change_p_vol<-change_p_vol[-1,]


#-----------------------------------------------------------------------------
rr <- read.csv("r.csv")
index_d <- read.csv("djx_dividend.csv")
stock_d <- read.csv("stock_dividend.csv")

colnames(rr)<-c('date','r')
r<-data.frame(date)
r$r<-NA
for(i in 1:length(date))
  r$r[i]<-0.01*mean(rr$r[rr$date==date[i]])  # r -- risk free rate

index_d<-index_d[,c('date','ticker','rate')]
colnames(index_d)<-c('date','ticker','d')
index_d$d<-index_d$d*0.01

# stock dividend
stock_d<-stock_d[,c('datadate','tic','div')]
colnames(stock_d)<-c('date','ticker','div')
stock_d<-stock_d[stock_d$ticker %in% b,]
d<-unique(b[!(b %in% stock_d$ticker)])  # (DJX) DIS UTX
stock<-unique(stock_d$ticker)

a<-which(stock_d$ticker==stock[1]&(!is.na(stock_d$div)))
if(a>21)
  for(j in (a-21):(a-1))
    stock_d$div[j]<-stock_d$div[a]
if(a<=21)
  for(j in 1:a)
    stock_d$div[j]<-stock_d$div[a]

for(i in 2:length(stock)){
  a<-which(stock_d$ticker==stock[i]&(!is.na(stock_d$div)))
  if(length(a)!=0)
  {
    if(stock_d$ticker[a-21]==stock[i])
      for(j in (a-21):(a-1))
        stock_d$div[j]<-stock_d$div[a]
      if(stock_d$ticker[a-21]!=stock[i])
        for(j in (a-21):(a-1))
          if(stock_d$ticker[j]==stock[i])
            stock_d$div[j]<-stock_d$div[a]
  }
}
stock_d$div[is.na(stock_d$div)]<-0


c<-merge(c,r,by='date')
p<-merge(p,r,by='date')

index_call<-merge(c,index_d,by=c('date','ticker')) 
index_put<-merge(p,index_d,by=c('date','ticker'))  

index_call <- xts(x = index_call[, -1], order.by = as.Date(index_call[, 1], format = "%Y/%m/%d")) # index call xts
index_put <- xts(x = index_put[, -1], order.by = as.Date(index_put[, 1], format = "%Y/%m/%d"))    # index put xts

stock_call<-merge(c,stock_d,by=c('date','ticker'),all=TRUE) 
stock_put<-merge(p,stock_d,by=c('date','ticker'),all=TRUE)  
stock_call<-stock_call[stock_call$ticker %in% stock_z,]
stock_put<-stock_put[stock_put$ticker %in% stock_z,]

stock_call$K[stock_call$K==0]<-NA
stock_call<-na.locf(stock_call,fromLast=T)
stock_put$K[stock_put$K==0]<-NA
stock_put<-na.locf(stock_put,fromLast=T)

stock_call <- xts(x = stock_call[,-1], order.by = as.Date(stock_call[, 1], format = "%Y/%m/%d")) 
stock_put <- xts(x = stock_put[,-1], order.by = as.Date(stock_put[, 1], format = "%Y/%m/%d"))    

stock_call$d<-as.character(log(1-as.numeric(stock_call$div)/as.numeric(stock_call$S))/(-21/252)) # stock call xts
stock_put$d<-as.character(log(1-as.numeric(stock_put$div)/as.numeric(stock_put$S))/(-21/252))    # stock put xts

# Change (2019)
ch_c_S<-data.frame(date)
ch_c_S[,c(b)]<-NA
ch_c_vol<-data.frame(date)
ch_c_vol[,c(b)]<-NA
ch_p_S<-data.frame(date)
ch_p_S[,c(b)]<-NA
ch_p_vol<-data.frame(date)
ch_p_vol[,c(b)]<-NA
for(i in 1:length(stock_z))
{
  ch_c_S[,stock_z[i]]<-stock_call$S[stock_call$ticker==stock_z[i]]
  ch_c_vol[,stock_z[i]]<-stock_call$vol[stock_call$ticker==stock_z[i]]
  ch_p_S[,stock_z[i]]<-stock_put$S[stock_put$ticker==stock_z[i]]
  ch_p_vol[,stock_z[i]]<-stock_put$vol[stock_put$ticker==stock_z[i]]
}
ch_c_S[,'DJX']<-c$S[c$ticker=='DJX']
ch_c_vol[,'DJX']<-c$vol[c$ticker=='DJX']
ch_p_S[,'DJX']<-p$S[p$ticker=='DJX']
ch_p_vol[,'DJX']<-p$S[p$ticker=='DJX']
 
for(i in 1:length(b))
{
  ch_c_S[2:63,b[i]]<-diff(as.numeric(ch_c_S[,b[i]]))        # call S's changes
  ch_c_vol[2:63,b[i]]<-diff(as.numeric(ch_c_vol[,b[i]]))    # call vol's changes
  ch_p_S[2:63,b[i]]<-diff(as.numeric(ch_p_S[,b[i]]))        # put S's changes
  ch_p_vol[2:63,b[i]]<-diff(as.numeric(ch_p_vol[,b[i]]))    # put vol¡®s changes
}

ch_c_S<-xts(x = ch_c_S[,-1], order.by = as.Date(ch_c_S[,1], format = "%Y/%m/%d")) 
ch_c_vol<-xts(x = ch_c_vol[,-1], order.by = as.Date(ch_c_vol[,1], format = "%Y/%m/%d"))
ch_p_S<-xts(x = ch_p_S[,-1], order.by = as.Date(ch_p_S[,1], format = "%Y/%m/%d")) 
ch_p_vol<-xts(x = ch_p_vol[,-1], order.by = as.Date(ch_p_vol[,1], format = "%Y/%m/%d"))

ch_c_S<-ch_c_S[-1,]
ch_c_vol<-ch_c_vol[-1,]
ch_p_S<-ch_p_S[-1,]
ch_p_vol<-ch_p_vol[-1,]

# Simulate the S and vol in 2019
index_call_sim<-index_call  # index_call after simulation
index_put_sim<-index_put    # index_put sfter simulation

index_call_sim$S[2:length(index_call_sim$ticker)]<-as.character(as.numeric(index_call_sim$S[1:(length(index_call_sim$ticker)-1)])*change_c_S[1:(length(index_call_sim$ticker)-1),1])
index_call_sim$vol[2:length(index_call_sim$ticker)]<-as.character(as.numeric(index_call_sim$vol[1:(length(index_call_sim$ticker)-1)])*change_c_vol[1:(length(index_call_sim$ticker)-1),1])
index_put_sim$S[2:length(index_put_sim$ticker)]<-as.character(as.numeric(index_put_sim$S[1:(length(index_put_sim$ticker)-1)])*change_p_S[1:(length(index_put_sim$ticker)-1),1])
index_put_sim$vol[2:length(index_put_sim$ticker)]<-as.character(as.numeric(index_put_sim$vol[1:(length(index_put_sim$ticker)-1)])*change_p_vol[1:(length(index_put_sim$ticker)-1),1])

index_call_sim<-index_call_sim[-1,]
index_put_sim<-index_put_sim[-1,]


stock_call_sim<-stock_call[-(1:30),]  # stock_call after simulation
stock_put_sim<-stock_put[-(1:30),]    # stock_put sfter simulation
for(i in 1:length(stock_z))
{
  stock_call_sim$S[stock_call_sim$ticker==stock_z[i]]<-as.character(as.numeric(stock_call_sim$S[stock_call_sim$ticker==stock_z[i]])*change_c_S[1:length(index_put_sim$ticker),stock_z[i]])
  stock_call_sim$vol[stock_call_sim$ticker==stock_z[i]]<-as.character(as.numeric(stock_call_sim$vol[stock_call_sim$ticker==stock_z[i]])*change_c_vol[1:length(index_put_sim$ticker),stock_z[i]])
  stock_put_sim$S[stock_put_sim$ticker==stock_z[i]]<-as.character(as.numeric(stock_put_sim$S[stock_put_sim$ticker==stock_z[i]])*change_p_S[1:length(index_put_sim$ticker),stock_z[i]])
  stock_put_sim$vol[stock_put_sim$ticker==stock_z[i]]<-as.character(as.numeric(stock_put_sim$vol[stock_put_sim$ticker==stock_z[i]])*change_p_vol[1:length(index_put_sim$ticker),stock_z[i]])
}

#----------------------------------------------------------------------------------------------------
# Binomial Tree
# Option price(2019)
cc_stock<-stock_call[,c('ticker')]
cc_stock$price<-as.character(GBSOption(TypeFlag="c",S=as.numeric(stock_call$S),X=as.numeric(stock_call$K),r=as.numeric(stock_call$r),Time=21/252,sigma=as.numeric(stock_call$vol),b=as.numeric(stock_call$d))@price)

pp_stock<-stock_put[,c('ticker')]
pp_stock$price<-as.character(GBSOption(TypeFlag="p",S=as.numeric(stock_put$S),X=as.numeric(stock_put$K),r=as.numeric(stock_put$r),Time=21/252,sigma=as.numeric(stock_put$vol),b=as.numeric(stock_put$d))@price)

cc_index<-index_call[,c('ticker')]
cc_index$price<-as.character(GBSOption(TypeFlag="c",S=as.numeric(index_call$S),X=as.numeric(index_call$K),r=as.numeric(index_call$r),Time=21/252,sigma=as.numeric(index_call$vol),b=as.numeric(index_call$d))@price)

pp_index<-index_put[,c('ticker')]
pp_index$price<-as.character(GBSOption(TypeFlag="p",S=as.numeric(index_put$S),X=as.numeric(index_put$K),r=as.numeric(index_put$r),Time=21/252,sigma=as.numeric(index_put$vol),b=as.numeric(index_put$d))@price)

# Option price(2018)
cc_stock2018<-stock_call_sim[,c('ticker')]
cc_stock2018$price<-as.character(GBSOption(TypeFlag="c",S=as.numeric(stock_call_sim$S),X=as.numeric(stock_call_sim$K),r=as.numeric(stock_call_sim$r),Time=21/252,sigma=as.numeric(stock_call_sim$vol),b=as.numeric(stock_call_sim$d))@price)

pp_stock2018<-stock_put_sim[,c('ticker')]
pp_stock2018$price<-as.character(GBSOption(TypeFlag="p",S=as.numeric(stock_put_sim$S),X=as.numeric(stock_put_sim$K),r=as.numeric(stock_put_sim$r),Time=21/252,sigma=as.numeric(stock_put_sim$vol),b=as.numeric(stock_put_sim$d))@price)

cc_index2018<-index_call_sim[,c('ticker')]
cc_index2018$price<-as.character(GBSOption(TypeFlag="c",S=as.numeric(index_call_sim$S),X=as.numeric(index_call_sim$K),r=as.numeric(index_call_sim$r),Time=21/252,sigma=as.numeric(index_call_sim$vol),b=as.numeric(index_call_sim$d))@price)

pp_index2018<-index_put_sim[,c('ticker')]
pp_index2018$price<-as.character(GBSOption(TypeFlag="p",S=as.numeric(index_put_sim$S),X=as.numeric(index_put_sim$K),r=as.numeric(index_put_sim$r),Time=21/252,sigma=as.numeric(index_put_sim$vol),b=as.numeric(index_put_sim$d))@price)


# Option Greeks
cc_stock$delta<-as.character(NA)
cc_stock$gamma<-as.character(NA)
cc_stock$theta<-as.character(NA)
cc_stock$rho<-as.character(NA)
cc_stock$vega<-as.character(NA)
cc_stock[,c('delta','gamma','theta','rho','vega')]<-as.character(sapply(c('delta','gamma','theta','rho','vega'), function(greek){GBSGreeks(Selection = greek,TypeFlag="c",S=as.numeric(stock_call$S),X=as.numeric(stock_call$K),r=as.numeric(stock_call$r),Time=21/252,sigma=as.numeric(stock_call$vol),b=as.numeric(stock_call$d))}))

pp_stock$delta<-as.character(NA)
pp_stock$gamma<-as.character(NA)
pp_stock$theta<-as.character(NA)
pp_stock$rho<-as.character(NA)
pp_stock$vega<-as.character(NA)
pp_stock[,c('delta','gamma','theta','rho','vega')]<-as.character(sapply(c('delta','gamma','theta','rho','vega'), function(greek){GBSGreeks(Selection = greek,TypeFlag="p",S=as.numeric(stock_put$S),X=as.numeric(stock_put$K),r=as.numeric(stock_put$r),Time=21/252,sigma=as.numeric(stock_put$vol),b=as.numeric(stock_put$d))}))

cc_index$delta<-as.character(NA)
cc_index$gamma<-as.character(NA)
cc_index$theta<-as.character(NA)
cc_index$rho<-as.character(NA)
cc_index$vega<-as.character(NA)
cc_index[,c('delta','gamma','theta','rho','vega')]<-as.character(sapply(c('delta','gamma','theta','rho','vega'), function(greek){GBSGreeks(Selection = greek,TypeFlag="c",S=as.numeric(index_call$S),X=as.numeric(index_call$K),r=as.numeric(index_call$r),Time=21/252,sigma=as.numeric(index_call$vol),b=as.numeric(index_call$d))}))

pp_index$delta<-as.character(NA)
pp_index$gamma<-as.character(NA)
pp_index$theta<-as.character(NA)
pp_index$rho<-as.character(NA)
pp_index$vega<-as.character(NA)
pp_index[,c('delta','gamma','theta','rho','vega')]<-as.character(sapply(c('delta','gamma','theta','rho','vega'), function(greek){GBSGreeks(Selection = greek,TypeFlag="p",S=as.numeric(index_put$S),X=as.numeric(index_put$K),r=as.numeric(index_put$r),Time=21/252,sigma=as.numeric(index_put$vol),b=as.numeric(index_put$d))}))

#--------------------------------------------------------------------------------------------------
# return
g<-data.frame(date)
g[,c(stock_z)]<-NA
cc_combine_prc<-xts(x = g[,-1], order.by = as.Date(g[,1], format = "%Y/%m/%d")) 
cc_combine_prc2018<-xts(x = g[,-1], order.by = as.Date(g[,1], format = "%Y/%m/%d")) 
for(i in 1:length(stock_z))
{
  cc_combine_prc[,stock_z[i]]<-cc_stock[cc_stock$ticker==stock_z[i],'price']
  cc_combine_prc2018[2:length(date),stock_z[i]]<-cc_stock2018[cc_stock2018$ticker==stock_z[i],'price']
}
cc_combine_prc$DJX<-cc_index$price
cc_combine_prc2018$DJX<-cc_index2018$price
cc_combine_prc<-cc_combine_prc[1:62,]
cc_combine_prc2018<-cc_combine_prc2018[2:63,]

pp_combine_prc<-xts(x = g[,-1], order.by = as.Date(g[,1], format = "%Y/%m/%d"))
pp_combine_prc2018<-xts(x = g[,-1], order.by = as.Date(g[,1], format = "%Y/%m/%d")) 
for(i in 1:length(stock_z))
{
  pp_combine_prc[,stock_z[i]]<-pp_stock[pp_stock$ticker==stock_z[i],'price']
  pp_combine_prc2018[2:length(date),stock_z[i]]<-pp_stock2018[pp_stock2018$ticker==stock_z[i],'price']
}
pp_combine_prc$DJX<-pp_index$price
pp_combine_prc2018$DJX<-pp_index2018$price
pp_combine_prc<-pp_combine_prc[1:62,]
pp_combine_prc2018<-pp_combine_prc2018[2:63,]

for(i in 1:62)
{
  cc_combine_prc2018$mean[i] <- mean(as.numeric(cc_combine_prc2018[i,1:30]))
  pp_combine_prc2018$mean[i] <- mean(as.numeric(pp_combine_prc2018[i,1:30]))
}


# Delta
# Gamma
# Theta
# Rho

#-----------------------------------------------------------------------------------------
# Vega Nuetral
# Call
l<-data.frame(date)
l[,c('stocks','index')]<-NA
vega<-xts(x = l[,-1], order.by = as.Date(l[,1], format = "%Y/%m/%d")) 
for(i in 1:length(date))
{
  vega$stocks[i]<-mean(as.numeric(cc_stock$vega[index(cc_stock)==date[i]]))
  vega$index[i]<-as.numeric(cc_index$vega[index(cc_index)==date[i]])
}
vega$k<-vega$index/vega$stocks

vega$stocks_sim[2:63]<-cc_combine_prc2018$mean
vega$index_sim[2:63]<-cc_combine_prc2018$DJX

vega$sim<-vega$stocks_sim*vega$k-vega$index_sim   # Portfolio Returns (sell 1 index -- buy k stocks)

vega$index_delta<-as.numeric(cc_index$delta)
vega$index_gamma<-as.numeric(cc_index$gamma)
vega$index_theta<-as.numeric(cc_index$theta)
vega$index_rho<-as.numeric(cc_index$rho)

for(i in 1:length(date))
{
  vega$stocks_delta[i]<-mean(as.numeric(cc_stock$delta[index(cc_stock)==date[i]]))
  vega$stocks_gamma[i]<-mean(as.numeric(cc_stock$gamma[index(cc_stock)==date[i]]))
  vega$stocks_theta[i]<-mean(as.numeric(cc_stock$theta[index(cc_stock)==date[i]]))
  vega$stocks_rho[i]<-mean(as.numeric(cc_stock$rho[index(cc_stock)==date[i]]))
}

vega$delta<-vega$stocks_delta*vega$k-vega$index_delta  # Potfolio Delta
vega$gamma<-vega$stocks_gamma*vega$k-vega$index_gamma  # Portfolio Gamma
vega$theta<-vega$stocks_theta*vega$k-vega$index_theta  # Portfolio Theta
vega$rho<-vega$stocks_rho*vega$k-vega$index_rho        # Portfolio Rho

vega$r<-mean(diff(r$r))      # Prediction of Risk free rate change
vega$t<-1/252    # The time period is one day
vega$price<-NA
for(i in 2:length(date))
  vega$price[i]<-mean(as.numeric(ch_c_S[i-1,1:31])) # Prediction of Price change

vega<-vega[-1,]

vega$risk<-vega$delta*vega$price+0.5*vega$gamma*vega$price^2+vega$theta*vega$t+vega$rho*vega$r

vega_neutral_call<-vega$risk
vega_neutral_call$k<-vega$k
vega_neutral_call$delta<-vega$delta
vega_neutral_call$gamma<-vega$gamma
vega_neutral_call$theta<-vega$theta
vega_neutral_call$rho<-vega$rho

# VaR
vega_neutral_call$sim<-vega$sim
VaR_vega_call = - quantile(vega_neutral_call$sim[1:62], 0.05) # VaR at 2019/07/01 for call portfolio



#-------
# Put
vega<-xts(x = l[,-1], order.by = as.Date(l[,1], format = "%Y/%m/%d")) 
for(i in 1:length(date))
{
  vega$stocks[i]<-mean(as.numeric(pp_stock$vega[index(pp_stock)==date[i]]))
  vega$index[i]<-as.numeric(pp_index$vega[index(pp_index)==date[i]])
}
vega$k<-vega$index/vega$stocks

vega$stocks_sim[2:63]<-pp_combine_prc2018$mean
vega$index_sim[2:63]<-pp_combine_prc2018$DJX

vega$sim<-vega$stocks_sim*vega$k-vega$index_sim   # Portfolio Returns (sell 1 index -- buy k stocks)

vega$index_delta<-as.numeric(pp_index$delta)
vega$index_gamma<-as.numeric(pp_index$gamma)
vega$index_theta<-as.numeric(pp_index$theta)
vega$index_rho<-as.numeric(pp_index$rho)

for(i in 1:length(date))
{
  vega$stocks_delta[i]<-mean(as.numeric(pp_stock$delta[index(pp_stock)==date[i]]))
  vega$stocks_gamma[i]<-mean(as.numeric(pp_stock$gamma[index(pp_stock)==date[i]]))
  vega$stocks_theta[i]<-mean(as.numeric(pp_stock$theta[index(pp_stock)==date[i]]))
  vega$stocks_rho[i]<-mean(as.numeric(pp_stock$rho[index(pp_stock)==date[i]]))
}

vega$delta<-vega$stocks_delta*vega$k-vega$index_delta  # Potfolio Delta
vega$gamma<-vega$stocks_gamma*vega$k-vega$index_gamma  # Portfolio Gamma
vega$theta<-vega$stocks_theta*vega$k-vega$index_theta  # Portfolio Theta
vega$rho<-vega$stocks_rho*vega$k-vega$index_rho        # Portfolio Rho

vega$r<-mean(diff(r$r))      # Prediction of Risk free rate change
vega$t<-1/252    # The time period is one day
vega$price<-NA
for(i in 2:length(date))
  vega$price[i]<-mean(as.numeric(ch_p_S[i-1,1:31])) # Prediction of Price change

vega<-vega[-1,]

vega$risk<-vega$delta*vega$price+0.5*vega$gamma*vega$price^2+vega$theta*vega$t+vega$rho*vega$r

vega_neutral_put<-vega$risk
vega_neutral_put$k<-vega$k
vega_neutral_put$delta<-vega$delta
vega_neutral_put$gamma<-vega$gamma
vega_neutral_put$theta<-vega$theta
vega_neutral_put$rho<-vega$rho

# VaR
vega_neutral_put$sim<-vega$sim
VaR_vega_put = - quantile(vega_neutral_put$sim[1:62], 0.05) # VaR at 2019/07/01 for put portfolio


#--------------------------------------------------------------------------------------------
# Theta Neutral
# Call
theta<-xts(x = l[,-1], order.by = as.Date(l[,1], format = "%Y/%m/%d")) 
for(i in 1:length(date))
{
  theta$stocks[i]<-mean(as.numeric(cc_stock$theta[index(cc_stock)==date[i]]))
  theta$index[i]<-as.numeric(cc_index$theta[index(cc_index)==date[i]])
}
theta$k<-theta$index/theta$stocks  # 1 index -- k stocks 

theta$stocks_sim[2:63]<-cc_combine_prc2018$mean
theta$index_sim[2:63]<-cc_combine_prc2018$DJX

theta$sim<-theta$stocks_sim*theta$k-theta$index_sim   # Portfolio Returns (sell 1 index -- buy k stocks)

theta$index_delta<-as.numeric(cc_index$delta)
theta$index_gamma<-as.numeric(cc_index$gamma)
theta$index_vega<-as.numeric(cc_index$vega)
theta$index_rho<-as.numeric(cc_index$rho)

for(i in 1:length(date))
{
  theta$stocks_delta[i]<-mean(as.numeric(cc_stock$delta[index(cc_stock)==date[i]]))
  theta$stocks_gamma[i]<-mean(as.numeric(cc_stock$gamma[index(cc_stock)==date[i]]))
  theta$stocks_vega[i]<-mean(as.numeric(cc_stock$vega[index(cc_stock)==date[i]]))
  theta$stocks_rho[i]<-mean(as.numeric(cc_stock$rho[index(cc_stock)==date[i]]))
}

theta$delta<-theta$stocks_delta*theta$k-theta$index_delta  # Potfolio Delta
theta$gamma<-theta$stocks_gamma*theta$k-theta$index_gamma  # Portfolio Gamma
theta$vega<-theta$stocks_vega*theta$k-theta$index_vega  # Portfolio Vega
theta$rho<-theta$stocks_rho*theta$k-theta$index_rho        # Portfolio Rho

theta$r<-mean(diff(r$r))      # Prediction of Risk free rate change
theta$price<-NA
theta$vol<-NA

for(i in 2:length(date))
{
  theta$vol[i]<-mean(as.numeric(ch_c_S[i-1,1:31]))     # Prediction of Price change
  theta$price[i]<-mean(as.numeric(ch_c_vol[i-1,1:31])) # Prediction of Volatility change
}

theta<-theta[-1,]

theta$risk<-theta$delta*theta$price+0.5*theta$gamma*theta$price^2+theta$vega*theta$vol+theta$rho*theta$r

theta_neutral_call<-theta$risk
theta_neutral_call$k<-theta$k
theta_neutral_call$delta<-theta$delta
theta_neutral_call$gamma<-theta$gamma
theta_neutral_call$vega<-theta$vega
theta_neutral_call$rho<-theta$rho

# VaR
theta_neutral_call$sim<-theta$sim
VaR_theta_call = - quantile(theta_neutral_call$sim[1:62], 0.05) # VaR at 2019/07/01 for call portfolio

#------
#Put
theta<-xts(x = l[,-1], order.by = as.Date(l[,1], format = "%Y/%m/%d")) 
for(i in 1:length(date))
{
  theta$stocks[i]<-mean(as.numeric(pp_stock$theta[index(pp_stock)==date[i]]))
  theta$index[i]<-as.numeric(pp_index$theta[index(pp_index)==date[i]])
}
theta$k<-theta$index/theta$stocks  # 1 index -- k stocks 

theta$stocks_sim[2:63]<-pp_combine_prc2018$mean
theta$index_sim[2:63]<-pp_combine_prc2018$DJX

theta$sim<-theta$stocks_sim*theta$k-theta$index_sim   # Portfolio Returns (sell 1 index -- buy k stocks)

theta$index_delta<-as.numeric(pp_index$delta)
theta$index_gamma<-as.numeric(pp_index$gamma)
theta$index_vega<-as.numeric(pp_index$vega)
theta$index_rho<-as.numeric(pp_index$rho)

for(i in 1:length(date))
{
  theta$stocks_delta[i]<-mean(as.numeric(pp_stock$delta[index(pp_stock)==date[i]]))
  theta$stocks_gamma[i]<-mean(as.numeric(pp_stock$gamma[index(pp_stock)==date[i]]))
  theta$stocks_vega[i]<-mean(as.numeric(pp_stock$vega[index(pp_stock)==date[i]]))
  theta$stocks_rho[i]<-mean(as.numeric(pp_stock$rho[index(pp_stock)==date[i]]))
}

theta$delta<-theta$stocks_delta*theta$k-theta$index_delta  # Potfolio Delta
theta$gamma<-theta$stocks_gamma*theta$k-theta$index_gamma  # Portfolio Gamma
theta$vega<-theta$stocks_vega*theta$k-theta$index_vega  # Portfolio Vega
theta$rho<-theta$stocks_rho*theta$k-theta$index_rho        # Portfolio Rho

theta$r<-mean(diff(r$r))      # Prediction of Risk free rate change
theta$price<-NA
theta$vol<-NA

for(i in 2:length(date))
{
  theta$vol[i]<-mean(as.numeric(ch_p_S[i-1,1:31]))     # Prediction of Price change
  theta$price[i]<-mean(as.numeric(ch_p_vol[i-1,1:31])) # Prediction of Volatility change
}

theta<-theta[-1,]

theta$risk<-theta$delta*theta$price+0.5*theta$gamma*theta$price^2+theta$vega*theta$vol+theta$rho*theta$r

theta_neutral_put<-theta$risk
theta_neutral_put$k<-theta$k
theta_neutral_put$delta<-theta$delta
theta_neutral_put$gamma<-theta$gamma
theta_neutral_put$vega<-theta$vega
theta_neutral_put$rho<-theta$rho

# VaR
theta_neutral_put$sim<-theta$sim
VaR_theta_put = - quantile(theta_neutral_put$sim[1:62], 0.05) # VaR at 2019/07/01 for put portfolio



colnames(vega_neutral_call)<-c('Greek Risk','k','Delta','Gamma','Theta','Rho','Simulated Value P&L')
colnames(vega_neutral_put)<-c('Greek Risk','k','Delta','Gamma','Theta','Rho','Simulated Value P&L')
colnames(theta_neutral_call)<-c('Greek Risk','k','Delta','Gamma','Vega','Rho','Simulated Value P&L')
colnames(theta_neutral_put)<-c('Greek Risk','k','Delta','Gamma','vega','Rho','Simulated Value P&L')


# Output Result
write.csv(vega_neutral_call, file = "vega_neutral_call.csv")
write.csv(vega_neutral_put, file = "vega_neutral_put.csv")
write.csv(theta_neutral_call, file = "theta_neutral_call.csv")
write.csv(theta_neutral_put, file = "theta_neutral_put.csv")

S_vol_change<-theta$price
S_vol_change$vol<-theta$vol
write.csv(S_vol_change,file = "S_vol_change.csv")
  
  







