## This empties the work space ####
rm(list=ls())
library(ggplot2)
## change directory
## setwd("/path/to/work_dir")
setwd("~/Travail/DTU/Time_series/Ass3")

#OLD data !
## reading data,regarding the first lines in the file as names:
data_old <- read.table(file="A3_power.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)

data_old$Time<-paste(data_old$Date, data_old$Hour, sep=" ")
data_old$Time<-as.POSIXct(data_old$Time, format="%d-%m-%Y %H:%M:%S")
data_old$logPower <- log(data_old$Power)

n_old<-length(data_old$Date)
N_old<-15672
m_old<-15012
M_old<-8785

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

plot(data_old$Time[1:N_old], data_old$Power[1:N_old],col='black',
     xlim=c(data_old$Time[1],data_old$Time[n_old]), 
     ylim=c(min(data_old$Power),max(data_old$Power)),xlab='Time'
     ,ylab="Power consumption [MWh]",
     type='l',main='01/01/2016 - 17/10/2017 every hour')
lines(data_old$Time[N_old:n_old],data_old$Power[N_old:n_old],col='red')
legend('topleft',legend = c("Train data","Test data"), 
       col = c('black','red'), pch = c(NA,NA),
       cex=0.7, bty = 'n',lty = c(1,1))

#### New dataset ####
## reading data,regarding the first lines in the file as names:
data <- read.table(file="A3_power_short.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)

data$Time<-paste(data$Date, data$Hour, sep=" ")
data$Time<-as.POSIXct(data$Time, format="%d-%m-%Y %H:%M:%S")
data$logPower <- log(data$Power)

n<-length(data$Date)
N<-1672
m<-1337
d<-1625

#### Question 1 ####
par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

plot(data$Time[1:N], data$Power[1:N],col='black',xlim=c(data$Time[1],data$Time[n]), 
     ylim=c(min(data$Power),max(data$Power)),xlab='Time',ylab="Power consumption [MWh]",
     type='l',main='06/08/2017 - 17/10/2017 
     every hour')
lines(data$Time[N:n],data$Power[N:n],col='red')
legend('topleft',legend = c("Train data","Test data"), 
       col = c('black','red'), pch = c(NA,NA),
       cex=0.7, bty = 'n',lty = c(1,1))

#Last month
par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

plot(data$Time[m:N], data$Power[m:N],col='black',xlim=c(data$Time[m],data$Time[n]), 
     ylim=c(min(data$Power[m:n]),max(data$Power[m:n])),xlab='Time',ylab="Power consumption [MWh]",
     type='l',main='01/10/2016 - 17/10/2017 
     every hour')
lines(data$Time[N:n],data$Power[N:n],col='red')
legend('topleft',legend = c("Train data","Test data"), 
       col = c('black','red'), pch = c(NA,NA),
       cex=0.7, bty = 'n',lty = c(1,1))

#log
par(mfrow=c(2,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

plot(data$Time[1:N], data$logPower[1:N],col='black',xlim=c(data$Time[1],data$Time[n]), 
     ylim=c(min(data$logPower),max(data$logPower)),xlab='Time',ylab="Log Power consumption",
     type='l',main='06/08/2017 - 17/10/2017 
     every hour')
lines(data$Time[N:n],data$logPower[N:n],col='red')
legend('topleft',legend = c("Train data","Test data"), 
       col = c('black','red'), pch = c(NA,NA),
       cex=0.7, bty = 'n',lty = c(1,1))

#Log Last month
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

plot(data$Time[m:N], data$logPower[m:N],col='black',xlim=c(data$Time[m],data$Time[n]), 
     ylim=c(min(data$logPower[m:n]),max(data$logPower[m:n])),xlab='Time',ylab="Log Power consumption",
     type='l', main='01/10/2017 - 17/10/2017 
     every hour')
lines(data$Time[N:n],log(data$Power[N:n]),col='red')
legend('topleft',legend = c("Train data","Test data"), 
       col = c('black','red'), pch = c(NA,NA),
       cex=0.7, bty = 'n',lty = c(1,1))

#Differenciate
diff1<-diff(data$logPower[1:n],1)

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

plot(data$Time[1:N], diff1[1:N],col='black',xlim=c(data$Time[1],data$Time[n]), 
     ylim=c(min(diff1),max(diff1)),xlab='Time',ylab="Diff Power consumption [MWh]",
     type='l',main='06/08/2017 - 17/10/2017 
     every hour')
lines(data$Time[N:n],diff1[N:n],col='red')
legend('topleft',legend = c("Train data","Test data"), 
       col = c('black','red'), pch = c(NA,NA),
       cex=0.7, bty = 'n',lty = c(1,1))

################################Question 2#####################
l<-7
lag<-24*l+1

#Non transform sample
par(mfrow=c(2,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

acf(x = data$Power[1:N], lag = lag, type = 'correlation',ann=FALSE, lwd=2, 
    main ='')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)
for (i in 1:n)
{lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
       col='darkorange2')}

pacf(x = data$Power[1:N], lag = lag, ann=FALSE, lwd=2, main ='')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)
for (i in 1:n)
{lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
       col='darkorange2')}

my.tsdiag <- function(dat,   nlag = lag, ...){
  if(class(dat) == "Arima")
    dat <- dat$residuals
  oldpar <- par(mfrow=c(3,1), mgp=c(2,0.7,0), mar=c(3,3,1.5,1))
  on.exit(par(oldpar))
  acf(dat,lag = lag, type = 'correlation',ann=FALSE, lwd=2, 
      main ='')
  mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
  mtext(side = 2,text = 'ACF', line = 2, cex = 1)
  for (i in 1:n)
  {lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
         col='darkorange2')}
  pacf(dat,lag = lag, ann=FALSE, lwd=2,main ='')
  mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
  mtext(side = 2,text = 'PACF', line = 2, cex = 1)
  for (i in 1:n)
  {lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
         col='darkorange2')}
  
  pval <- sapply(1:nlag, function(i) Box.test(dat, i, type = "Ljung-Box")$p.value)
  plot(1L:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1), main = "p values for Ljung-Box statistic")
  abline(h = 0.05, lty = 2, col = "blue")
}

#log transformed Sample
par(mfrow=c(2,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

acf(x = data$logPower[1:N], lag = lag, type = 'correlation',ann=FALSE, lwd=2, 
    main ='')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)
for (i in 1:n)
{lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
       col='darkorange2')}

pacf(x = data$logPower[1:N], lag = lag, ann=FALSE, lwd=2, main ='')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)
for (i in 1:n)
{lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
       col='darkorange2')}

#Diff
par(mfrow=c(2,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))

acf(x = diff1[1:N], lag = lag, type = 'correlation',ann=FALSE, lwd=2, 
    main ='')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'ACF', line = 2, cex = 1)
for (i in 1:n)
{lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
       col='darkorange2')}

pacf(x = diff1[1:N], lag = lag, ann=FALSE, lwd=2, main ='')
mtext(side = 1,text = 'lag', line = 2, cex = 1.3)
mtext(side = 2,text = 'PACF', line = 2, cex = 1)
for (i in 1:n)
{lines(x = c(24*i,24*i), y = c(-200,200), type = 'l', lty = 2,lwd=1.4, 
       col='darkorange2')}

###### question 4
#Model1 (2, 1, 0)*(0, 1, 3)_24

Model1<-arima(x = data$logPower[1:N], order = c(2, 1, 0),
              seasonal = list(order = c(0, 1, 3), period = 24))
my.tsdiag(Model1)
Model1



Model2<-arima(x = data$logPower[1:N], order = c(2, 1, 0),
              seasonal = list(order = c(1, 1, 4), period = 24))
my.tsdiag(Model2)
Model2

Model3<-arima(x = data$logPower[1:N], order = c(3, 1, 1),
              seasonal = list(order = c(1, 1, 4), period = 24))
Model3
#aic = -6856.85
my.tsdiag(Model3)


model1<-arima(x = data$logPower[1:N], order = c(2, 1, 3),
             seasonal = list(order = c(1, 1, 2), period = 24))
my.tsdiag(model1)


## Comparing with random numbers ...
par(mfrow=c(2,1))
ts.plot(Model3$residuals, col='black',ylab='Residuals')
ts.plot(ts(rnorm(length(Model3$residuals))*sqrt(Model3$sigma2)),
        ylab='Simulated residuals') 

## Looking at distributional assumption
par(mfrow=c(1,2))
hist(Model3$residuals,probability=T,col='white',main='Histogram of the residuals', xlab='')
curve(dnorm(x,sd=sqrt(Model3$sigma2)), col=2, lwd=3, add = TRUE)

qqnorm(Model3$residuals)
qqline(Model3$residuals,col=2)


# sign test mean and sd:
Test <- data.frame(row.names=c(''))

Len <- length(Model3$residuals)
Test$Expected_sign_changes <- (Len-1)/2 #expected normal of sign changes
### sd: sqrt((Len-1)/4) 
### 95% interval:
Test$lower_bound <- (Len-1)/2 - 1.96 * sqrt(Len-1/4) 
Test$upper_bound <- (Len-1)/2 + 1.96 * sqrt(Len-1/4) 
### test:
res <- Model3$residuals
Test$Len_sign_changes <- sum( res[-1] * res[-length(res)]<0 )

### or:
binom.test(Len.sign.changes, length(Model3$residuals)-1)

####### Question 5: Prediction
Pred<-predict(Model3, n.ahead = n-N)

#95% confidence interval
sd<-Pred$se
tlower95<-qnorm(0.025)*sd
tupper95<-qnorm(0.975)*sd

Predupper<-Pred$pred+tupper95
Predlower<-Pred$pred+tlower95

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Time[d:n],c(data$logPower[d:(N+1)],rep(NA,n-N-1)), type = 'l',lwd=1,lty=1,ann=FALSE,
     bty='l',pch=4,cex=0.6,cex.axis = 0.7, mgp=c(3,0.5,0),
     ylim=c(min(Predlower),max(Predupper)))
lines(data$Time[(N+1):n],data$logPower[(N+1):n], type = 'l',
      lwd=1,lty=1,ann=FALSE,bty='l',pch=4,cex=0.6, cex.axis = 0.7, mgp=c(3,0.5,0), col ='red')
lines(data$Time[(N+1):n], Pred$pred, type = 'l',  lwd=1,lty=1,ann=FALSE,bty='l',pch=4,cex=0.6, 
      cex.axis = 0.7, mgp=c(3,0.5,0), col ='purple')
lines(data$Time[(N+1):n],Predupper, type = 'l',  lwd=1,lty=2,ann=FALSE,bty='l',pch=4,cex=0.6, 
      cex.axis = 0.7, mgp=c(3,0.5,0), col ='purple')
lines(data$Time[(N+1):n],Predlower, type = 'l',  lwd=1,lty=2,ann=FALSE,bty='l',pch=4,cex=0.6, 
      cex.axis = 0.7, mgp=c(3,0.5,0), col ='purple')

mtext(side = 1,text = 'Time (hour)', line = 1.5, cex = 0.85)
mtext(side = 2,text = 'Log-power consumption', line = 1.9, cex = 0.85)

legend('topleft',legend = c("Observations", "Measurements",'Predictions','95% prediction interval'),
       col = c("black","red",'purple','purple'),lty = c(1,1,1,2), pch = c(NA,NA,NA,NA),cex=0.5,
       bty = 'n')

table <- matrix(cbind(data$logPower[c(N+1,N+6,N+12,N+24,N+48)],Pred$pred[c(1,6,12,24,48)],Predlower[c(1,6,12,24,48)],Predupper[c(1,6,12,24,48)]),ncol=5,byrow=TRUE)
colnames(table) <- c("2017-10-15 0am","2017-10-15 5am","2017-10-15 11am","2017-10-15 23am"
                     ,"2017-10-16 23am")
rownames(table) <- c("Observations","Predictions","Lower bounderies","Upper bounderies")
table <- as.table(table)
table

#Original domain
#Prediction
Ini_Pred<-exp(Pred$pred)

#95% confidence interval
Ini_Predupper<-exp(Predupper)
Ini_Predlower<-exp(Predlower)

#PLot in the initial domain

par(mfrow=c(1,1))
par(mgp=c(2, 0.7,0), mar=c(3,3,2,1))
plot(data$Time[d:n],c(data$Power[d:(N+1)],rep(NA,n-N-1)), type = 'l',lwd=1,lty=1,ann=FALSE,
     bty='l',pch=4,cex=0.6,cex.axis = 0.7, mgp=c(3,0.5,0),
     ylim=c(min(Ini_Predlower),max(Ini_Predupper)))
lines(data$Time[(N+1):n],data$Power[(N+1):n], type = 'l',
      lwd=1,lty=1,ann=FALSE,bty='l',pch=4,cex=0.6, cex.axis = 0.7, mgp=c(3,0.5,0), col ='red')
lines(data$Time[(N+1):n], Ini_Pred, type = 'l',  lwd=1,lty=1,ann=FALSE,bty='l',pch=4,cex=0.6, 
      cex.axis = 0.7, mgp=c(3,0.5,0), col ='purple')
lines(data$Time[(N+1):n],Ini_Predupper, type = 'l',  lwd=1,lty=2,ann=FALSE,bty='l',pch=4,cex=0.6, 
      cex.axis = 0.7, mgp=c(3,0.5,0), col ='purple')
lines(data$Time[(N+1):n],Ini_Predlower, type = 'l',  lwd=1,lty=2,ann=FALSE,bty='l',pch=4,cex=0.6, 
      cex.axis = 0.7, mgp=c(3,0.5,0), col ='purple')

mtext(side = 1,text = 'Time (hour)', line = 1.5, cex = 0.85)
mtext(side = 2,text = 'Power consumption', line = 1.9, cex = 0.85)

legend('topleft',legend = c("Observations", "Measurements",'Predictions','95% prediction interval'),
       col = c("black","red",'purple','purple'),lty = c(1,1,1,2), pch = c(NA,NA,NA,NA),cex=0.5,
       bty = 'n')

table <- matrix(cbind(data$Power[c(N+1,N+6,N+12,N+24,N+48)],Ini_Pred[c(1,6,12,24,48)],Ini_Predlower[c(1,6,12,24,48)],Ini_Predupper[c(1,6,12,24,48)]),ncol=5,byrow=TRUE)
colnames(table) <- c("2017-10-15 0am","2017-10-15 5am","2017-10-15 11am","2017-10-15 23am"
                     ,"2017-10-16 23am")
rownames(table) <- c("Observations","Predictions","Lower bounderies","Upper bounderies")
table <- as.table(table)
table
