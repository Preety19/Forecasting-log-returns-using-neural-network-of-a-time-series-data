library(forecast)

library(tseries)

data=X0P0000SELC_BO_1_ 
attach(data)
View(data)
dummy1=as.factor(Dummy)


####converting data into ts ,plotting its graph and checking for its stationarity ####
data.ts1=ts(data[-1,10],start = c(2018,1),end =c(2020,243), frequency = 243)

d=data.frame(dummy1,Close)
plot(d,type="l")

plot(data.ts1)
adf.test(data.ts) #prices not stationary


plot.ts(data.ts,main="Daily Nav of HSBC-CANARA FUND")
adf.test(data.ts1)
pp.test(data.ts1)


length(d$dummy1)
length(d$Close)

### for checking linearity ####
install.packages("modifiedmk")
library(modifiedmk)
x=c(data.ts1)
#h0: no linearity present
mkttest(x)
# reject ho , thus linearity present



###plotting graph,acf and pacf simulatenously ####
tsdisplay(data.ts1)

pacf(data.ts1) # 1 spike
acf(data.ts1) # 
#fetching the parameters for arima
auto.arima(data.ts1)




####fitting the arima/arma model for above obtained parameters####
fitlin1 <- Arima(data.ts1, order = c(2,0,0))
summary(fitlin1)

fitlin1$coef
fitlin1$sigma2
fitlin1$var.coef
fitlin1$mask
fitlin1$loglik
fitlin1$model




##checking the fit of arch ####
install.packages("FinTS")
library(FinTS)
ArchTest(data.ts1)
#arch effect present

# analysis of the residuals
arga1 <- fitlin1$residuals
tsdisplay(arga1)#lag


tsdisplay((arga1)**2)#lag

#From the analysis of the ACF and PACF of the squares of the linear model residuals 
#we conclude that there is a case for a changing variance over time.
#As the pattern is conducive to an ARMA model we will do a simulation with multiple GARCH models to assist us on the fit decision.



###grach####
install.packages("fGarch")
library(fGarch)
??fgarch
??garchfit
# univariate Time series
Model_1 <- garchFit(formula = ~arma(2,0) + garch(1,1), data=data.ts1)
summary(Model_1)

plot(Model_1)

predict(Model_1)


# parameters of Garch model
garch_par=Model_1@fit$par
garch_par





### not taking this one  ####
#High Level GARCH spec class to hold the univariate and multivariate spec objects.
install.packages("rugarch")
library(rugarch)
gspec.ru <- ugarchspec(mean.model=list( armaOrder=c(2,0)), distribution="std")
gfit.ru1 <- ugarchfit(gspec.ru, data.ts1)
gfit.ru1



mod=fitlin1$model

#### Kalman filter ####
library(stats)
??KalmanLike()
# for arima 
kalman_fit=KalmanForecast(30,fitlin1$model)
kalman_fit
plot(kalman_fit$pred , type = "l")


#for garch
kalman_fit1=KalmanForecast(30,Model_1@data)
kalman_fit1

#KalmanRun(data.ts1,mod)
# for arima
kalman_pred=kalman_fit$pred + fitlin1$coef
kalman_pred

# passing garch
kalman_pred1 = kalman_fit$pred + Model_1@fit$par
kalman_pred1







#trial and error####
install.packages("bssm")
library(bssm)

??ekf
gfit_matrix=as.matrix(gfit.ru1@model)

ekf_1=ekf(gfit.ru1@model)

ekf(as.matrix(gfit.ru1@model))

a=as.matrix(gfit.ru1@model$modeldata$data)

ekf_2=ekf(scaled)
is.atomic(a)
is.recursive(a)

b=as.matrix(gfit.ru1@model["modeldata"["data"]])
b
ekf(b)
c=as.matrix(data.ts1)




#neural network ####
install.packages("nnfor")
library(nnfor)
??`nnfor-package`

??nnetar

nnetar(data.ts1,p=40)
plot(forecast(nnetar(data.ts1,p=40),h=40)) # good !

library(neuralnet)
??neuralnet


#stats package 

bds.test(data.ts1,m=2)
??bds.test







