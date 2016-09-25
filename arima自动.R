autoarima <- function(x) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly = TRUE))
  stopifnot("package:tseries" %in% search() || require("tseries", quietly = TRUE))
  stopifnot("package:forecast" %in% search() || require("forecast", quietly = TRUE))
  stopifnot("package:zoo" %in% search() || require("zoo", quietly = TRUE))
  if (is.ts(x) == FALSE && is.xts(x) == FALSE) {
    print("Wrong data")
    break
  }
  print("-----------------------------------------------------------------------")
  print("Test for white-noise")
  a <- auto.arima(x)
  if (a$arma[6] == 0) {
    x1 = x
  } else {
    x1 <- diff(x, a$arma[6])[-1]
  }
  for (i in 1:2) print(Box.test(x1, type = "Ljung-Box", lag = 6 * i))
  a1 = Box.test(x1, type = "Ljung-Box", lag = 6)
  a2 = Box.test(x1, type = "Ljung-Box", lag = 12)
  if (min(a1$p.value, a2$p.value) < 0.05) {
    print("The series is not white noise!")
  } else {
    print("The series may be white noise!")
  }
  print("-----------------------------------------------------------------------")
  print(a)
  print("-----------------------------------------------------------------------")
  x.fit <- arima(x, order = c(a$arma[1], a$arma[6], a$arma[2]))
  print("Analysis for residual")
  for (i in 1:2) print(Box.test(x.fit$residuals, lag = 6 * i))
  a1 <- Box.test(x.fit$residuals, lag = 6)
  a2 <- Box.test(x.fit$residuals, lag = 12)
  if (min(a1$p.value, a2$p.value) < 0.05) {
    print("The residual series is not white noise!")
  } else {
    print("The residual series is white noise!")
  }
  print("-----------------------------------------------------------------------")
  x.fore <- forecast(x.fit, h = 10)
  print("Forecast for the series")
  print(x.fore)
  windows(300, 200)
  par(mfrow = c(2, 2))
  acf(x1)
  pacf(x1)
  plot(diff(x)^2, main = "Square Diff")
  plot(x.fore)
}

autoarima2 <- function(x) {
  stopifnot("package:tseries" %in% search() || require("tseries", quietly = TRUE))
  stopifnot("package:forecast" %in% search() || require("forecast", quietly = TRUE))
  stopifnot("package:zoo" %in% search() || require("zoo", quietly = TRUE))
  if (is.ts(x) == FALSE && is.xts(x) == FALSE) {
    print("Wrong data")
    break
  }
  print("-----------------------------------------------------------------------")
  print("Test for white-noise")
  for (i in 1:2) print(Box.test(x, type = "Ljung-Box", lag = 6 * i))
  a1 = Box.test(x, type = "Ljung-Box", lag = 6)
  a2 = Box.test(x, type = "Ljung-Box", lag = 12)
  if (min(a1$p.value, a2$p.value) < 0.05) {
    print("The series is not white noise!")
  } else {
    print("The series may be white noise!")
  }
  print("-----------------------------------------------------------------------")
  a <- auto.arima(x)
  print(a)
  print("-----------------------------------------------------------------------")
  x.fit <- arima(x, order = c(a$arma[1], a$arma[6], a$arma[2]), seasonal = list(order = c(0, 
                                                                                          1, 0), period = 12))
  print("Analysis for residual")
  for (i in 1:2) print(Box.test(x.fit$residuals, lag = 6 * i))
  a1 <- Box.test(x.fit$residuals, lag = 6)
  a2 <- Box.test(x.fit$residuals, lag = 12)
  if (min(a1$p.value, a2$p.value) < 0.05) {
    print("The residual series is not white noise!")
  } else {
    print("The residual series is white noise!")
  }
  print("-----------------------------------------------------------------------")
  x.fore <- forecast(x.fit, h = 10)
  print("Forecast for the series")
  print(x.fore)
  windows(300, 200)
  par(mfrow = c(2, 2))
  acf(x)
  pacf(x)
  plot(diff(x)^2, main = "Square Diff")
  plot(x.fore)
}


#画图检验拟合效果
L1<-SSEC.fore$fitted-1.96*sqrt(SSEC.fit$sigma2)
U1<-SSEC.fore$fitted+1.96*sqrt(SSEC.fit$sigma2)
L2<-ts(SSEC.fore$lower[,2],start=2009)
U2<-ts(SSEC.fore$upper[,2],start=2009)
c1<-min(Cl(SSEC),L1,L1)
c2<-max(Cl(SSEC),U1,U2)
windows(300,200);plot(Cl(SSEC),type="p",pch=8,ylim=c(c1,c2))
lines(SSEC.fore$fitted,col=2,lwd=2)
lines(SSEC.fore$mean,col=2,lwd=2)
lines(L1,col=4,lty=2)
lines(U1,col=4,lty=2)
lines(L1,col=4,lty=2)
lines(L2,col=4,lty=2)
lines(U2,col=4,lty=2)