plot.developB_N <-
  function(xx = devel14.df, post = TRUE, rnd = 7, check.rounding = FALSE)
{
### Purpose:- Attempt to find Tmin using nlxb instead of nls
### ----------------------------------------------------------------------
### Modified from:- plot.developB to fit more temperature points
### ----------------------------------------------------------------------
### Arguments:- rnd: how much to round coefficients
###             check.rounding: print out part of predictions
###                             from model and formula if req'd.
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 24 Apr 2013, 09:10
### ----------------------------------------------------------------------
### Revisions:- 21/10/2014 'coeffs' changed to 'coeff'

  require(reshape2)
  require(stringr)
  require(plyr)
  require(RColorBrewer)
  require(nlmrt)

  if(post){
    pdf(file = "Non-linearFitB_N.pdf", height = 195/25.4, width = 270/25.4)
    on.exit(dev.off(), add = TRUE)
                                        # par(mfrow = c(2, 1))
  }

  colours <- brewer.pal(8, "Dark2")[3:4]
## browser()
  new.df <- arrange(unique(xx[, 3:1]), Sex, TempNom, TempRecord)[, c(1,3,2)]
  ## Get table of means and SEs

  len.pred <- predict(update(length.lm14, . ~. -Sex), new.df, se.fit = TRUE)
  length.df <- within(new.df, Length <- round(len.pred$fit, 4))
  length.df <- within(length.df, SE <- round(len.pred$se.fit, 5))
  ## browser()
  ## Non-linear fit for development
################################## require(nlme) 
  xx <- within(xx, T <- TempRecord)
  xx <- within(xx, Dev <- 1/Days)

                                        #
  xxM <- xx[xx$Sex == "M",] ## subset won't work with nlxb()
  xxF <- xx[xx$Sex == "F",]
##  browser()
  rate.nlsM <- nlxb(Dev ~ exp(rho * (T - Tmin)) -
                    exp(rho*Tmax - (Tmax - T)/del) + lam,
                    data = xxM, ## trace = TRUE, 
                    start = c(rho = .14, del = 10, lam = .002, Tmax = 58, Tmin = .2))
  coefM <- as.list(rate.nlsM$coeff)
  rate.nlsF <- nlxb(Dev ~ exp(rho * (T - Tmin)) -
                    exp(rho*Tmax - (Tmax - T)/del)+ 
                    lam , data = xxF,#
                    start = c(rho = .14, del = 10, lam = .002, Tmax = 58, Tmin = .2))

##                    start = c(rho = .13, del = 8, lam = .02, Tmax = 38, Tmin = 1.2))

  coefF <- as.list(rate.nlsF$coeff)


  newdat <- data.frame(T = seq(0, 33, .025))

  predM <- with(coefM, exp(rho * (newdat$T -Tmin)) -
                exp(rho*Tmax - (Tmax - newdat$T)/del) + lam)
  predF <- with(coefF, exp(rho * (newdat$T-Tmin)) -
                exp(rho*Tmax - (Tmax - newdat$T)/del) + lam)
  

  use.df <- data.frame(T = newdat$T, F = predF, M = predM)
  predM[predM < 0] <- NA
  predF[predF < 0] <- NA
## browser()
  plot(newdat$T, predM, type = "l", col = colours[1],
       ##      xlab = substitute(paste(T *degree, K), list(T = "Temperature (", K = "C)")),
       ##     xlab = substitute(paste(Temperature, B * degree, "C)"), list(B = " ("))
       xlab = expression(Temperature~(degree*C)),
##       xlim = range(c(newdat$T, rate.nlsF$coeff["Tmax"])), forget Tmax
       main = "Non-linear fit using Lactin modified Logan model with min Temp",
       ##xlab = expression(paste(Temperature (,*degree, "C)")))
       ylab = "Development rate (/day)", cex.lab = 1.3)
  abline(v = rate.nlsM$coeff["Tmax"], col = colours[1], lty = 3)
  abline(v = rate.nlsF$coeff["Tmax"], col = colours[2], lty = 3)
  abline(h = 0, col = "grey")
  with(xx[xx$Sex == "M",], points(TempRecord, Dev, pch = 1, col = colours[1]))
  with(xx[xx$Sex == "F",], points(TempRecord, Dev, pch = 4, col = colours[2]))

  lines(newdat$T, predF, type = "l", col = colours[2])

  ## ToptF <- na.omit(with(use.df, T[F == max(F, na.rm = TRUE)]))[1]
  ## ToptM <- na.omit(with(use.df, T[M == max(M, na.rm = TRUE)]))[1]

  ## Linear section of development curve:
  xxM <- xx[xx$Sex == "M" & xx$T < 27,]
  xxF <- xx[xx$Sex == "F" & xx$T < 27,]
  lmM <- lm(Dev ~ T, data = xxM)
  lmF <- lm(Dev ~ T, data = xxF)
  abline(coef(lmM), col = colours[1], lty = 2)
  abline(coef(lmF), col = colours[2], lty = 2)
  use.temp.range <- range(xx$TempRecord[xx$TempRecord < 27])
  clipline(xlim = use.temp.range, ylim = c(0, 10), a = coef(lmM)[1],
           b = coef(lmM)[2],
           col = colours[1], lwd = 3)
  clipline(xlim = use.temp.range, ylim = c(0, 10), a = coef(lmF)[1],
           b = coef(lmF)[2],
           col = colours[2], lwd = 3)

  ## Equations for curved lines
  zF <- - round(with(coefF, Tmax *(rho - 1/del)), rnd + 1) # use minus in equation
  aF <- round(with(coefF, rho), rnd)
  dF <- round(1/with(coefF, del), rnd) # use inverse in equation
  cF <- - round(with(coefF, lam), rnd) # use minus in equation
  tF <- - round(with(coefF, Tmin), rnd) # use minus in equation
  formF <- substitute(D(T) == e^paste(a, {T+t}) - e^(d*T - z) - c,
                      list(a = aF, z = zF, d = dF, c = cF, t = tF))

  zM <- - round(with(coefM, Tmax *(rho - 1/del)), rnd + 1) # use minus in equation
  aM <- round(with(coefM, rho), rnd)
  dM <- round(1/with(coefM, del), rnd) # use inverse in equation
  cM <- - round(with(coefM, lam), rnd) # use minus in equation
  tM <- - round(with(coefM, Tmin), rnd) # use minus in equation
  formM <- substitute(D(T) == e^paste(a, {T+t}) - e^(d*T - z) - c,
                      list(a = aM, z = zM, d = dM, c = cM, t = tM))

  ## Formulas for straight lines
  intM <- round(coef(lmM)[1], rnd - 2)
  slopeM<- round(coef(lmM)[2], rnd - 1)
  formLinM <- substitute(D(T) == slopeM * T - intM , list(intM = -intM,
                            slopeM = slopeM))
  intF <- round(coef(lmF)[1], rnd - 2)
  slopeF<- round(coef(lmF)[2], rnd - 1)
  formLinF <- substitute(D(T) == slopeF * T - intF , list(intF = -intF,
                            slopeF = slopeF))


  ## Make legend: set up some variables
  ygap <- .0025
  ytop <- .048
  xleft <- 3
  xlen <- 3
  xgap <- 0.5
  leg.cex <- 1.1
  temp.cex <- .6 # temperature points of interest
  text(xleft, ytop - ygap, formF, cex = leg.cex, adj = 0)
  text(xleft, ytop, formM, cex = leg.cex, adj = 0)
  text(xleft, ytop - 3*ygap, formLinF, cex = leg.cex, adj = 0)
  text(xleft, ytop - 2*ygap, formLinM, cex = leg.cex, adj = 0)

  segments(xleft-xgap, y0 = ytop - ygap, x1 = xleft-xgap-xlen, y1 = ytop - ygap,
           col = colours[2], lwd = 1)
  segments(xleft-xgap, y0 = ytop , x1 = xleft-xgap-xlen, y1 = ytop ,
           col = colours[1], lwd = 1)
  segments(xleft-xgap, y0 = ytop - 3*ygap, x1 = xleft-xgap-xlen, y1 = ytop - 3*ygap,
           col = colours[2], lwd = 3)
  segments(xleft-xgap, y0 = ytop - 2*ygap, x1 = xleft-xgap-xlen, y1 = ytop - 2*ygap,
           col = colours[1], lwd = 3)

  ## Get interesting temperatures and max D(T)


  ## Slope of line
  T <-  seq(23, 27, .01) # use just this bit
##  browser()
  derivF <- with(coefF, deriv(~exp(rho*(T - Tmin)) -
                              exp(rho*Tmax - (Tmax - T)/del) + lam, "T"))
  derivM <- with(coefM, deriv(~exp(rho*(T - Tmin)) -
                              exp(rho*Tmax - (Tmax - T)/del) + lam, "T")) 

  gradientF <- attributes(with(coefF, eval(derivF)))$gradient
  gradientM <- attributes(with(coefM, eval(derivM)))$gradient
  ToptF <- approx(gradientF, T, xout = 0)$y # optimal temperatures
  ToptM <- approx(gradientM, T, xout = 0)$y
  dev.maxF <- exp(aF * (ToptF + tF)) - exp(dF * ToptF - zF) - cF # max dev rates
  dev.maxM <- exp(aM * (ToptM + tM)) - exp(dM * ToptM - zM) - cM

  ## identify max points
  idy <- .035 # limit of identifying lines
  idx <- 29
  segments(ToptF, idy, ToptF, dev.maxF, lwd = .3)#, col = "grey")
  segments(ToptM, idy, ToptM, dev.maxM, lwd = .3)#, col = "grey")
  segments(idx, dev.maxF, ToptF, dev.maxF, lwd = .3)#, col = "grey")
  segments(idx, dev.maxM, ToptM, dev.maxM, lwd = .3)#, col = "grey")
  text(ToptF - xgap/10, idy, round(ToptF, 2), cex = temp.cex,
       col = colours[2], adj = 1)
  text(ToptM + xgap/10, idy, round(ToptM, 2), cex = temp.cex,
       col = colours[1], adj = 0)
  text(idx + xgap/10, dev.maxF, round(dev.maxF, 4),
       cex = temp.cex, col = colours[2], adj = 0)
  text(idx + xgap/10, dev.maxM, round(dev.maxM, 4),
       cex = temp.cex, col = colours[1], adj = 0)



  MassyT <- coefM["Tmax"][[1]] # asymptote for male curve
  FassyT <- coefF["Tmax"][[1]]
  ## Mark in asymptotic temperatures
  text(FassyT + xgap/10, -0.0013, round(FassyT, 2), cex = temp.cex,
       col = colours[2], adj = 0)
  text(MassyT - xgap/10, -0.0013, round(MassyT, 2), cex = temp.cex,
       col = colours[1], adj = 1)

  ## Point where development begins (slight fudge: should approx all data
  begF <- with(use.df, approx(F, T, xout = 0)$y)
  begM <- with(use.df, approx(M, T, xout = 0)$y)
  ## Corresponding linear points
  begFlin <- -intF/slopeF
  begMlin <- -intM/slopeM

  ## Mark in begin temperatures
  text(begF, -0.0007, round(begF, 2), cex = temp.cex, col = colours[2], adj = 0)
  text(begM, 0.0007, round(begM, 2), cex = temp.cex, col = colours[1], adj = 1)
  text(begFlin, -0.0007, round(begFlin, 2), cex = temp.cex, col = colours[2],
       adj = 0)
  text(begMlin, 0.0007, round(begMlin, 2), cex = temp.cex, col = colours[1], adj = 1)


  ## General form of equation
  Ystart <- 0.022 # Begin table y value
  Xstart <- 20
  Xspace <- 1
  tab.cex <- .9
  tab.space <- 4 # width of table columns

  text(24, Ystart + 1.5*ygap, "Model coefficients", cex = leg.cex)
  text(24, Ystart - 7*ygap,
       expression(r(T) == e^paste(rho, (T-T[min])) - e^paste("[", rho* T[max] - ~
           ##         frac(paste("(", T[max] - T, ")"), delta), "]")  +lambda),
           frac(T[max]~ - T , delta), "]")  +lambda),
       cex = 1.2 * leg.cex)

  text(Xstart, Ystart - ygap, expression(rho), cex = leg.cex, adj = 1)
  text(Xstart, Ystart - 2*ygap, expression(T[max]), cex = leg.cex, adj = 1)
  text(Xstart, Ystart - 3*ygap, expression(T[min]), cex = leg.cex, adj = 1)
  text(Xstart, Ystart - 4*ygap, expression(delta), cex = leg.cex, adj = 1)
  text(Xstart, Ystart - 5*ygap, expression(lambda), cex = leg.cex, adj = 1)
  text(Xstart + Xspace, Ystart, "males", cex = tab.cex, adj = 0)
  text(Xstart + Xspace, Ystart - ygap, round(coefM$rho, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace, Ystart - 2*ygap, round(coefM$Tmax, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace, Ystart - 3*ygap, round(coefM$Tmin, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace, Ystart - 4*ygap, round(coefM$del, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace, Ystart - 5*ygap, round(coefM$lam, rnd),
       cex = tab.cex, adj = 0)

  text(Xstart + Xspace + tab.space, Ystart, "females", cex = tab.cex, adj = 0)
  text(Xstart + Xspace + tab.space, Ystart - ygap, round(coefF$rho, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace + tab.space, Ystart - 2*ygap, round(coefF$Tmax, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace + tab.space, Ystart - 3*ygap, round(coefF$Tmin, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace + tab.space, Ystart - 4*ygap, round(coefF$del, rnd),
       cex = tab.cex, adj = 0)
  text(Xstart + Xspace + tab.space, Ystart - 5*ygap, round(coefF$lam, rnd),
       cex = tab.cex, adj = 0)
  segments(Xstart- Xspace, Ystart - ygap/2, Xstart + 8,
           Ystart - ygap/2, lwd = .5)
  segments(Xstart + Xspace/2, Ystart+ ygap/2,  Xstart + Xspace/2,
           Ystart - 5.5*ygap, lwd = .5)
  segments(Xstart + Xspace/2+ tab.space, Ystart+ ygap/2,
           Xstart + Xspace/2+ tab.space, Ystart - 5.5*ygap, lwd = .5)


  if(check.rounding){
    ## Check if there is too much rounding of coefficients:
    use.df$Fline <- exp(aF * use.df$T) -  exp(-zF + use.df$T * dF) - cF  
    use.df$Mline <- exp(aM * use.df$T) -  exp(-zM + use.df$T * dM) - cM
    invisible()
    use.df[1000:1020,]
  }
}
