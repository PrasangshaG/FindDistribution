# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

distributionPG <- function(t) {
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  
  
  n <- length(t)
  str(t)
  
  sortt <- sort(t)
  
  
  dat <- sortt
  meanda <- mean(dat)
  stdev <- sd(dat)
  
  ################  Function to test if the data vector is integer ################
  testInteger <- function(x, n){
    for(i in 1:n){
      test <-  x[i] - as.integer(x[i])
      if(test == 0){ flag = TRUE }
      else { return(FALSE) }
    }
    if(flag == TRUE)
      return(TRUE)
  }
  
  ti <- testInteger(t, n)
  ################  Function to test if the data vector is non negative ################
  testpos <- function(x, n){
    for(i in 1:n){
      if(x[i] >= 0){ flag = TRUE }
      else { return(FALSE) }
    }
    if(flag == TRUE)
      return(TRUE)
  }
  
  
  tp <- testpos(t, n)
  
  
  ################  Function to test if the data vector is in [0,1] ################
  testbeta <- function(x, n){
    for(i in 1:n){
      if(x[i] >= 0 && x[i] <=1){ flag = TRUE }
      else { return(FALSE) }
    }
    if(flag == TRUE)
      return(TRUE)
  }
  
  
  tbeta <- testbeta(t, n)
  
  
  
  ###################################################
  ####### p is a vector of probabilities #######
  p <- c(0.2, 0.4, 0.6, 0.8)
  
  
  expectedcell1 = 0.2*n
  expectedcell2 = 0.2*n
  expectedcell3 = 0.2*n
  expectedcell4 = 0.2*n
  expectedcell5 = 0.2*n
  
  
  ################  Uniform distribution ##################
  checkunif <-function(dat, p, meanda, stdev)
  {
    observedunif1 =0
    observedunif2 =0
    observedunif3 =0
    observedunif4 =0
    observedunif5 =0
    
    minval = meanda-1.732*stdev
    maxval = 1.732*stdev + meanda
    quantuniform <- qunif(p, minval, maxval)
    
    dat[1]
    for(i in 1:n)
    {
      if(dat[i] <= quantuniform[1])
      {
        observedunif1 = observedunif1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantuniform[1] && dat[i] <=quantuniform[2])
      {
        observedunif2 = observedunif2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantuniform[2] && dat[i] <=quantuniform[3])
      {
        observedunif3 = observedunif3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantuniform[3] && dat[i] <=quantuniform[4])
      {
        observedunif4 = observedunif4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantuniform[4])
      {
        observedunif5 = observedunif5 +1
      }
    }
    
    observedunif <- c(observedunif1, observedunif2, observedunif3, observedunif4, observedunif5, minval, maxval)
    return(observedunif)
  }
  
  #################### Poisson Distribution ############
  checkpois <- function(dat, p, meanda, stdev)
  {
    observedpois1 =0
    observedpois2 =0
    observedpois3 =0
    observedpois4 =0
    observedpois5 =0
    
    lambda = meanda
    quantpois <- qpois(p, lambda)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantpois[1])
      {
        observedpois1 = observedpois1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantpois[1] && dat[i] <=quantpois[2])
      {
        observedpois2 = observedpois2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantpois[2] && dat[i] <=quantpois[3])
      {
        observedpois3 = observedpois3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantpois[3] && dat[i] <=quantpois[4])
      {
        observedpois4 = observedpois4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantpois[4])
      {
        observedpois5 = observedpois5 +1
      }
    }
    
    observedpois <- c(observedpois1, observedpois2, observedpois3, observedpois4, observedpois5, lambda)
    return(observedpois)
  }
  
  
  #################### Exponential Distribution ############
  checkexp <- function(dat, p, meanda, stdev)
  {
    observedexp1 =0
    observedexp2 =0
    observedexp3 =0
    observedexp4 =0
    observedexp5 =0
    
    lambda = 1/meanda
    quantexp <- qexp(p, lambda)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantexp[1])
      {
        observedexp1 = observedexp1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantexp[1] && dat[i] <=quantexp[2])
      {
        observedexp2 = observedexp2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantexp[2] && dat[i] <=quantexp[3])
      {
        observedexp3 = observedexp3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantexp[3] && dat[i] <=quantexp[4])
      {
        observedexp4 = observedexp4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantexp[4])
      {
        observedexp5 = observedexp5 +1
      }
    }
    
    observedexp <- c(observedexp1, observedexp2, observedexp3, observedexp4, observedexp5, lambda)
    return(observedexp)
  }
  
  
  
  
  #################### Normal Distribution ############
  checknorm <- function(dat, p, meanda, stdev)
  {
    observednor1 =0
    observednor2 =0
    observednor3 =0
    observednor4 =0
    observednor5 =0
    
    mu = meanda
    st = stdev
    v = st*st
    quantnorm <- qnorm(p, mu, st)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantnorm[1])
      {
        observednor1 = observednor1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantnorm[1] && dat[i] <=quantnorm[2])
      {
        observednor2 = observednor2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantnorm[2] && dat[i] <=quantnorm[3])
      {
        observednor3 = observednor3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantnorm[3] && dat[i] <=quantnorm[4])
      {
        observednor4 = observednor4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantnorm[4])
      {
        observednor5 = observednor5 +1
      }
    }
    
    observednor <- c(observednor1, observednor2, observednor3, observednor4, observednor5, mu, v)
    return(observednor)
  }
  
  
  
  
  ################## Gamma distribution ############
  checkgamma <- function(dat, p, meanda, stdev)
  {
    observedgam1 =0
    observedgam2 =0
    observedgam3 =0
    observedgam4 =0
    observedgam5 =0
    
    v = stdev*stdev
    theta = v/meanda
    k = (meanda *meanda)/v
    quantgamm <- qgamma(p, k, theta)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantgamm[1])
      {
        observedgam1 = observedgam1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantgamm[1] && dat[i] <=quantgamm[2])
      {
        observedgam2 = observedgam2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantgamm[2] && dat[i] <=quantgamm[3])
      {
        observedgam3 = observedgam3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantgamm[3] && dat[i] <=quantgamm[4])
      {
        observedgam4 = observedgam4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantgamm[4])
      {
        observedgam5 = observedgam5 +1
      }
    }
    
    observedgam <- c(observedgam1, observedgam2, observedgam3, observedgam4, observedgam5, k, theta)
    return(observedgam)
  }
  
  
  
  ################## Chi squared distribution ############
  checkchi <- function(dat, p, meanda, stdev)
  {
    observedchi1 =0
    observedchi2 =0
    observedchi3 =0
    observedchi4 =0
    observedchi5 =0
    
    
    k = meanda
    quantchi <- qchisq(p, k, ncp =0)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantchi[1])
      {
        observedchi1 = observedchi1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantchi[1] && dat[i] <=quantchi[2])
      {
        observedchi2 = observedchi2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantchi[2] && dat[i] <=quantchi[3])
      {
        observedchi3 = observedchi3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantchi[3] && dat[i] <=quantchi[4])
      {
        observedchi4 = observedchi4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantchi[4])
      {
        observedchi5 = observedchi5 +1
      }
    }
    
    observedchi <- c(observedchi1, observedchi2, observedchi3, observedchi4, observedchi5, k)
    return(observedchi)
  }
  
  
  
  ################## Beta distribution  check if x in [0,1] ############
  checkbeta <- function(dat, p, meanda, stdev)
  {
    observedbeta1 =0
    observedbeta2 =0
    observedbeta3 =0
    observedbeta4 =0
    observedbeta5 =0
    
    
    v = stdev*stdev
    b = (meanda*(1-meanda)*(1-meanda)/v) + meanda -1
    a = (b*meanda)/(1-meanda)
    
    if(a <= 0 || b <= 0)
    {
      a = 0.1
      b = 0.1
    }
    
    quantbeta <- qbeta(p, a, b, ncp =0)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantbeta[1])
      {
        observedbeta1 = observedbeta1 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantbeta[1] && dat[i] <=quantbeta[2])
      {
        observedbeta2 = observedbeta2 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantbeta[2] && dat[i] <=quantbeta[3])
      {
        observedbeta3 = observedbeta3 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantbeta[3] && dat[i] <=quantbeta[4])
      {
        observedbeta4 = observedbeta4 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantbeta[4])
      {
        observedbeta5 = observedbeta5 +1
      }
    }
    
    observedbeta <- c(observedbeta1, observedbeta2, observedbeta3, observedbeta4, observedbeta5, a, b)
    return(observedbeta)
  }
  
  
  ################## Weibull distribution k=0.2 ############
  checkweibull1 <- function(dat, p, meanda, stdev)
  {
    observedweibull11 =0
    observedweibull12 =0
    observedweibull13 =0
    observedweibull14 =0
    observedweibull15 =0
    
    
    
    l = meanda/120
    k = 0.2
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull11 = observedweibull11 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull12 = observedweibull12 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull13 = observedweibull13 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull14 = observedweibull14 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull15 = observedweibull15 +1
      }
    }
    
    observedweibull1 <- c(observedweibull11, observedweibull12, observedweibull13, observedweibull14, observedweibull15, k, l)
    return(observedweibull1)
  }
  
  
  ################## Weibull distribution k=0.3 ############
  checkweibull2 <- function(dat, p, meanda, stdev)
  {
    observedweibull21 =0
    observedweibull22 =0
    observedweibull23 =0
    observedweibull24 =0
    observedweibull25 =0
    
    
    
    l = meanda/8.85
    k = 0.3
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull21 = observedweibull21 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull22 = observedweibull22 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull23 = observedweibull23 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull24 = observedweibull24 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull25 = observedweibull25 +1
      }
    }
    
    observedweibull2 <- c(observedweibull21, observedweibull22, observedweibull23, observedweibull24, observedweibull25, k, l)
    return(observedweibull2)
  }
  
  
  
  
  ################## Weibull distribution k=0.4 ############
  checkweibull3 <- function(dat, p, meanda, stdev)
  {
    observedweibull31 =0
    observedweibull32 =0
    observedweibull33 =0
    observedweibull34 =0
    observedweibull35 =0
    
    
    
    l = meanda/3.32
    k= 0.4
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull31 = observedweibull31 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull32 = observedweibull32 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull33 = observedweibull33 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull34 = observedweibull34 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull35 = observedweibull35 +1
      }
    }
    
    observedweibull3 <- c(observedweibull31, observedweibull32, observedweibull33, observedweibull34, observedweibull35, k, l)
    return(observedweibull3)
  }
  
  
  ################## Weibull distribution k=0.5 ############
  checkweibull4 <- function(dat, p, meanda, stdev)
  {
    observedweibull41 =0
    observedweibull42 =0
    observedweibull43 =0
    observedweibull44 =0
    observedweibull45 =0
    
    
    
    l = meanda/2
    k = 0.5
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull41 = observedweibull41 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull42 = observedweibull42 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull43 = observedweibull43 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull44 = observedweibull44 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull45 = observedweibull45 +1
      }
    }
    
    observedweibull4 <- c(observedweibull41, observedweibull42, observedweibull43, observedweibull44, observedweibull45, k, l)
    return(observedweibull4)
  }
  
  
  
  ################## Weibull distribution k=0.6 ############
  checkweibull5 <- function(dat, p, meanda, stdev)
  {
    observedweibull51 =0
    observedweibull52 =0
    observedweibull53 =0
    observedweibull54 =0
    observedweibull55 =0
    
    
    
    l = meanda/1.42
    k = 0.6
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull51 = observedweibull51 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull52 = observedweibull52 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull53 = observedweibull53 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull54 = observedweibull54 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull55 = observedweibull55 +1
      }
    }
    
    observedweibull5 <- c(observedweibull51, observedweibull52, observedweibull53, observedweibull54, observedweibull55, k, l)
    return(observedweibull5)
  }
  
  
  
  ################## Weibull distribution k=0.8 ############
  checkweibull6 <- function(dat, p, meanda, stdev)
  {
    observedweibull61 =0
    observedweibull62 =0
    observedweibull63 =0
    observedweibull64 =0
    observedweibull65 =0
    
    
    
    l = meanda/1.1
    k = 0.8
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull61 = observedweibull61 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull62 = observedweibull62 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull63 = observedweibull63 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull64 = observedweibull64 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull65 = observedweibull65 +1
      }
    }
    
    observedweibull6 <- c(observedweibull61, observedweibull62, observedweibull63, observedweibull64, observedweibull65, k, l)
    return(observedweibull6)
  }
  
  
  ################## Weibull distribution k=1.2 ############
  checkweibull7 <- function(dat, p, meanda, stdev)
  {
    observedweibull71 =0
    observedweibull72 =0
    observedweibull73 =0
    observedweibull74 =0
    observedweibull75 =0
    
    
    
    l = meanda/0.93
    k = 1.2
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull71 = observedweibull71 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull72 = observedweibull72 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull73 = observedweibull73 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull74 = observedweibull74 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull75 = observedweibull75 +1
      }
    }
    
    observedweibull7 <- c(observedweibull71, observedweibull72, observedweibull73, observedweibull74, observedweibull75, k, l)
    return(observedweibull7)
  }
  
  ################## Weibull distribution k=1.5 ############
  checkweibull8 <- function(dat, p, meanda, stdev)
  {
    observedweibull81 =0
    observedweibull82 =0
    observedweibull83 =0
    observedweibull84 =0
    observedweibull85 =0
    
    
    
    l = meanda/0.89
    k = 1.5
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull81 = observedweibull81 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull82 = observedweibull82 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull83 = observedweibull83 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull84 = observedweibull84 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull85 = observedweibull85 +1
      }
    }
    
    observedweibull8 <- c(observedweibull81, observedweibull82, observedweibull83, observedweibull84, observedweibull85, k, l)
    return(observedweibull8)
  }
  
  
  ################## Weibull distribution k=2 ############
  checkweibull9 <- function(dat, p, meanda, stdev)
  {
    observedweibull91 =0
    observedweibull92 =0
    observedweibull93 =0
    observedweibull94 =0
    observedweibull95 =0
    
    
    
    l = meanda/0.88
    k = 2
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull91 = observedweibull91 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull92 = observedweibull92 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull93 = observedweibull93 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull94 = observedweibull94 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull95 = observedweibull95 +1
      }
    }
    
    observedweibull9 <- c(observedweibull91, observedweibull92, observedweibull93, observedweibull94, observedweibull95, k, l)
    return(observedweibull9)
  }
  
  
  
  
  ################## Weibull distribution k=4 ############
  checkweibull10 <- function(dat, p, meanda, stdev)
  {
    observedweibull101 =0
    observedweibull102 =0
    observedweibull103 =0
    observedweibull104 =0
    observedweibull105 =0
    
    
    
    l = meanda/0.9
    k = 4
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull101 = observedweibull101 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull102 = observedweibull102 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull103 = observedweibull103 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull104 = observedweibull104 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull105 = observedweibull105 +1
      }
    }
    
    observedweibull10 <- c(observedweibull101, observedweibull102, observedweibull103, observedweibull104, observedweibull105, k, l)
    return(observedweibull10)
  }
  
  
  
  ################## Weibull distribution k=6 ############
  checkweibull11 <- function(dat, p, meanda, stdev)
  {
    observedweibull111 =0
    observedweibull112 =0
    observedweibull113 =0
    observedweibull114 =0
    observedweibull115 =0
    
    
    
    l = meanda/0.93
    k = 6
    
    quantweibull <- qweibull(p, k, l)
    
    for(i in 1:n)
    {
      if(dat[i] <= quantweibull[1])
      {
        observedweibull111 = observedweibull111 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[1] && dat[i] <=quantweibull[2])
      {
        observedweibull112 = observedweibull112 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[2] && dat[i] <=quantweibull[3])
      {
        observedweibull113 = observedweibull113 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[3] && dat[i] <=quantweibull[4])
      {
        observedweibull114 = observedweibull114 +1
      }
    }
    
    for(i in 1:n)
    {
      if(dat[i] > quantweibull[4])
      {
        observedweibull115 = observedweibull115 +1
      }
    }
    
    observedweibull11 <- c(observedweibull111, observedweibull112, observedweibull113, observedweibull114, observedweibull115, k, l)
    return(observedweibull11)
  }
  
  
  ################   Test statistic & Pval unif ################################
  observedunif <- checkunif(dat,p, meanda, stdev)
  cunif = ((observedunif[1] - expectedcell1)^2/expectedcell1 + (observedunif[2] - expectedcell2)^2/expectedcell2
           + (observedunif[3] - expectedcell3)^2/expectedcell3 + (observedunif[4] - expectedcell4)^2/expectedcell4
           + (observedunif[5] - expectedcell5)^2/expectedcell5)
  
  pvalunif = 1- pchisq(cunif, df =  4)
  minval = observedunif[6]
  maxval = observedunif[7]
  stringunif <- paste("Uniform(min=", minval, ",max =",  maxval, ")")
  
  
  ################   Test statistic & Pval Poisson ################################
  
  observedpois <- checkpois(dat,p, meanda, stdev)
  cpois = ((observedpois[1] - expectedcell1)^2/expectedcell1 + (observedpois[2] - expectedcell2)^2/expectedcell2
           + (observedpois[3] - expectedcell3)^2/expectedcell3 + (observedpois[4] - expectedcell4)^2/expectedcell4
           + (observedpois[5] - expectedcell5)^2/expectedcell5)
  
  pvalpois = 1- pchisq(cpois, df =  4)
  lambdap = observedpois[6]
  stringpois <- paste("Poisson(Lambda=", lambdap,")")
  
  
  ################   Test statistic & Pval Exponential ################################
  observedexp <- checkexp(dat,p, meanda, stdev)
  cexp = ((observedexp[1] - expectedcell1)^2/expectedcell1 + (observedexp[2] - expectedcell2)^2/expectedcell2
          + (observedexp[3] - expectedcell3)^2/expectedcell3 + (observedexp[4] - expectedcell4)^2/expectedcell4
          + (observedexp[5] - expectedcell5)^2/expectedcell5)
  
  pvalexp = 1- pchisq(cexp, df =  4)
  lambdaexp = observedexp[6]
  stringexp <- paste("Exponential(Lambda=", lambdaexp,")")
  
  
  ################   Test statistic & Pval Normal ################################
  observednorm <- checknorm(dat,p, meanda, stdev)
  cnorm = ((observednorm[1] - expectedcell1)^2/expectedcell1 + (observednorm[2] - expectedcell2)^2/expectedcell2
           + (observednorm[3] - expectedcell3)^2/expectedcell3 + (observednorm[4] - expectedcell4)^2/expectedcell4
           + (observednorm[5] - expectedcell5)^2/expectedcell5)
  
  pvalnorm = 1- pchisq(cnorm, df =  3)
  mu_n = observednorm[6]
  vari_n = observednorm[7]
  stringnormal <- paste("Normal(Mean=", mu_n, ",Variance =",  vari_n, ")")
  
  
  
  ################   Test statistic & Pval Gamma ################################
  
  observedgam <- checkgamma(dat,p, meanda, stdev)
  cgam = ((observedgam[1] - expectedcell1)^2/expectedcell1 + (observedgam[2] - expectedcell2)^2/expectedcell2
          + (observedgam[3] - expectedcell3)^2/expectedcell3 + (observedgam[4] - expectedcell4)^2/expectedcell4
          + (observedgam[5] - expectedcell5)^2/expectedcell5)
  
  pvalgam = 1- pchisq(cgam, df =  3)
  k = observedgam[6]
  theta = observedgam[7]
  stringgamma <- paste("Gamma(shape=", k, ",scale =",  theta, ")")
  
  
  
  
  
  ################   Test statistic & Pval Chi ################################
  observedchi <- checkchi(dat,p, meanda, stdev)
  cchi = ((observedchi[1] - expectedcell1)^2/expectedcell1 + (observedchi[2] - expectedcell2)^2/expectedcell2
          + (observedchi[3] - expectedcell3)^2/expectedcell3 + (observedchi[4] - expectedcell4)^2/expectedcell4
          + (observedchi[5] - expectedcell5)^2/expectedcell5)
  
  pvalchi = 1- pchisq(cchi, df =  4)
  k = observedchi[6]
  stringchi <- paste("Chi-squared(degree of freedom=", k, ")")
  
  
  
  ################   Test statistic & Pval Beta ################################
  if(tbeta == TRUE)
  {
  observedbeta <- checkbeta(dat,p, meanda, stdev)
  
  cbeta = ((observedbeta[1] - expectedcell1)^2/expectedcell1 + (observedbeta[2] - expectedcell2)^2/expectedcell2
           + (observedbeta[3] - expectedcell3)^2/expectedcell3 + (observedbeta[4] - expectedcell4)^2/expectedcell4
           + (observedbeta[5] - expectedcell5)^2/expectedcell5)
  
  pvalbeta = 1- pchisq(cbeta, df =  3)
  alpha = observedbeta[6]
  beta = observedbeta[7]
  stringbeta <- paste("Beta(alpha=", alpha, ",beta =",  beta, ")")
  }
  
  
  ################   Test statistic & Pval Weibull1 ################################
  observedweibull1 <- checkweibull1(dat,p, meanda, stdev)
  cweibull1 = ((observedweibull1[1] - expectedcell1)^2/expectedcell1 + (observedweibull1[2] - expectedcell2)^2/expectedcell2
               + (observedweibull1[3] - expectedcell3)^2/expectedcell3 + (observedweibull1[4] - expectedcell4)^2/expectedcell4
               + (observedweibull1[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull1 = 1- pchisq(cweibull1, df =  4)
  shape1 = observedweibull1[6]
  scale1 = observedweibull1[7]
  stringweibull1 <- paste("Weibull(shape=", shape1, ",scale =",  scale1, ")")
  
  
  ################   Test statistic & Pval Weibull2 ################################
  observedweibull2 <- checkweibull2(dat,p, meanda, stdev)
  cweibull2 = ((observedweibull2[1] - expectedcell1)^2/expectedcell1 + (observedweibull2[2] - expectedcell2)^2/expectedcell2
               + (observedweibull2[3] - expectedcell3)^2/expectedcell3 + (observedweibull2[4] - expectedcell4)^2/expectedcell4
               + (observedweibull2[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull2 = 1- pchisq(cweibull2, df =  4)
  shape2 = observedweibull2[6]
  scale2 = observedweibull2[7]
  stringweibull2 <- paste("Weibull(shape=", shape2, ",scale =",  scale2, ")")
  
  
  ################   Test statistic & Pval Weibull3 ################################
  observedweibull3 <- checkweibull3(dat,p, meanda, stdev)
  cweibull3 = ((observedweibull3[1] - expectedcell1)^2/expectedcell1 + (observedweibull3[2] - expectedcell2)^2/expectedcell2
               + (observedweibull3[3] - expectedcell3)^2/expectedcell3 + (observedweibull3[4] - expectedcell4)^2/expectedcell4
               + (observedweibull3[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull3 = 1- pchisq(cweibull3, df =  4)
  shape3 = observedweibull3[6]
  scale3 = observedweibull3[7]
  stringweibull3 <- paste("Weibull(shape=", shape3, ",scale =",  scale3, ")")
  
  ################   Test statistic & Pval Weibull4 ################################
  observedweibull4 <- checkweibull4(dat,p, meanda, stdev)
  cweibull4 = ((observedweibull4[1] - expectedcell1)^2/expectedcell1 + (observedweibull4[2] - expectedcell2)^2/expectedcell2
               + (observedweibull4[3] - expectedcell3)^2/expectedcell3 + (observedweibull4[4] - expectedcell4)^2/expectedcell4
               + (observedweibull4[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull4 = 1- pchisq(cweibull4, df =  4)
  shape4 = observedweibull4[6]
  scale4 = observedweibull4[7]
  stringweibull4 <- paste("Weibull(shape=", shape4, ",scale =",  scale4, ")")
  
  
  ################   Test statistic & Pval Weibull5 ################################
  observedweibull5 <- checkweibull5(dat,p, meanda, stdev)
  cweibull5 = ((observedweibull5[1] - expectedcell1)^2/expectedcell1 + (observedweibull5[2] - expectedcell2)^2/expectedcell2
               + (observedweibull5[3] - expectedcell3)^2/expectedcell3 + (observedweibull5[4] - expectedcell4)^2/expectedcell4
               + (observedweibull5[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull5 = 1- pchisq(cweibull5, df =  4)
  shape5 = observedweibull5[6]
  scale5 = observedweibull5[7]
  stringweibull5 <- paste("Weibull(shape=", shape5, ",scale =",  scale5, ")")
  
  
  ################   Test statistic & Pval Weibull6 ################################
  observedweibull6 <- checkweibull6(dat,p, meanda, stdev)
  cweibull6 = ((observedweibull6[1] - expectedcell1)^2/expectedcell1 + (observedweibull6[2] - expectedcell2)^2/expectedcell2
               + (observedweibull6[3] - expectedcell3)^2/expectedcell3 + (observedweibull6[4] - expectedcell4)^2/expectedcell4
               + (observedweibull6[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull6 = 1- pchisq(cweibull6, df =  4)
  shape6 = observedweibull6[6]
  scale6 = observedweibull6[7]
  stringweibull6 <- paste("Weibull(shape=", shape6, ",scale =",  scale6, ")")
  
  
  ################   Test statistic & Pval Weibull7 ################################
  observedweibull7 <- checkweibull7(dat,p, meanda, stdev)
  cweibull7 = ((observedweibull7[1] - expectedcell1)^2/expectedcell1 + (observedweibull7[2] - expectedcell2)^2/expectedcell2
               + (observedweibull7[3] - expectedcell3)^2/expectedcell3 + (observedweibull7[4] - expectedcell4)^2/expectedcell4
               + (observedweibull7[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull7 = 1- pchisq(cweibull7, df =  4)
  shape7 = observedweibull7[6]
  scale7 = observedweibull7[7]
  stringweibull7 <- paste("Weibull(shape=", shape7, ",scale =",  scale7, ")")
  
  
  ################   Test statistic & Pval Weibull8 ################################
  observedweibull8 <- checkweibull8(dat,p, meanda, stdev)
  cweibull8 = ((observedweibull8[1] - expectedcell1)^2/expectedcell1 + (observedweibull8[2] - expectedcell2)^2/expectedcell2
               + (observedweibull8[3] - expectedcell3)^2/expectedcell3 + (observedweibull8[4] - expectedcell4)^2/expectedcell4
               + (observedweibull8[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull8 = 1- pchisq(cweibull8, df =  4)
  shape8 = observedweibull8[6]
  scale8 = observedweibull8[7]
  stringweibull8 <- paste("Weibull(shape=", shape8, ",scale =",  scale8, ")")
  
  
  ################   Test statistic & Pval Weibull9 ################################
  observedweibull9 <- checkweibull9(dat,p, meanda, stdev)
  cweibull9 = ((observedweibull9[1] - expectedcell1)^2/expectedcell1 + (observedweibull9[2] - expectedcell2)^2/expectedcell2
               + (observedweibull9[3] - expectedcell3)^2/expectedcell3 + (observedweibull9[4] - expectedcell4)^2/expectedcell4
               + (observedweibull9[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull9 = 1- pchisq(cweibull9, df =  4)
  shape9 = observedweibull9[6]
  scale9 = observedweibull9[7]
  stringweibull9 <- paste("Weibull(shape=", shape9, ",scale =",  scale9, ")")
  
  
  ################   Test statistic & Pval Weibull10 ################################
  observedweibull10 <- checkweibull10(dat,p, meanda, stdev)
  cweibull10 = ((observedweibull10[1] - expectedcell1)^2/expectedcell1 + (observedweibull10[2] - expectedcell2)^2/expectedcell2
                + (observedweibull10[3] - expectedcell3)^2/expectedcell3 + (observedweibull10[4] - expectedcell4)^2/expectedcell4
                + (observedweibull10[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull10 = 1- pchisq(cweibull10, df =  4)
  shape10 = observedweibull10[6]
  scale10 = observedweibull10[7]
  stringweibull10 <- paste("Weibull(shape=", shape10, ",scale =",  scale10, ")")
  
  
  ################   Test statistic & Pval Weibull11 ################################
  observedweibull11 <- checkweibull11(dat,p, meanda, stdev)
  cweibull11 = ((observedweibull11[1] - expectedcell1)^2/expectedcell1 + (observedweibull11[2] - expectedcell2)^2/expectedcell2
                + (observedweibull11[3] - expectedcell3)^2/expectedcell3 + (observedweibull11[4] - expectedcell4)^2/expectedcell4
                + (observedweibull11[5] - expectedcell5)^2/expectedcell5)
  
  pvalweibull11 = 1- pchisq(cweibull11, df =  4)
  shape11 = observedweibull11[6]
  scale11 = observedweibull11[7]
  stringweibull11 <- paste("Weibull(shape=", shape11, ",scale =",  scale11, ")")
  
  
  
  ########################################################################
  ##########  Create the RESULT data structure ###########################
  
  Distrname = c(stringunif, stringpois, stringexp, stringnormal, stringgamma, stringchi,
                stringbeta, stringweibull1, stringweibull2, stringweibull3, stringweibull4,
                stringweibull5, stringweibull6, stringweibull7, stringweibull8, stringweibull9,
                stringweibull10, stringweibull11)
  
  Probability = c(pvalunif, pvalpois, pvalexp, pvalnorm, pvalgam, pvalchi,
                  pvalbeta, pvalweibull1, pvalweibull2, pvalweibull3, pvalweibull4,
                  pvalweibull5, pvalweibull6, pvalweibull7, pvalweibull8, pvalweibull9,
                  pvalweibull10, pvalweibull11)
  
  
  ######### Domain checks ###########
  if(tp != TRUE || ti != TRUE)
  {
    Probability[2] = 0 ##Poisson
  }
  
  if(tp != TRUE)
  {
    Probability[3] = 0  ### Exponential
    Probability[5] = 0  #### Gamma
    Probability[6] = 0  ### Chi
    for(i in 8:18)
    {
      Probability[i] = 0  ### Weibulls
    }
  }
  
  
  if(tbeta != TRUE)
  {
    Probability[7] = 0
  }
  
  
  ######## Result is a data frame #######
  Result <- data.frame(Distrname, Probability)
  
  Result_sorted <- Result[order(-Probability),]
  
  print(Result_sorted)
  
  
}
