#################################################################################################
#################################################################################################
#####   This R-code serves to compute a Bayes factor for Kendall's tau, as described in     #####
#####   van Doorn, J.B., Ly, A., Marsman, M. & Wagenmakers, E.-J. (in press). Bayesian      #####
#####   Inference for Kendall’s Rank Correlation Coefficient. The American Statistician.    #####
#################################################################################################
#####   To use it, input your values below for yourKendallTauValue and yourN and run the    #####
#####   whole script. The function call at the bottom will use your values and will compute #####
#####   and print the Bayes factor in the console. The last line will plot the posterior    #####
#####   distribution. This analysis is also available in JASP (www.jasp-stats.org), an      #####
#####   open-source statistical software for Bayesian statistics with a graphical user      #####
#####   interface.                                                                          #####
#################################################################################################
#################################################################################################

# yourKendallTauValue <- 0                      # Input your obtained value for kendall's tau here
# yourN <- 100                                  # Input your sample size here, then run the whole script


# Prior specification Kendall's Tau
scaledBetaTau <- function(tau, alpha=1, beta=1){
  result <-   ((pi*2^(-2*alpha))/beta(alpha,alpha))  * cos((pi*tau)/2)^(2*alpha-1)
  return(result)
}

priorTau <- function(tau, kappa){
  scaledBetaTau(tau, alpha = (1/kappa), beta = (1/kappa))
}

priorTauPlus <- function(tau, kappa=1) {
  non.negative.index <- tau >=0
  less.than.one.index <- tau <=1
  value.index <- as.logical(non.negative.index*less.than.one.index)
  result <- tau*0
  result[value.index] <- 2*priorTau(tau[value.index], kappa)
  return(result)
}

priorTauMin <- function(tau, kappa=1) {
  negative.index <- tau <=0
  greater.than.min.one.index <- tau >= -1
  value.index <- as.logical(negative.index*greater.than.min.one.index)
  result <- tau*0
  result[value.index] <- 2*priorTau(tau[value.index], kappa)
  return(result)
}


# Posterior specification Kendall's Tau
postDensKendallTau <- function(delta,Tstar,n,kappa=1,var=var,test="two-sided"){ 
  if(test == "two-sided"){priorDens <- priorTau(delta,kappa)
  } else if(test == "positive"){priorDens <- priorTauPlus(delta,kappa)
  } else if(test == "negative"){priorDens <- priorTauMin(delta,kappa)}
  priorDens <- priorTau(delta,kappa)
  dens <- dnorm(Tstar,(1.5*delta*sqrt(n)),sd=sqrt(var))* priorDens
  return(dens)
}
posteriorTau <- function(delta,kentau,n,kappa=1,var=1,test="two-sided"){
  Tstar <- (kentau * ((n*(n-1))/2))/sqrt(n*(n-1)*(2*n+5)/18)
  var <- min(1,var)
  if(test == "two-sided"){lims <- c(-1,1)
  } else if(test == "positive"){lims <- c(0,1)
  } else if(test == "negative"){lims <- c(-1,0)}
  logicalCensor <- (delta >= lims[1] & delta <= lims[2])
  dens <- logicalCensor*postDensKendallTau(delta,Tstar,n,kappa,var,test=test)/
    integrate(function(delta){postDensKendallTau(delta,Tstar,n,kappa,var,test=test)},lims[1],lims[2])$value
} 

# Bayes factor computation Kendall's Tau
bfCorrieKernelKendallTau <- function(tau, n, kappa=1, var=1, ciValue=0.95){ 
  tempList <- list(vector())
  output <- list(n=n, r=tau, bf10=NA, bfPlus0=NA, bfMin0=NA)
  output$bf10 <- priorTau(0,kappa)/posteriorTau(0,tau,n,kappa=kappa,var=var,test="two-sided")
  output$bfPlus0 <- priorTauPlus(0,kappa)/posteriorTau(0,tau,n,kappa=kappa,var=var,test="positive")
  output$bfMin0 <- priorTauMin(0,kappa)/posteriorTau(0,tau,n,kappa=kappa,var=var,test="negative")
  return(output)
}

# Compute credible intervals kendalls tau
credibleIntervalKendallTau <- function(kentau,n,kappa=1,var=1, test="two-sided", ciValue = 0.95){
  nSeqs <- 1000
  lowCI <- (1-ciValue)/2
  upCI <- (1+ciValue)/2
  taus <- seq(-1,1,length.out = (nSeqs-1))
  densVals <- posteriorTau(taus, kentau, n, kappa = kappa, var = var, test = test)
  densVals <- cumsum((densVals[1:(nSeqs-1)]+densVals[2:nSeqs])*0.5*(taus[2]-taus[1]))
  lowerCI <- taus[which(densVals>=lowCI)[1]]
  upperCI <- taus[which(densVals>=upCI)[1]]
  median <- taus[which(densVals>=0.5)[1]]
  return(list(lowerCI = lowerCI, median = median, upperCI = upperCI))
}

sampleTausA <- function(myTau,myN,nSamples = 3e3, var = 1){
  nSeqs <- 1000
  tauSamples <- NULL
  taus <- seq(-1,1,length.out = nSeqs)
  densVals <- posteriorTau(taus, myTau, myN, var = var)
  ceiling <- max(densVals)
  lowerB <- taus[which(round(densVals,digits=6) != 0 )][1]
  upperB <- rev(taus[which(round(densVals,digits=6) != 0 )])[1]
  
  while(length(tauSamples) < nSamples){
    prop <- runif(1,lowerB,upperB)
    propDens <- posteriorTau(prop, myTau, myN, var = var)
    if(propDens > runif(1,0,ceiling)){tauSamples <- c(tauSamples,prop)}
  }
  return(tauSamples)
}



# bfCorrieKernelKendallTau(tau = yourKendallTauValue, n = yourN)  # This function call will carry out the computations
# and returns a list with the Bayes factors (regular, plussided, minsided)

# The following code plots a (simple) posterior distribution of your results.
# plot(density(sampleTausA(myTau = yourKendallTauValue, myN = yourN),from = -1, to = 1), las = 1, bty = "n", lwd=3, 
#      main = "Posterior Distribution for Kendall's tau", xlab = expression(tau))