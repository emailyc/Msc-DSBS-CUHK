#the function of R_hat to judge the convergence
Rhat <- function(data){
  n <- dim(data)[1]
  r <- dim(data)[2]
  pj <- apply(data,2,mean)
  sj <- apply(data,2,var)
  pjk <- mean(pj)
  B <- sum((pj-pjk)^2)*n/(r-1)
  W <- mean(sj)
  R <- ((1-1/n)*W+B/n)/W
  return(R)
}

#Q1
#method 1
set.seed(0)
g <- function(x){abs(cos(x))/x*exp(-(log(x)-3)^2)}

N <- 100000  #the sample size of each estimate
estimate <- c()
data <- c()
for(i in 1:10){
  X <- rlnorm(N)
  Y <- g(X)/dlnorm(X)
  estimate[i] <- mean(Y)
  data <- cbind(data,Y)
}

mean(estimate)
sd(estimate)

#method 2
set.seed(0)
N <- 1000  #the sample size of each estimate
estimate <- c()
data <- c()
lsd= sqrt(0.5)
a=sqrt(pi)
for(i in 1:10){
  X <- rlnorm(N,3,lsd)
  Y <- abs(cos(X))
  estimate[i] <- mean(Y)
  data <- cbind(data,Y)
}
estimate= a* estimate


estimate
mean(estimate)
sd(estimate)


#Q2

#method 1
set.seed(0)

r.TruncatedNorm <- function(mu, sigma,lower,upper){
  temp <- rnorm(1,mu,sigma)
  while((temp>=upper)|(temp<=lower)){temp <- rnorm(1,mu,sigma)}
  return(temp)
}

data <- c(1, 2, 2.5, 3, 3.5, 4, 5, 7.0, 7.2, 7.3, 7.35, 7.4, 7.45, 7.5, 7.6, 7.9)

d.target <- function(a,mu,theta,sigma2,tau2){prod(a*dnorm(data,mu,sqrt(sigma2))+(1-a)*dnorm(data,theta,sqrt(tau2)))*1/sigma2*1/tau2}

N <- 5000 #sample size
M <- 5 #number of chains
A <- c()
Mu <- c()
Theta <- c()
Sigma2 <- c()
Tau2 <- c()

for(m in 1:M){
  a <- rep(NA,N);a[1] <- 0.5
  mu <- rep(NA,N);mu[1] <- 0
  theta <- rep(NA,N);theta[1] <- 0
  sigma2 <- rep(NA,N);sigma2[1] <- 1
  tau2 <- rep(NA,N);tau2[1] <- 1
  sigma <- 1 #the sigma of the proposal norm distribution
  for(i in 2:N){
    a.try <- runif(1)
    r <- d.target(a.try,mu[i-1],theta[i-1],sigma2[i-1],tau2[i-1])/d.target(a[i-1],mu[i-1],theta[i-1],sigma2[i-1],tau2[i-1])
    r <- min(r,1)
    u=runif(1,0,1)
    if(u<r){
      a[i] <- a.try
    }else{
      a[i] <- a[i-1]
    }
    
    mu.try <- r.TruncatedNorm(mu[i-1],sigma,-Inf,6)
    r <- d.target(a[i],mu.try,theta[i-1],sigma2[i-1],tau2[i-1])/d.target(a[i],mu[i-1],theta[i-1],sigma2[i-1],tau2[i-1])
    r <- min(r,1)
    u=runif(1,0,1)
    if(u<r){
      mu[i] <- mu.try
    }else{
      mu[i] <- mu[i-1]
    }
    
    theta.try <- rnorm(1,theta[i-1],sigma)
    r <- d.target(a[i],mu[i],theta.try,sigma2[i-1],tau2[i-1])/d.target(a[i],mu[i],theta[i-1],sigma2[i-1],tau2[i-1])
    r <- min(r,1)
    u=runif(1,0,1)
    if(u<r){
      theta[i] <- theta.try
    }else{
      theta[i] <- theta[i-1]
    }
    
    sigma2.try <- rlnorm(1,log(sigma2[i-1])-sigma^2/2,sigma)
    r <- d.target(a[i],mu[i],theta[i],sigma2.try,tau2[i-1])/d.target(a[i],mu[i],theta[i],sigma2[i-1],tau2[i-1])*dlnorm(sigma2[i-1],log(sigma2.try)-sigma^2/2,sigma)/dlnorm(sigma2.try,log(sigma2[i-1])-sigma^2/2,sigma)
    r <- min(r,1)
    u=runif(1,0,1)
    if(u<r){
      sigma2[i] <- sigma2.try
    }else{
      sigma2[i] <- sigma2[i-1]
    }
    
    tau2.try <- rlnorm(1,log(tau2[i-1])-sigma^2/2,sigma)
    r <- d.target(a[i],mu[i],theta[i],sigma2[i],tau2.try)/d.target(a[i],mu[i],theta[i],sigma2[i],tau2[i-1])*dlnorm(tau2[i-1],log(tau2.try)-sigma^2/2,sigma)/dlnorm(tau2.try,log(tau2[i-1])-sigma^2/2,sigma)
    r <- min(r,1)
    u=runif(1,0,1)
    if(u<r){
      tau2[i] <- tau2.try
    }else{
      tau2[i] <- tau2[i-1]
    }
  }
  
  A <- cbind(A,a)
  Mu <- cbind(Mu,mu)
  Theta <- cbind(Theta,theta)
  Sigma2 <- cbind(Sigma2,sigma2)
  Tau2 <- cbind(Tau2,tau2)
}


par(mfrow=c(2,3))
plot(ts(a))
plot(ts(mu))
plot(ts(theta))
plot(ts(sigma2))
plot(ts(tau2))

Rhat(A[-(1:(N/2)),])
Rhat(Mu[-(1:(N/2)),])
Rhat(Theta[-(1:(N/2)),])
Rhat(Sigma2[-(1:(N/2)),])
Rhat(Tau2[-(1:(N/2)),])


#method2
set.seed(0)
N <- 5000 #sample size
n <- length(data) #data size
M <- 5 #number of chains
A <- c()
Mu <- c()
Theta <- c()
Sigma2 <- c()
Tau2 <- c()

for(m in 1:M){
  a <- rep(NA,N);a[1] <- 0.5
  mu <- rep(NA,N);mu[1] <- 0
  theta <- rep(NA,N);theta[1] <- 0
  sigma2 <- rep(NA,N);sigma2[1] <- 1
  tau2 <- rep(NA,N);tau2[1] <- 1
  z <- matrix(NA,nr=N,nc=n);z[1,] <- rbinom(n,1,0.5)
  
  for(i in 2:N){
    a[i] <- rbeta(1,sum(z[i-1,])+1,sum((1-z[i-1,]))+1)
    mu[i] <- rnorm(1,5*sum(z[i-1,]*data)/(5*sum(z[i-1,])+sigma2[i-1]),sqrt(5*sigma2[i-1]/(5*sum(z[i-1,])+sigma2[i-1])))
    theta[i] <- rnorm(1,5*sum((1-z[i-1,])*data)/(5*sum((1-z[i-1,]))+tau2[i-1]),sqrt(5*tau2[i-1]/(5*sum((1-z[i-1,]))+tau2[i-1])))
    sigma2[i] <- 1/rgamma(1,sum(z[i-1,])/2+1,sum(z[i-1,]*(data-mu[i])^2)/2+1)
    tau2[i] <- 1/rgamma(1,sum((1-z[i-1,]))/2+1,sum((1-z[i-1,])*(data-theta[i])^2)/2+1)
    
    if(mu[i]>=6){
      a[i] <- 1-a[i]
      temp <- mu[i]
      mu[i] <- theta[i]
      theta[i] <- temp
      temp <- sigma2[i]
      sigma2[i] <- tau2[i]
      tau2[i] <- temp
    }
    
    a1 <- a[i]/(sigma2[i])^(5/2)*exp(-(data-mu[i])^2/2/sigma2[i])
    a2 <- (1-a[i])/(tau2[i])^(5/2)*exp(-(data-theta[i])^2/2/tau2[i])
    z[i,] <- rbinom(rep(1,n),rep(1,n),a1/(a1+a2))
  }
  
  A <- cbind(A,a)
  Mu <- cbind(Mu,mu)
  Theta <- cbind(Theta,theta)
  Sigma2 <- cbind(Sigma2,sigma2)
  Tau2 <- cbind(Tau2,tau2)
}

Rhat(A[-(1:(N/2)),])
Rhat(Mu[-(1:(N/2)),])
Rhat(Theta[-(1:(N/2)),])
Rhat(Sigma2[-(1:(N/2)),])
Rhat(Tau2[-(1:(N/2)),])

par(mfrow=c(2,3))
plot(ts(a))
plot(ts(mu))
plot(ts(theta))
plot(ts(sigma2))
plot(ts(tau2))

#d
sum((theta>mu)[-(1:(N/2))])/(N/2)

#e
mean(a[(theta>mu)[-(1:(N/2))]])