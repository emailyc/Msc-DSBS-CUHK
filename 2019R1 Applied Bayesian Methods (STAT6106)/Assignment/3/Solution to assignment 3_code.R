######################################################################
# Question 1 (20 points)
######################################################################
# Pseudo-Random-Number-Generator
# out[1] = (a * seed)%%m
# out[i + 1] = (a * out[i])%%m
RNG = function(B = 100, seed = 0, a = 65539, m = 2^31) {
  out = rep(0, B)
  if (seed == 0)
    seed = as.numeric(format(Sys.time(), "%s")) #Using the computer's clock to set seed
  out[1] = (a * seed)%%m
  for (i in 1:(B - 1)) {
    out[i + 1] = (a * out[i])%%m
  }
  return(out/m) #standardize it to the range of 0~1
}
RNG(seed = 12345)
randu3k=matrix(RNG(seed=12345,B=3000),ncol=3,byrow=TRUE)
library(rgl)
plot3d(randu3k)


######################################################################
# Question 2 (20 points)
######################################################################
# IMSL
RNG_IMSL = function(B = 100, seed = 0, a = 16807, m = 2^31-1) {
  out = rep(0, B)
  if (seed == 0)
    seed = as.numeric(format(Sys.time(), "%s")) #Using the computer's clock to set seed
  out[1] = (a * seed)%%m
  for (i in 1:(B - 1)) {
    out[i + 1] = (a * out[i])%%m
  }
  return(out/m) #standardize it to the range of 0~1
}

# SIMSCRIPT
RNG_SIMSCRIPT = function(B = 100, seed = 0, a = 6303600167, m = 2^31-1) {
  out = rep(0, B)
  if (seed == 0)
    seed = as.numeric(format(Sys.time(), "%s")) #Using the computer's clock to set seed
  out[1] = (a * seed)%%m
  for (i in 1:(B - 1)) {
    out[i + 1] = (a * out[i])%%m
  }
  return(out/m) #standardize it to the range of 0~1
}

x <- RNG_IMSL(seed=1,B=1000)
y <- RNG_SIMSCRIPT(seed=1,B=1000)

# Correlation values
cor(x, y)

# Scatterplot
plot(x,y,xlab='IMSL',ylab='SIMSCRIPT')

######################################################################
# Question 3 (30 points)
######################################################################
##############method 1: use the under area method
n=100 # 1000, 10000
pmax=exp(5^(0.1))
x=runif(n,0,5)
y=runif(n,0,pmax)
px=exp(x^(0.1))
pmax*5*sum(y<=px)/n

##############method 2: sample x in uniform distribution (0,5)
n=100 # 1000, 10000
x=runif(n,0,5)
mean(5*exp(x^(0.1)))

######################################################################
# Question 4 (30 points)
######################################################################
############method 1: sample x from a log-norm disribution
set.seed(0)
g <- function(x){abs(sin(x))*exp(-x)}
N <- 100000  #the sample size of each estimate
estimate <- c()
data <- c()
for(i in 1:10){
  X <- rlnorm(N)
  Y <- g(X)/dlnorm(X)
  estimate[i] <- mean(Y)
  data <- cbind(data,Y)
}

############method 1: sample x from an exponentioal distribution, f(x)=e^(-x), thus, X~Exp(1)
set.seed(0)
N <- 100000  #the sample size of each estimate
estimate <- c()
data <- c()
for(i in 1:10){
  X <- rexp(N)
  Y <- abs(sin(X))
  estimate[i] <- mean(Y)
  data <- cbind(data,Y)
}

mean(estimate)
sd(estimate)