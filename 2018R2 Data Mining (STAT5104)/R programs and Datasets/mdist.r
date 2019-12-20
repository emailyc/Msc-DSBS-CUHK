# input data matrix x, assume no missing values
# output mahalanobis distance of x

mdist<-function(x) {
   t<-as.matrix(x)
   m<-apply(t,2,mean)
   s<-var(t)
   mahalanobis(t,m,s)
}
