# improved nnet()
# Try nnet(x,y) k times and output the best trial
# x is the matrix of input variable
# y is the dependent value; y must be factor if linout=F is used
# The default option of trace is set to F

library(nnet)
ann<-function(x,y,size,maxit=100,linout=F,trace=F,try=5) {
  ann1<-nnet::nnet(y~.,data=x,size=size,maxit=maxit,linout=linout,trace=trace)
  v1<-ann1$value

  for (i in 2:try) {
    ann<-nnet::nnet(y~.,data=x,size=size,maxit=maxit,linout=linout,trace=trace)
    if (ann$value<v1) {
      v1<-ann$value
      ann1<-ann
    }
  }
  ann1
}  


