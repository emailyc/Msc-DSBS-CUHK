# function for standardize transformation
stand<-function(x) {
   n<-dim(x)[1]		# row dim of x
   p<-dim(x)[2]		# column dim of x
   m<-apply(x,2,mean)	# compute column mean
   s<-apply(x,2,sd)	# compute column sd
   m<-matrix(m,nr=n,nc=p,byrow=T)  # convert m into nxp matrix, each row is m
   s<-matrix(s,nr=n,nc=p,byrow=T)  # convert s into nxp matrix, each row is s
   (x-m)/s		# output standardize score
}