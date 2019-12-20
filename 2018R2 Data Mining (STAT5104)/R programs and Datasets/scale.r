
# function to scale continuous or ordinal variables to [0,1]
scale.con<-function(d) {
  n<-dim(d)[1]		# row dim of d
  p<-dim(d)[2]		# column dim of d
  
  cmin<-apply(d,2,min)	# column min of d
  cmax<-apply(d,2,max)	# column max of d
  range<-cmax-cmin	# column range
  cmin<-matrix(cmin,nr=n,nc=p,byrow=T)	  # change cmin to a nxp matrix
  range<-matrix(range,nr=n,nc=p,byrow=T)  # change range to a nxp matrix
  
  (d-cmin)/range	# transform d
}  
  

# function to convert categorical variable with k levels to (k-1) dummy variables
cat2dum<-function(v) {
  v<-factor(v)		  # change v to factor
  lab<-levels(v)	  # get value in v
  k<-length(lab)	  # get no. levels in v
  x<-outer(v,lab,"==")+0  # create matrix x with k columns
#  x[,1:(k-1)]		  # exclude the last column
  x			  # output x
}  


# function to convert nominal variables to dummy variables
scale.dum<-function(d) {
  n<-dim(d)[1]		# row dim of d
  p<-dim(d)[2]		# col. dim of d
  x<-NULL		# initalize x
  for (i in 1:p) {
    v<-d[,i]		# get the i-th col in d
    z<-cat2dum(v)	# convert into z, matrix dummy var.
    x<-cbind(x,z)	# column-binding of z
  }
  k<-dim(x)[2]		# col. dim of x
  nh<-apply(x,2,sum)	# compute frequency of each category
  nk<-matrix(nh,nrow=n,ncol=k,byrow=T)	# create nxk matrix with each rows is nh
  x/(2*nk)		# output
}





    
    
  
