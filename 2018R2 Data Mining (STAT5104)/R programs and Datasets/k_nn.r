# improved version of knn
# v is an integer vector containing all the value of k to be tested
# cl0, cl1 are class label for train and test data
# if k is an integer, 1 to k is assumed
# return the k with least error rate

library(class)
k_nn<-function(x0,x1,cl0,cl1,v,l=0,prob=F,use.all=T) {
  err0=1					# initialize error rate
  if (length(v)==1) v<-c(1:v)			# change v to an integer vector 1:v

  for (k in v) {		
    res<-knn(x0,x1,cl0,k,l,prob,use.all)	# apply knn
    ctab<-table(res,cl1)			# save c-table
    err<-1-sum(diag(ctab))/sum(ctab)		# compute error rate
    if (err<err0) {				# update if err<err0
      k0<-k
      res0<-res
      err0<-err
      ctab0<-ctab
    }
    cat("k=",k," error rate=",err,"\n")		# display results
  }  
  cat("best k=",k0," error rate=",err0,"\n")	# display best result
  res0						# output res0
}

