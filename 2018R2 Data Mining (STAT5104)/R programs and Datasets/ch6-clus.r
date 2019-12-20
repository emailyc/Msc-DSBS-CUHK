d<-read.csv("iris.csv")		# read in data
x<-d[,1:4]			# save the the first columns to x
km<-kmeans(x,3)			# K-means clustering
print(km)			# print result

readline()
plot(x,col=km$cluster)
table(km$cluster,d$Species)

km			# display k-means result

readline()
d1<-x[km$cluster==1,]	# select group using cluster label
d2<-x[km$cluster==2,]
d3<-x[km$cluster==3,]
dim(d1)			# display group size
dim(d2)
dim(d3)
apply(d1,2,mean)	# display group mean
apply(d2,2,mean)
apply(d3,2,mean)

61*var(d1)		# display sum of square matrix
sum(diag(61*var(d1)))	# display within group ss
sum(diag(49*var(d2)))
sum(diag(37*var(d3)))

readline()
source("km.r")		# improved kmeans
km2<-km(x,2)		# try different values of k
km3<-km(x,3)
km4<-km(x,4)
km5<-km(x,5)

readline()
# HMEQ exmaple
d<-read.csv("hmeq1.csv")	# read in dataset
x<-d[,-c(1,5,6)]		# exclude nominal variables

km2<-km(x,2,try=20)		# try various k
km3<-km(x,3,try=20)
km4<-km(x,4,try=20)
km5<-km(x,5,try=20)	# max stat
km6<-km(x,6,try=20)
readline()


# scale to [0,1]
source("scale.r")	# load scale
z<-scale.con(x)		# rescale continuous variables
w<-scale.dum(d[,5:6])	# rescale nominal variables
z<-cbind(z,w)		# combine z and w column-wise

km2<-km(z,2,try=20)
km3<-km(z,3,try=20)	# max stat
km4<-km(z,4,try=20)
km5<-km(z,5,try=20)
km6<-km(z,6,try=20)

readline()
par(mfrow=c(2,5))	# define 1x5 multiframe graphic
lab<-factor(km3)	# convert label into factor

plot(lab,z$LOAN,main="LOAN")		# boxplots with group labels from km3	
plot(lab,z$MORTDUE,main="MORTDUE") 
plot(lab,z$VALUE,main="VALUE")
plot(lab,z$YOJ,main="YOJ")
plot(lab,z$DEROG,main="DEROG")

plot(lab,z$DELINQ,main="DELINQ")	
plot(lab,z$CLAGE,main="CLAGE") 
plot(lab,z$NINQ,main="NINQ")
plot(lab,z$CLNO,main="CLNO")
plot(lab,z$DEBTINC,main="DEBTINC")

readline()
# apply rpart using km3 as class label
library(rpart)		# load library
ctree<-rpart(km3~.,data=x,method="class",maxdepth=4)
print(ctree)		# print result


readline()
# hcluster
d<-read.csv("hclus-ex.csv")	# read in data
dist(d,diag=T,upper=T)		# compute distance matrix

readline()
par(mfrow=c(1,1))
plot(d)
text(d$x+0.01,d$y+0.01,1:6)

readline()
hc1<-hclust(dist(d),method="single")
hc2<-hclust(dist(d),method="complete")
hc3<-hclust(dist(d),method="average")
hc4<-hclust(dist(d),method="ward")

readline()
par(mfrow=c(2,2))
plot(hc1,hang=-1)
plot(hc2,hang=-1)
plot(hc3,hang=-1)
plot(hc4,hang=-1)


