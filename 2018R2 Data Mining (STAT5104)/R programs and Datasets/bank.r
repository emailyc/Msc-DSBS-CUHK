
d<-read.csv("bank.csv")	# read in data
acc<-factor(d[,1])		# change to factor
serv<-d[,2]

Lv1<-levels(acc)		# levels in acc
Lv2<-levels(serv)		# levels in serv

n<-length(Lv1)
p<-length(Lv2)

x<-matrix(0,nrow=n,ncol=p)	# initialize x

for (i in 1:n) {
  t1<-serv[acc==Lv1[i]]
  t2<-outer(t1,Lv2,"==")+0
  x[i,]<-apply(t2,2,sum)
}

colnames(x)<-Lv2		# apply col names
dx<-replace(x,x!=0,"yes")	# replace 0 to no, non-zero to yes
dx<-replace(dx,dx=="0","no")

write.csv(dx,"bank1.csv",row.names=F)	# save dx

### library(arules)

library(arules)		# load library arules
df<-data.frame(dx)		# change to data frame
serv<-as(df,"transactions")	# create sparse matrix

rules<-apriori(serv)		# assoc. rules
rules
summary(rules)

inspect(head(sort(rules,by="lift"),n=10))	# select 10 rules with largest lift value

r1<-subset(rules, subset = rhs %in% "CKING=yes")
inspect(head(sort(r1,by="support"),n=10))
inspect(head(sort(r1,by="confidence"),n=10))
inspect(head(sort(r1,by="lift"),n=10))


r2<-subset(rules, subset = lhs %in% "AUTO=no" & rhs %in% "CKING=yes")
inspect(head(sort(r2,by="lift"),n=10))




