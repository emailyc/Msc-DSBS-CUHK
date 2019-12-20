# association rule using arules

d<-read.csv("titanic.csv")	# read in data

library(arules)			# load library arules
x<-as(d,"transactions")		# change d to transactions
summary(x)			# display summary

rules<-apriori(x)		# assoc rule
summary(rules)			# display summary

rules<-apriori(x, parameter=list(support=0.2,confidence=0.7))	# min support=0.2 and min conf=0.7
summary(rules)			# display summary

inspect(head(sort(rules,by="lift"),n=10))		# display 10 rules with largest lift value

r1<-subset(rules, subset = rhs %in% "Survive=no")	# select rules with Survive=no on rhs
inspect(r1)			# display r1

r2<-subset(rules, subset = lhs %in% "Class=3rd")	# select rules with Class=3rd on lhs
inspect(r2)
