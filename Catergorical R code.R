#Chi-squared test of independence
data <- matrix(c(9,8,27,8,47,236,23,39,88,49,179,706,28,48,89,19,104,293),ncol=6,byrow=TRUE)
dimnames(data) = list(Degree=c("< HS","HS","College"),Belief=c("1","2","3","4","5","6"))
chisq.test(data)
chisq.test(data)$stdres

#Likelihood ratio test of independence
G2 = 2*sum(data*log(data/chisq.test(data)$expected))
P.G2=1-pchisq(G2, prod(dim(data)-1))

#Wald CI for difference in proportion in 2x2 tables
phs <- matrix(c(189,10845,104,10933),byrow=TRUE,ncol=2)
dimnames(phs) <- list(Group=c("Placebo","Aspirin"),MI=c("Yes","No"))
phs.test = prop.test(phs)

#Wald CI for relative risk
r =phs.test$estimate[1]/phs.test$estimate[2] 
ASE <- sqrt(sum((1-phs[,1]/rowsum(phs, group=c(1,1)))/phs[,1]))
logr.CI <- log(r) + c(-1,1)*1.96*ASE
exp(logr.CI)


#Wald CI for odds ratio
odds <- phs.test$estimate/(1-phs.test$estimate)
theta <- phs[1,1]*phs[2,2]/(phs[1,2]*phs[2,1])
ASE <- sqrt(sum(1/phs))
logtheta.CI <- log(theta) + c(-1,1)*1.96*ASE
exp(logtheta.CI)

#LRT CI for odds ratio in the aspirin and heart attack example
yes = c(189, 104)
no = c(10845, 10933)
n = yes+no
x = c(1, 0)
fit <- glm(yes/n ~x, weights=n, family=binomial(link = "logit"))
confint(fit)
exp(confint(fit)[2,])

#score CI for difference in proportion, relative risk and odds ratio
#see www.stat.ufl.edu/~aa/cda/R/two-sample/R2/index.html


#mosaic plot for eduation and God example
x<- c(9,8,27,8,47,236,23,39,88,49,179,706,28,48,89,19,104,293)
data <- matrix(x, nrow=3,ncol=6, byrow=TRUE)
dimnames(data) = list(Degree=c("< HS","HS","College"),Belief=c("1","2","3","4","5","6"))
#install.packages("vcdExtra")
library("vcdExtra")
StdResid <- chisq.test(data)$stdres
StdResid <- matrix(StdResid,nrow=3,ncol=6,byrow=TRUE)
mosaic(data,residuals = StdResid, gp=shading_Friendly)


#strafified 2x2x2 tables
vic.race<-c("white","black")
def.race<-vic.race
death.penalty<-c("yes", "no")
datalabel<-list(defendant=def.race,death=death.penalty,victim=vic.race)
table.2.6<- expand.grid(defendant=def.race,death=death.penalty,victim=vic.race)
data<-c(53, 11, 414, 37, 0, 4, 16, 139)
table.2.6<-cbind(table.2.6,count=data)
a = xtabs(count~defendant+death+victim ,data=table.2.6)
#marginal table
apply(a, c(1,2), sum)
#test of conditional independence
mantelhaen.test(a)
#Conditional odds ratios
summary(oddsratio(a, log=F, stratum=2))


#ordinal trend
income<-c("<15000","15000-25000","25000-40000",">40000")
jobsat<-c("VD","LD","MS","VS")
table.2.8<-expand.grid(income=income,jobsat=jobsat)
data<-c(1,2,1,0,3,3,6,1,10,10,14,9,6,7,12,11)
table.2.8<-cbind(table.2.8,count=data)
temp<-xtabs(count~income+jobsat,table.2.8)
GKgamma(temp)

#linear trend alternative
levels(table.2.8$income)<-c(7.5,20,32.5,60)
levels(table.2.8$jobsat)<-1:4
res<-table.2.8[,1:2][rep(1:nrow(table.2.8),table.2.8$count),]

newtable <- matrix(NA,dim(table.2.8))
res$income=as.numeric(levels(res$income))
res$jobstat=as.numeric(levels(res$jobstat))


M2=(cor(res)[2,1]^2)*(nrow(res)-1)  #M-square=(n-1)r^2
p.val=1-pchisq(3.807461,1) 

#Fisher
test<-fisher.test(matrix(c(3,1,1,3),byrow=T,ncol=2))
#one-sided p-value
fisher.test(matrix(c(3,1,1,3),byrow=T,ncol=2), alternative="greater")

#unconditional test of independence
res<-optim(par=.25, fn=function(pi){ log(2) + 3*log(pi) + 3*log(1-pi)},
           method="L-BFGS-B", lower=.00001, upper=.9999, control=list(fnscale=-1))
exp(res$value) #p-value












