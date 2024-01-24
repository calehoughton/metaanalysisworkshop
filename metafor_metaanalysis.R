install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")
# one data set
slope<- -1
intercept<-0
predictor<-rnorm(n=100,mean=10,sd=10)
response<-intercept+slope*predictor+rnorm(n=100,mean=0,sd=40)
plot(predictor,response)

#estimate slope
model<-lm(response~predictor)
summary(model)

# multiple datasets

store<-matrix(nrow=200,ncol=4)
#We need to create somewhere to store our data

for(x in 1:200){
  #we will simulate 200 different datasets 
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))+3
  #we're using this code to select sample sizes at random from a log normal distribution, so that small sample sizes are common and large sample sizes are rare. And n is always > 3.                  
  
  
  predictor<-rnorm(n=samplesize,mean=10,sd=10)
  response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
  #predictor and response are just as we used before, except now n is given by samplesize
  
  model<-lm(response~predictor)
  #same linear model as we ran before
  
  store[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4])
  #extract the model outputs
  
  
}
store<-as.data.frame(store)
names(store)<-c("n","slope","standard.error","p.value")

# making a funnel plot for this data
par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Slope",ylab="Sample size")
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)")

#significant slope estimates
sigslope<-which(store$p.value<0.05)
par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Slope",ylab="Sample size")
points(store$slope[sigslope],store$n[sigslope],pch=16,col="red")
abline(v=slope,lty=2)
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)")
points(store$slope[sigslope],(1/store$standard.error[sigslope]),pch=16,col="red")
abline(v=slope,lty=2)

#estimate mean effect, no variance
model2<-lm(slope~1,data=store)
summary(model2)

#estimate mean effect with metafor
library(metafor)
par(mfrow=c(1,1))
meta<-rma(yi=slope,sei=standard.error,data=store)
meta

#funnel and forest plots
funnel(meta)
forest(meta,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra")

###
# New dataset
latitude<-runif(100,0,90)
#we will randomly sample a latitude from 0,90 degree North
slope<-0+latitude*-0.1+rnorm(100,0,3)
plot(latitude,slope)


#meta analysis with species as a random effect
store2<-matrix(nrow=200,ncol=7)
#create somewhere to store our data

species<-rep(1:20,each=10)
specieseffect<-rep(rnorm(20,0,2),each=10)
#will use this to generate our 20 species random effects

for(x in 1:200){
  #simulate 200 different datasets 
  
  latitude<-runif(1,0,90)
  
  slope<-0+specieseffect[x]+latitude*-0.1+rnorm(1,0,3)
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))
  #select sample sizes at random from a log normal distribution, so  small sample sizes are common and large sample sizes are rare                    
  
  if(samplesize>3){
    #included this if function so that we don't run an analyses on datasets that are too small
    predictor<-rnorm(n=samplesize,mean=10,sd=10)
    response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
    
    model<-lm(response~predictor)
    #the same linear model as before
    
    store2[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4],latitude,species[x],x)
    #extract the model outputs we want and store them in our store matrix
    
    
  }}
store2<-as.data.frame(store2)
names(store2)<-c("n","slope","standard.error","p.value","latitude","species","ID")
#meta-analysis and funnel plot
plot(store2$slope,(1/store2$standard.error),xlab="Slope",ylab="Precision, (1/se)")
meta2 <-rma(yi=slope,sei=standard.error,data=store2)
meta2
funnel(meta2)

#metaanaylsis using lattitude as a covariate
meta3<-rma(yi=slope,sei=standard.error,mods=~latitude,data=store2)
meta3

#metaanalysis adding species as a random term
store2$se2<-store2$standard.error^2
store3<-store2[-which(is.na(store2$slope)==TRUE),]
#this function won't run with NAs, so we remove those rows
meta4<-rma.mv(yi=slope,V=se2,mods=~latitude,random=~1|species/ID,data=store3)
meta4


###
# using a real dataset
birdbroods<-read.csv("~/Downloads/birdbroods.csv",sep=",",header=TRUE)
plot(birdbroods$slope,(1/birdbroods$slope.SE),xlab="Slope",ylab="Precision, (1/se)")

birdbroods$se2<-birdbroods$slope.SE^2
meta5<-rma.mv(yi=slope,V=se2,random=~1|Species/id.pop,data=birdbroods)
meta5
forest(meta5,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra",order="obs")

# controlling for protected areas
meta6<-rma.mv(yi=slope,V=se2,mods=~protected.area,random=~1|Species/id.pop,data=birdbroods)
meta6


###
# randomly generating publication bias
store4<-matrix(nrow=200,ncol=4)
slope<- 0
intercept<-0
predictor<-rnorm(n=100,mean=10,sd=10)
response<-intercept+slope*predictor+rnorm(n=100,mean=0,sd=40)
for(x in 1:200){
  #we will simulate 200 different datasets 
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))+3
  #we're using this code to select sample sizes at random from a log normal distribution, so that small sample sizes are common and large sample sizes are rare. And n is always > 3.                  
  
  
  predictor<-rnorm(n=samplesize,mean=10,sd=10)
  response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
  #predictor and response are just as we used before, except now n is given by samplesize
  
  model<-lm(response~predictor)
  #same linear model as we ran before
  
  store4[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4])
  #extract the model outputs
  
  
}

store4<-as.data.frame(store4)
names(store4)<-c("n","slope","standard.error","p.value")

store4 <-store4[is.na(store4$slope)==FALSE,]
store4$publish<-0


#
store4$publish[store4$p.value<=0.05]<-1
largesamplesize<-intersect(which(store4$p.value>0.05),which(store4$n>30))
retainlarge<-largesamplesize[as.logical(rbinom(length(largesamplesize),prob=0.75,size=1))]
store4$publish[retainlarge]<-1
smallsamplesize<-intersect(which(store4$p.value>0.05),which(store4$n<=30))
retainsmall<-smallsamplesize[as.logical(rbinom(length(smallsamplesize),prob=0.25,size=1))]
store4$publish[retainsmall]<-1

par(mfrow=c(1,2))
plot(store4$slope,(1/store4$standard.error),xlab="Slope",ylab="Precision, (1/se)",main="Before")
plot(store4$slope[store4$publish==1],(1/store4$standard.error[store4$publish==1]),xlab="Slope",ylab="Precision, (1/se)",main="After")

regtest(x=slope, sei=standard.error, data=store4,
        model="rma", predictor="sei", ret.fit=FALSE)

regtest(x=slope, sei=standard.error, data=store4,
        model="rma", subset=publish==1, predictor="sei", ret.fit=FALSE)
