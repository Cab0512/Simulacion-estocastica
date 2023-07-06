library(statmod)

y=matrix(0,nrow=10,ncol=30)

sigmav=3
tauv=1
betav=5

uv=rnorm(nrow(y),mean = 0, sd = 3)
for (i in 1:nrow(y)){
  for(j in 1:ncol(y)){
    y[i,j]=betav+uv[i]+rnorm(1,mean=0,sd=tauv)
  }
}

mixeffecgib<-function(y,uini,betaini,tauini,sigmaini,N){
  I=length(uini)
  J=ncol(y)
  u=matrix(0,nrow=N,ncol=I)
  u[1,]=uini
  beta=rep(0,times=N)
  beta[1]=betaini
  sigma2=rep(0,times=N)
  sigma2[1]=sigmaini
  tau2=rep(0,times=N)
  tau2[1]=tauini
  for (cont in 2:N){
    sdit=sqrt((J*(tau2[cont-1]+sigma2[cont-1]))^{-1})
    for (i in 1:length(uini)){
      med=J*(mean(y[i,])-beta[cont-1])/(J+tau2[cont-1]*(sigma2[cont-1]^{-1}))
      u[cont,i]=rnorm(1,mean=med,sd=sdit)
    }
    promy=mean(c(y))
    sdbeta=sqrt(tau2[cont-1]/(J*I))
    beta[cont]=rnorm(1,mean=(promy-mean(u[cont,])),sd=sdbeta)
    sigma2[cont]=rinvgauss(1, mean=I/2 , dispersion=0.5*sum(u[cont,]^2))
    disptau=0
    for (i in 1:nrow(y)){
      for (j in 1:ncol(y)){
        disptau=disptau+(y[i,j]-u[cont,i]-beta[cont])^2
      }
    }
    disptau=disptau*0.5
    tau2[cont]=rinvgauss(1,mean=I*J/2,dispersion=disptau)
  }
  
  return(list(u=u,beta=beta,sigma2=sigma2,tau2=tau2))
}

N=300
uini=rep(0,times=nrow(y))
betaini=0
tauini=1
sigmaini=1

simulef=mixeffecgib(y,uini,betaini,tauini,sigmaini,N)

summary(simulef$beta)
hist(simulef$beta)
plot(simulef$beta,type="l")
