library(BayesGOF)
lau=Rata.Tumor

#Logaritmo cond posteriori

lpost<-function(alpha,beta,theta){
  theta<-as.vector(theta)
    pos=-2.5*log(alpha+beta)
  pos=pos+log(gamma(alpha+beta))-log(gamma(alpha))-log(gamma(beta))
  pos=pos+sum((alpha-1)*log(theta)+(beta-1)*log(1-theta))
  return(pos)  
}


lauchasgib<-function(y,n,thetaini,aini,bini,sda,sdb,N){
  theta=matrix(0,nrow=N,ncol=length(y))
  theta[1,]=thetaini
  alpha=rep(0,times=N)
  alpha[1]=aini
  beta=rep(0,times=N)
  beta[1]=bini
  
  apruebo=0
  for (cont in 2:N){
    for (i in 1:71){
      theta[cont,i]=rbeta(1,shape1=alpha[cont-1]+y[i],shape2=beta[cont-1]+n[i]-y[i])
    }
    alpha[cont]=alpha[cont-1]
    beta[cont]=beta[cont-1]
    apropos=rnorm(1,mean=alpha[cont],sd=sda)
    bpropos=rnorm(1,mean=beta[cont],sd=sdb)
    if( (apropos>0) && (bpropos>0)){
      print(c(cont,apropos,bpropos))
      u=runif(1,min=0,max=1)
      lograt=lpost(apropos,bpropos,theta[cont,])-lpost(alpha[cont],beta[cont],theta[cont,])
      print(c(cont,lograt))
      if (log(u)<lograt){
        alpha[cont]=apropos
        beta[cont]=bpropos
        apruebo=apruebo+1
    }
    }
    
  }
  return(list(theta=theta,alpha=alpha,beta=beta,ratioacept=apruebo/N))
}

y=lau$y
n=lau$N
N=500

#Datos iniciales de los daneses
alphaini=1.6
betaini=10
thetaini=y/n
sda=0.5
sdb=2.5

simul<-lauchasgib(y,n,thetaini,alphaini,betaini,sda,sdb,100)
simul


plot(simul$alpha,simul$beta)




