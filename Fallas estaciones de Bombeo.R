y=c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t=c(94, 16, 63, 126, 5, 31, 1, 1, 2, 10)

fallasbombeogibbs<-function(y,t,lambdaini,betini,N){
  lambda=matrix(0,nrow=N,ncol=length(y))
  lambda[1,]=lambdaini
  bet=rep(0,times=N)
  bet[1]=betini
  for (cont in 2:N){
    for (i in 1:10){
      lambda[cont,i]=rgamma(1, y[i]+1,t[i]+bet[cont-1])
    }
    bet[cont]=rgamma(1,11,(sum(lambda[cont,])+40))
  }
  return(list(lambda=lambda,bet=bet))
}
N=1000
simul=fallasbombeogibbs(y,t,c(1,1,1,1,1,1,1,1,1,1),11,N)
simul
plot(simul$lambda[-c(1:3),1],type="l")
summary(simul$lambda)
