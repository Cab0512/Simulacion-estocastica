

besaggib<-function(theta,y1ini,y2ini,y3ini,N){
  y1=rep(0,times=N)
  y1[1]=y1ini
  y2=rep(0,times=N)
  y2[1]=y2ini
  y3=rep(0,times=N)
  y3[1]=y3ini
  for (cont in 2:N){
    y1[cont]=rexp(1,rate=(1+theta[1]*y2[cont-1]+theta[3]*y3[cont-1]))
    y2[cont]=rexp(1,rate=(1+theta[1]*y1[cont]+theta[2]*y3[cont-1]))
    y3[cont]=rexp(1,rate=(1+theta[2]*y2[cont]+theta[1]*y1[cont]))
    
  }
  return(list(y1=y1,y2=y2,y3=y3))
}
N=500
simul=besaggib(c(1,2,3),1,1,1,N)
y1=simul$y1
y2=simul$y2
y3=simul$y3
plot(y1,type="l")
plot(y2,type="l")
plot(y3,type="l")
