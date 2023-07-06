a=(-1+2*rbinom(1,1,0.5))*rexp(1,rate=1)
M=sqrt(2/pi)*exp(1/2)
sima=c()
simr=c()
for (i in 1:100){
  x=rexp(1,rate=1)
  if(rbinom(1,1,0.5)==1){
    x=-x
  }
  u=runif(1,min=0,max=1)
  gx=0.5*exp(-abs(x))
  fx=sqrt(1/(2*pi))*exp(-0.5*x^2)
  if (u<fx/(M*gx)){
    sima=c(sima,x)
  }
  else{
    simr=c(simr,x)
  }
}
simr
length(sima)/100
