#Propri media
tau_pri=1/50^2
mu_pri=200

#curve: grafica funcion 
curve(dnorm(x, mean = mu_pri, sd = 1/sqrt(tau_pri)), from = 0, to = 350)

#priori tau
alpha_pri=10
beta_pri=0.002
curve(dgamma(x, shape = alpha_pri, scale = beta_pri), from = 0, to = .05)


#Priori conjunta
mu_mall=seq(50, 300, length.out = 100)
tau_mall=seq(0,.05,length.out=100)
priori<-function(mu, tau){
  x=dnorm(mu, mean = mu_pri, sd = 1/sqrt(tau_pri))
  y=dgamma(tau, shape = alpha_pri, scale = beta_pri)
  return(x*y)
}
#outer (x,y,funcion) hace el producto cartesiano y evalua en la funcion
priori_mall=outer(mu_mall, tau_mall, priori)
#grafica densidad funcion R^2 a R
image(mu_mall, tau_mall, priori_mall)
#contuor grafica las curvas dee nivel 
contour(mu_mall, tau_mall, priori_mall, add = TRUE)

#Simular gausiana
muv=250
tauv=1/10^2
n=50
x=rnorm(n, mean = muv, sd = sqrt(1/tauv))
xbar=mean(x)
#breaks(inicio,final, numero intervalos)
hist(x, prob = TRUE, breaks = seq(min(x), max(x), length.out =7))
curve(dnorm(x, mean = muv, sd = 1/sqrt(tauv)), add = TRUE)

#Posteriori
mumall=seq(248, 256, length.out = 100)
taumall=seq(0.005, .02, length.out = 100)
postmall=matrix(0, nrow = length(mumall), ncol = length(taumall))
for(i in seq_along(mumall)){
  for(j in seq_along(taumall)){
    postmall[i,j]=priori(mumall[i], taumall[j])*prod(dnorm(x, mean = mumall[i], sd = 1/sqrt(taumall[j])))
  }
}

image(mumall, taumall, postmall)
contour(mumall, taumall, postmall, add = TRUE)

#Full condicionales
rmu <- function(tau){
  condprom=(n*tau*xbar + tau_pri*mu_pri)/(n*tau + tau_pri)
  condtau=n*tau + tau_pri
  return(rnorm(1, mean = condprom, sd = sqrt(1/condtau)))
}

rtau <- function(mu){
  condalpha=n/2 + alpha_pri
  condbeta=1/(sum((x-mu)^2)/2 + 1/beta_pri)
  return(rgamma(1, shape = condalpha, scale = condbeta))
}

#Gibbs Sampler
nsimul=200 
simulmu=rep(0, nsimul)
simultau=simulmu
simultau[1]= alpha_pri * beta_pri 
simulmu[1]=mu_pri
for(i in 2:nsimul){
  simulmu[i]=rmu(simultau[i-1])
  simultau[i]=rtau(simulmu[i])
}

#Trayectorias
simulaciones=cbind(simulmu, simultau)
par(mfrow=c(1,2))
plot(simulaciones, type = "l")
burn=1:25
plot(simulaciones[-burn,], type = "l")
chain=simulaciones[-burn,]

#Comparando densidades
par(mfrow=c(1,2), mar = c(2,2,.5,.5))
image(mumall, taumall, postmall)
contour(mumall, taumall, postmall, add = TRUE)
dens_chain=kde2d(chain[,1], chain[,2], n = 100,lim=c(range(mumall),c(range(taumall))))
image(dens_chain, xlim = range(mumall), ylim = range(taumall))
contour(dens_chain, add = TRUE)
        
#Introducción Coda
library(coda)
chain <- mcmc(chain)
summary(chain)
plot(chain)
