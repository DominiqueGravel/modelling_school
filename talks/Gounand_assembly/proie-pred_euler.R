		#  Modèle Proie-Prédateur	#
		#===============================#
rm(list = ls())
#setwd("~/Desktop/WTERIM2013/C-R")
#system.time(source("proie-pred_euler.R"))
#===========
## Cinétique
#===========

#====================
#Choix des paramètres
#====================
r=0.5
K=5.5
a=0.2
b=0.4
m=0.2

pars=c(r=r,K=K,a=a,b=b,m=m)
n1=1
n2=0.5
init=c(n1=n1,n2=n2)
names(init)=c("n1","n2")


#===========
## Cinétique
#===========

#Equation dérivée 		
#----------------
deriv = function(t,y,parms){
		with(as.list(c(y,parms)), {
	dn1=n1*(r*(1-n1/K)-a*n2/(1+b*n1))
	dn2=n2*(a*n1/(1+b*n1)-m)
		list(c(dn1,dn2))
	})
}

Tmax=100000
pas=0.1
times=seq(1,Tmax,pas)

library(deSolve)
datas=as.matrix(ode(init,times,deriv,pars,method="euler"))



#plot(datas[,1],datas[,2],ylim=range(datas[,2:3]),type="l")
#lines(datas[,1],datas[,2],col="red")