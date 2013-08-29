rm(list=ls())
library(deSolve)

# Parameters
D = 0.1    
r = 1
K = 0.7
a = 1
b = 10
d = 0.1
			
# Cell size
delx = 1

# Number of cells
numboxes = 100
	
# Sequence
Distance = seq(from=0.5, by=delx, length.out = numboxes)

model = function(Time, State, Pars) {
with(as.list(c(State,Pars)), {
	dR 		= r*R*(1-R/K) - a*R*C/(1+b*R)	
	deltax 	= c(0.5,rep(1,numboxes-1),0.5)    
	Flux    	= -D*diff(c(0,C,0))/deltax
	dC    	= -diff(Flux)/delx + a*R*C/(1+b*R) - d*C
	list(c(dR,dC))
	})
}

R = rep(K,times = numboxes)       
C = rep(0.01,times = numboxes)
C[50:51] = 0.1

state = c(R = R, C = C)          

times = seq(0,10,by=0.01)             

out = ode(y=state,times=times,func=model,parms=0)  

DENSITY   <- out[,2:(numboxes+1)]

# set margins
par(mfrow=c(1,1))
par(oma=c(0,0,3,0))   

# set colors
color = topo.colors

# make the plot
filled.contour(x=times,y=Distance,DENSITY,color= color,
xlab="time", ylab= "Distance",main="Density")



