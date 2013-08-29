rm(list=ls())
library(deSolve)
	
# SPATIAL PARAMATERS
# Cell size
delx = 1

# Number of cells
numboxes = 10
	
# Sequence
Distance = seq(from=0.5, by=delx, length.out = numboxes)

# DEFINE THE MODEL
model = function(Time, State, Pars) {
with(as.list(c(State,Pars)), {
	dR 		= r*R*(1-R/K) - a*R*C/(1+b*R)	
	deltax 	= c(0.5,rep(1,numboxes-1),0.5)    
	Flux    = -D*diff(c(0,C,0))/deltax
	dC    	= -diff(Flux)/delx + a*R*C/(1+b*R) - d*C
	list(c(dR,dC))
	})
}

# DEFINE ALL INITIAL CONDITIONS
R = rep(1,times = numboxes)       
C = rep(0.01,times = numboxes)
C[numboxes/2] = 0.1
state = c(R = R, C = C)  

# PARAMETERS 
pars = c(r = 1, K = 0.7, a = 1, b = 10, d = 0.1, D = 0.)	
times = seq(0,1000,by=0.1)             

# RUN THE SIMULATION
out = ode(y=state,times=times,func=model,parms=pars)  

# PLOT THE RESULTS
#par(oma=c(0,0,3,0))   
#color = topo.colors
#filled.contour(x=times,y=Distance,z = out[,(numboxes+2):ncol(out)],color= color, xlab="time", ylab= "Distance",main="Density")

plot(x = times, y = out[,(numboxes + numboxes/2)], type = "l")

#plot(x = times, y = out[,(numboxes/2)], type = "l")


