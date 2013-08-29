rm(list=ls())
library(deSolve)
	
# SPATIAL PARAMETERS
# Cell size
delx = 1

# Number of cells
numboxes = 100
	
# Sequence
Distance = seq(from=0.5, by=delx, length.out = numboxes)

# DEFINE THE MODEL
model = function(Time, State, Pars) {
with(as.list(c(State,Pars)), {
	R = State[1:numboxes]
	C = State[(numboxes+1):(numboxes*2)]
	dR 		= r*R*(1-R/K) - a*R*C/(1+b*R)	
	deltax 	= c(0.5,rep(1,numboxes-1),0.5)    
	Flux    = -D*diff(c(0,C,0))/deltax
	dC    	= -diff(Flux)/delx + a*R*C/(1+b*R) - d*C
	list(c(dR,dC))
	})
}

# DEFINE ALL INITIAL CONDITIONS
N0 = c(runif(numboxes,0,1),runif(numboxes,0,01))

# PARAMETERS 
pars = c(r = 1, K = 10, a = 1, b = 10, d = 0.1, D = 0.3)	
times = seq(0,1000,by=0.1)             

# RUN THE SIMULATION
out = ode(y=N0,times=times,func=model,parms=pars)  

# PLOT THE RESULTS
#par(mar = c(5,5,2,1))
#par(oma=c(0,0,3,0))   
#color = topo.colors
#filled.contour(x=times,y=Distance,z = out[,(numboxes+2):ncol(out)],color= color, xlab="time", ylab= "Distance",main="Density")

plot(times,out[,150],type = "l")

#plot(x = times, y = out[,(numboxes/2)], type = "l")


