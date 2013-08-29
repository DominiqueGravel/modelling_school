 
rm(list=ls())
 
 #----------------------#
 # the model functions: #
 #----------------------#
 
 # Dr = diffusion rate m2/time
 # igr = intrinsic growth rate
 # K = carrying capacity
 # delx = cell size
 # numboxes = number of cells
 # a, b, d, r parameters of interaction prey-predator
 

  
 model <-function(Time, state, parameters){ 
 # N.B. first argument must be Time even if it is not used in the model function
  	
 	with(as.list(parameters), {
 	

  	deltax = c(0.5, rep(1, numboxes-1), 0.5)
    R = state[1:numboxes]
    C = state[(numboxes+1):length(state)]

    #Flux_R = - Dr * diff(c(0, R,0))/deltax # ressource may migrate too!
    Flux_C = - Dr * diff(c(0, C,0))/deltax
    
    dR =  r* R * (1-R/K) - a*R*C/(1+b*R) #- diff(Flux_R)/delx 
    dC =  a*R*C/(1+b*R) - d*C - diff(Flux_C)/delx 

  	
  	return(list(N=c(dR, dC)))
  	# N.B. model must return a list
  	
  	})
              
 }




 #----------------------------------------------------#
 # RUNNING the model with a given set of parameters   #
 #----------------------------------------------------#
 
 times     <- seq(0,500,1)
 
 par1 <- c(Dr =0.3, K=1, delx=1, numboxes =300, a=1, b=5, d=0.1, r=1)
 
 # random initial state
 R0 = runif(par1["numboxes"], 0,par1["K"] )
 C0 = runif(par1["numboxes"], 0,par1["K"] )
 stateInit = c(R0,C0)

 library(deSolve)
 
 out = ode(stateInit, times, model, par1)
 
 

 #----------------------#
 # GRAPH                #
 #----------------------#
 
par(mfrow = c(1,2), oma =c(0,0,3,0))


#image(x=times, y= seq(from=0.5, by=par1["delx"], length.out=par1["numboxes"]), out[,2:(par1["numboxes"]+1)], xlab="time", ylab="distance", main="density R", col=terrain.colors(24))


image(x=times, y= seq(from=0.5, by=par1["delx"], length.out=par1["numboxes"]), out[,(par1["numboxes"]+2):ncol(out)], xlab="time", ylab="distance", main="density C", col= terrain.colors(16))
# N.B. the palette 'terrain.colors' spread from green (min) to lightgrey (max)

plot(times, out[,par1["numboxes"]/2], type="l", ylim = c(0, max(out[,-1])), ylab = "densities in one point", main="red =C, black=R")
lines(times, out[,par1["numboxes"]/2 + par1["numboxes"]], type="l", col="red")

