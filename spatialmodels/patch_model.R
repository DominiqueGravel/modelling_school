########################################################################
# Multi-species discrite-time Levins metapopulation model
# Spatially explicit local dispersal
# Spatially uniform environment
# 
# Store the results in a time series of occupancy
# and a presence absence matrix
#
# By: Dominique Gravel (dominique_gravel@uqar.ca) 
# April 2013
########################################################################

patch_model = function(c, e, S, spatial_graph, nsteps) {
	# Runs a muti-species Levins model on a spatial graph
	#
	# Args: 
	# 	c: colonization probability 
	#	e: extinction probability
	# 	S: number of species
	#	spatial_graph: a spatial graph object
	#	nsteps: number of time steps to run the simulation
	#
	# Returns:
	#	A list with a time series of occupancy for each species and a presence-absence
	# 	matrix from the last time step
	#
	########################################
	# Prepare the simulation
	
	####################
	# Adjency matrix
	adjMat = spatial_graph[[2]] # The original matrix
	n = nrow(adjMat)	# Number of nodes
	degrees = apply(adjMat,2,sum) # Number of degrees for each node
	ColProb =  c*adjMat*matrix(degrees^-1,nr = n,nc=n, byrow=T) # Colonization probability weighted by the number of out degrees

	####################
	# Initialization of the metacommunity in the presence-absence matrix pres
	# Global distribution at the beginning of the run
	pres = matrix(1, nr = n, nc = S)

	####################
	# Matrix in which we record the occupancy over time  
	Series = matrix(nr=nsteps,nc=S+1)
	
	########################################
	# Loop over all time steps
	for(time in 1:nsteps) {
		
		####################
		# Test if there is extinction
		ExtMat = matrix(0,nr=n,nc=S)
		randExt = matrix(runif(n*S,0,1),nr=n,nc=S)
		ExtMat[pres == 1 & randExt < e] = -1

		####################		
		# Test if there is colonization
		ColMat = matrix(0,nr=n,nc=S)
		randCol = matrix(runif(n*S,0,1),nr=n,nc=S)

		# Calculate how many populations are connected to the focal patch
		ConPop = ColProb%*%pres		

		# Perform the test
		ColMat[pres == 0 & randCol < ConPop] = 1

		####################
		# Apply changes in presence-absence		
		pres = pres + ExtMat + ColMat
		
		# Record occupancy
		Series[time,] = c(time, apply(pres,2,sum)/n)
		
	}

	return(list(pres,Series))	
}





