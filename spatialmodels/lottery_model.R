#######################################################################
# Individual based lottery model with spatially explicit
# dispersal and environmental heterogeneity
# 
# The model collapse to a neutral model when the niche breadth tends to infinity
#
# Store the results in a time series of occupancy, a presence-absence matrix
# and a local abundance matrix
# 
# By: Dominique Gravel (dominique_gravel@uqar.ca) 
# April 2013
########################################################################

# Main function
lottery_model = function(m,M,d,S,J,sdE,sdN,spatial_graph,nsteps) {
	# Args: 
	# 	m: immigration probability from the neighbourhood
	#	M: immigration probability from outside the metacommunity 
	#	k: local death rate 	
	# 	S: number of species
	#	J: local community size
	# 	sdE: within patch standard deviation of the environment
	# 	sdN: niche breadth
	#	spatial_graph: a spatial graph object
	#	nsteps: number of time steps to run the simulation
	#
	# Returns:
	#	A list with a time series of occupancy for each species, a site presence-absence matrix	
	#	and a site-abundance matrix from the last time step
	#	
	########################################
	# Prepare the simulation
	
	####################
	# Adjency matrix
	adjMat = spatial_graph[[2]] 	# The original matrix
	n = nrow(adjMat)				# Number of nodes
	degrees = apply(adjMat,2,sum) 	# Number of degrees for each node
	w = adjMat*matrix(degrees^-1,nr = n,nc=n, byrow=T) # Weighted adjancy matrix

	####################
	# Local environmental conditions
	Env = matrix(nr=n, nc = J)
	for(i in 1:n) Env[i,] = rnorm(J,mean = runif(1,0,100), sd = sdE) 	# For a random distribution of environments
#	for(i in 1:n) Env[i,] = rnorm(J,mean = i/n*100, sd = 0)				# For a uniform distribution of environments		
	####################
	# Niche optimums
	u = runif(S, 0, 100) 	# For a random distribution of optimums
#	u = 100*c(1:S)/S		# For a uniform distribution of optimums
	
	####################
	# Initialization of the metacommunity 
	# Starts with uniform abundance at each location
	localC = list()
	P = matrix(0,nr = n, nc = S)
	for(i in 1:n) {
		localC[[i]] = t(rmultinom(n = J,size = 1, prob = numeric(S)+J^-1))	
		P[i,] = apply(localC[[i]],2,sum)/J
	}
	
	####################
	# Matrix in which we record the occupancy over time  
	Series = matrix(nr=nsteps,nc=S+1)

	####################
	# Loop over all time steps
	for(time in 1:nsteps) {

		# Calculate the weighted relative abundance in the neighboring communities
		wP = w%*%P/apply(w,1,sum)
		
		####################
		# Loop across the patches	
		for(i in 1:n) {	
			# Kill individuals at random
			rand = runif(J,0,1)
			localC[[i]][rand < k,] = 0
		
			####################
			# Calculate recruitment probability		
			# Relative abundance in the seed rain				
			rel_seed = M*S^-1 + m*wP[i,] + (1-m-M)*apply(localC[[i]],2,sum)/sum(localC[[i]])
	
			# Weighting by local environmental conditions
			surv = exp(-(matrix(Env[i,],nr = J, nc = S, byrow = F) - matrix(u,nr = J, nc = S, byrow = T))^2/2/sdN^2)
			recruitProb = surv*matrix(rel_seed,nr = J,nc = S,byrow = T)/apply(surv*matrix(rel_seed,nr = J,nc = S,byrow = T),1,sum)
	
			####################
			# Replace dead individuals
			recruit = t(apply(recruitProb,1,recruit_fn))	
			localC[[i]][rand<d,] = recruit[rand<d,] 
		
			# Record local relative abundance
			P[i,] = apply(localC[[i]],2,sum)/sum(localC[[i]])	
		}
		
	####################	
	# Transform abundance in presence/absence
	pres = matrix(0,nr = n, nc = S)
	pres[P>0] = 1
	occ = apply(pres,2,mean)			

	# Record occupancy
	Series[time,] = c(time,occ)
	}
	
	# Output
	return(list(pres,Series,P))
}

# Convenient function used in the simulation
recruit_fn = function(prob) rmultinom(n = 1, size = 1, prob = prob)









