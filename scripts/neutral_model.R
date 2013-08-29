S = 2
J = 100
P = S^-1
m = 0.001
N = numeric(S)
nsteps = 10000

neutral = function(S,J,P,m,N0,nsteps) {
	N = N0
	res = matrix(nr = nsteps, nc = S)

	for(i in 1:nsteps) {
	
		# Mortality
		N = N - rmultinom(n = 1, size = 1, prob = N/J) 

		# Recruitment
		N = N + rmultinom(n = 1, size = 1, prob = m*P + (1-m)*N/(J-1)) 
	 	
		res[i,] = N
	}	
	return(res)
}

N0 = rmultinom(n = 1, size = J, prob = numeric(S)+S^-1)

nsteps = 1e5
m = 0.2
test = neutral(S = S, J = J, P = P, m = m, N0 = N0, nsteps = nsteps)

plot(c(1:nsteps),test[,1],type = "l", ylim = c(0,J))




