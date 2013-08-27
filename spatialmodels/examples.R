########################################################################
# Examples of dynamics on spatial graphs with different metacommunity perspectives
#
# By: Dominique Gravel (dominique_gravel@uqar.ca) 
# April 2013
#
########################################################################
###############################
# General parameters
S = 100
n = 25
r = 0.3
nsteps = 100

########################################
########################################
# Patch dynamics example
source("spatial_graphs.R")
source("patch_model.R")
c = 0.2
e = 0.1
spatial_graph = geograph(n, r)
results = patch_model(c, e, S, spatial_graph, nsteps)

# Figure 1: time series
x11(height = 5.5, width = 6)
plot(c(1:nsteps),apply(results[[2]][,2:(S+1)],1,sum),xlab = "Time", ylab = "Average local species richness",cex.lab = 1.5, cex.axis = 1.25, type = "l", ylim = c(0,S))

# Figure 2: local species richness
s = apply(results[[1]],1,sum)
vec.col = numeric(length(s))
RK = rank(s)
for(i in 1:n) vec.col[i] = rainbow(n,start = 0, end = 0.7)[RK[i]]
plot_spatial(spatial_graph, vec.col)

########################################
########################################
# Neutral example
source("spatial_graphs.R")
source("lottery_model.R")
# Default parameters
m = 0.2
M = 0.01
k = 0.1
J = 100
sdN = Inf
sdE = 5
spatial_graph = geograph_fn(n,r)
results = lottery_model(m,M,k,S,J,sdE,sdN,spatial_graph,nsteps)

# Figure 1: time series
x11(height = 5.5, width = 6)
plot(c(1:nsteps),apply(results[[2]][,2:(S+1)],1,sum),xlab = "Time", ylab = "Average local species richness",cex.lab = 1.5, cex.axis = 1.25, type = "l", ylim = c(0,S))

# Figure 2: local species richness
s = apply(results[[1]],1,sum)
vec.col = numeric(length(s))
RK = rank(s)
for(i in 1:n) vec.col[i] = rainbow(n,start = 0, end = 0.7)[RK[i]]
plot_spatial(spatial_graph, vec.col)


########################################
########################################
# Species sorting example
source("spatial_graphs.R")
source("lottery_model.R")
# Default parameters
m = 0.2
M = 0.01
k = 0.1
J = 100
sdN = 15
sdE = 5
spatial_graph = geograph_fn(n,r)
results = lottery_model(m,M,k,S,J,sdE,sdN,spatial_graph,nsteps)

# Figure 1: time series
x11(height = 5.5, width = 6)
plot(c(1:nsteps),apply(results[[2]][,2:(S+1)],1,sum),xlab = "Time", ylab = "Average local species richness",cex.lab = 1.5, cex.axis = 1.25, type = "l", ylim = c(0,S))

# Figure 2: local species richness
s = apply(results[[1]],1,sum)
vec.col = numeric(length(s))
RK = rank(s)
for(i in 1:n) vec.col[i] = rainbow(n,start = 0, end = 0.7)[RK[i]]
plot_spatial(spatial_graph, vec.col)

