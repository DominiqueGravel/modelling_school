########################################################################
# Source code for the figures 1-4 in the paper
# Gravel, Poisot and Desjardins. 2013. Using neutral theory to reveal the contribution of dispersal to community assembly in complex landscapes. J. Limnology. 
#
# By: Dominique Gravel (dominique_gravel@uqar.ca) 
# April 2013
#
########################################################################
source("spatial_graphs.R")
source("patch_model.R")
source("lottery_model.R")
library(vegan)

###############################
# Useful functions
alpha_fn = function(pres) {
	s = apply(pres,1,sum)
	vec.col = numeric(length(s))
	RK = rank(s)
	for(i in 1:n) vec.col[i] = rainbow(n,start = 0, end = 0.7)[RK[i]]
	return(vec.col)	
}

plot_alpha = function(spatial_graph, vec.col) {
  	# Plots a spatial graph 
  	#
  	# Args:
  	#   spatia_graph: the output of one of the spatial graphs
  	#
	par(mar=c(1,1,1,1))
	XY = spatial_graph[[1]]
	adjMat = spatial_graph[[2]]

	plot(XY[,1],XY[,2],xlab = "", ylab = "",cex = 1.5,labels = F)
	adjVec = stack(as.data.frame(adjMat))[,1]
	XX = expand.grid(XY[,1],XY[,1])
	YY = expand.grid(XY[,2],XY[,2])
	XX = subset(XX,adjVec==1)
	YY = subset(YY,adjVec==1)
	arrows(x0 = XX[,1],x1=XX[,2],y0 = YY[,1], y1 = YY[,2], length = 0,lwd = 0.2, col = "darkgrey")
	points(XY[,1],XY[,2],pch=21,bg=vec.col,cex = 1.5)
}

###############################
# General parameters
S = 100
n = 25
r = 0.3

###############################
# Draw the four landscapes
connected = connected_fn(n)
lattice = lattice_fn(n)
geograph = geograph_fn(n,r)
geotree = geotree_fn(n,r)

# Geographic distances among plots
distConnected = as.matrix(dist(connected[[1]], upper = T, diag = T))
distLattice = as.matrix(dist(lattice[[1]], upper = T, diag = T))
distGeograph = as.matrix(dist(geograph[[1]], upper = T, diag = T))
distGeotree = as.matrix(dist(geotree[[1]], upper = T, diag = T))

# Topologic distances among plots
topoLattice = spm(lattice[[2]])
topoGeograph = spm(geograph[[2]])
topoGeotree = spm(geotree[[2]])

# Degree centrality
degGeograph = deg_cen(geograph)
degGeotree = deg_cen(geotree)

# Eigen centrality
eigGeograph = eig_cen(geograph)
eigGeotree = eig_cen(geotree)

# Closeness centrality
clsGeograph = cls_cen(geograph)
clsGeotree = cls_cen(geotree)

###############################
# Run the three models with the four landscapes

###############
# Patch dynamics
# Specific parameters
c = 0.4
e = 0.1
nsteps = 1000

# Simulations:
patchConnected = patch_model(c, e, S, connected, nsteps)
patchLattice = patch_model(c, e, S, lattice, nsteps)
patchGeograph = patch_model(c, e, S, geograph, nsteps)
patchGeotree = patch_model(c, e, S, geotree, nsteps)

# Alpha diversity
alphaPatchConnected = alpha_fn(patchConnected[[1]])
alphaPatchLattice = alpha_fn(patchLattice[[1]])
alphaPatchGeograph = alpha_fn(patchGeograph[[1]])
alphaPatchGeotree = alpha_fn(patchGeotree[[1]])

# Beta diversity
betaPatchConnected = as.matrix(vegdist(patchConnected[[1]],method = "bray",diag=T,upper=T))
betaPatchLattice = as.matrix(vegdist(patchLattice[[1]],method = "bray",diag=T,upper=T))
betaPatchGeograph = as.matrix(vegdist(patchGeograph[[1]],method = "bray",diag=T,upper=T))
betaPatchGeotree = as.matrix(vegdist(patchGeotree[[1]],method = "bray",diag=T,upper=T))

###############
# Neutral dynamics
# Specific parameters
m = 0.2
M = 0.01
k = 0.1
J = 100
sdN = Inf
sdE = 5
nsteps = 1000

# Simulations
neutralConnected = lottery_model(m,M,k,S,J,sdE,sdN,connected,nsteps)
neutralLattice = lottery_model(m,M,k,S,J,sdE,sdN,lattice,nsteps)
neutralGeograph = lottery_model(m,M,k,S,J,sdE,sdN,geograph,nsteps)
neutralGeotree = lottery_model(m,M,k,S,J,sdE,sdN,geotree,nsteps)

# Alpha diversity
alphaNeutralConnected = alpha_fn(neutralConnected[[1]])
alphaNeutralLattice = alpha_fn(neutralLattice[[1]])
alphaNeutralGeograph = alpha_fn(neutralGeograph[[1]])
alphaNeutralGeotree = alpha_fn(neutralGeotree[[1]])

# Beta diversity
betaNeutralConnected = as.matrix(vegdist(neutralConnected[[1]],method = "bray",diag=T,upper=T))
betaNeutralLattice = as.matrix(vegdist(neutralLattice[[1]],method = "bray",diag=T,upper=T))
betaNeutralGeograph = as.matrix(vegdist(neutralGeograph[[1]],method = "bray",diag=T,upper=T))
betaNeutralGeotree = as.matrix(vegdist(neutralGeotree[[1]],method = "bray",diag=T,upper=T))

###############
# Species sorting dynamics
# Specific parameters
m = 0.2
M = 0.01
k = 0.1
J = 100
sdN = 15
sdE = 5
nsteps = 1000

# Simulations
ssConnected = lottery_model(m,M,k,S,J,sdE,sdN,connected,nsteps)
ssLattice = lottery_model(m,M,k,S,J,sdE,sdN,lattice,nsteps)
ssGeograph = lottery_model(m,M,k,S,J,sdE,sdN,geograph,nsteps)
ssGeotree = lottery_model(m,M,k,S,J,sdE,sdN,geotree,nsteps)

# Alpha diversity
alphaSSConnected = alpha_fn(ssConnected[[1]])
alphaSSLattice = alpha_fn(ssLattice[[1]])
alphaSSGeograph = alpha_fn(ssGeograph[[1]])
alphaSSGeotree = alpha_fn(ssGeotree[[1]])

# Beta diversity
betaSSConnected = as.matrix(vegdist(ssConnected[[1]],method = "bray",diag=T,upper=T))
betaSSLattice = as.matrix(vegdist(ssLattice[[1]],method = "bray",diag=T,upper=T))
betaSSGeograph = as.matrix(vegdist(ssGeograph[[1]],method = "bray",diag=T,upper=T))
betaSSGeotree = as.matrix(vegdist(ssGeotree[[1]],method = "bray",diag=T,upper=T))

###############################
# Figure 1
quartz(height = 6.5, width = 6.5/4)
t = matrix(c(1:4),nr = 4, nc = 1, byrow = T)
layout(t)
layout.show(4)

#plot_alpha(connected,alphaPatchConnected)
plot_alpha(connected,alphaNeutralConnected)
#plot_alpha(connected,alphaSSConnected)

#plot_alpha(lattice,alphaPatchLattice)
plot_alpha(lattice,alphaNeutralLattice)
#plot_alpha(lattice,alphaSSLattice)

#plot_alpha(geograph,alphaPatchGeograph)
plot_alpha(geograph,alphaNeutralGeograph)
#plot_alpha(geograph,alphaSSGeograph)

#plot_alpha(geotree,alphaPatchGeotree)
plot_alpha(geotree,alphaNeutralGeotree)
#plot_alpha(geotree,alphaSSGeotree)

dev.copy2pdf(file = "Networks.pdf")

###############################
# Figure 2
quartz(height = 6, width = 6)
t = matrix(c(1:4),nr = 2, nc = 2, byrow = T)
layout(t)
layout.show(4)

apply(patchGeograph[[1]],2,sum)

maxPatchGeograph = max(apply(patchGeograph[[1]],1,sum))
maxNeutralGeograph = max(apply(neutralGeograph[[1]],1,sum))
maxSSGeograph = max(apply(ssGeograph[[1]],1,sum))

maxPatchGeotree = max(apply(patchGeotree[[1]],1,sum))
maxNeutralGeotree = max(apply(neutralGeotree[[1]],1,sum))
maxSSGeotree = max(apply(ssGeotree[[1]],1,sum))

par(mar=c(3,6,4,1))
plot(degGeograph,apply(patchGeograph[[1]],1,sum)/maxPatchGeograph,pch = 19, xlab = "", ylab = "Species richness 
(scaled)", ylim = c(0,1),cex.lab = 1.5, cex.axis = 1.25)
points(degGeograph,apply(neutralGeograph[[1]],1,sum)/maxNeutralGeograph,pch = 21, bg = "red")
points(degGeograph,apply(ssGeograph[[1]],1,sum)/maxSSGeograph,pch = 21, bg = "blue")

par(mar=c(3,4,4,3))
plot(eigGeograph,apply(patchGeograph[[1]],1,sum)/maxPatchGeograph,pch = 19, xlab = "", ylab = "", ylim = c(0,1),cex.lab = 1.5, cex.axis = 1.25)
points(eigGeograph,apply(neutralGeograph[[1]],1,sum)/maxNeutralGeograph,pch = 21, bg = "red")
points(eigGeograph,apply(ssGeotree[[1]],1,sum)/maxSSGeograph,pch = 21, bg = "blue")

par(mar=c(6,6,1,1))
plot(degGeotree,apply(patchGeotree[[1]],1,sum)/maxPatchGeotree,pch = 19, xlab = "Degree centrality", ylab = "Species richness
(scaled)", ylim = c(0,1),cex.lab = 1.5, cex.axis = 1.25)
points(degGeotree,apply(neutralGeotree[[1]],1,sum)/maxNeutralGeotree,pch = 21, bg = "red")
points(degGeotree,apply(ssGeotree[[1]],1,sum)/maxSSGeotree,pch = 21, bg = "blue")

par(mar=c(6,4,1,3))
plot(eigGeotree,apply(patchGeotree[[1]],1,sum)/maxPatchGeotree,pch = 19, xlab = "Eigen centrality", ylab = "", ylim = c(0,1),cex.lab = 1.5, cex.axis = 1.25)
points(eigGeotree,apply(neutralGeotree[[1]],1,sum)/maxNeutralGeotree,pch = 21, bg = "red")
points(eigGeotree,apply(ssGeotree[[1]],1,sum)/maxSSGeotree,pch = 21, bg = "blue")

dev.copy2pdf(file = "Centrality.pdf")

###############################
# Figure 3

quartz(height = 4.5, width = 6)
t = matrix(c(1:12),nr = 3, nc = 3, byrow = T)
layout(t)
layout.show(9)

par(mar=c(1,1,1,1))
plot(distLattice,betaPatchLattice,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(distGeograph,betaPatchGeograph,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(distGeotree,betaPatchGeotree,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)

plot(distLattice,betaNeutralLattice,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(distGeograph,betaNeutralGeograph,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(distGeotree,betaNeutralGeotree,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)

plot(distLattice,betaSSLattice,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(distGeograph,betaSSGeograph,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(distGeotree,betaSSGeotree,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)

dev.copy2pdf(file = "BetaGeoDist.pdf")

###############################
# Figure 4

quartz(height = 4.5, width = 6)
t = matrix(c(1:12),nr = 3, nc = 3, byrow = T)
layout(t)
layout.show(9)

par(mar=c(1,1,1,1))
plot(topoLattice,betaPatchLattice,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(topoGeograph,betaPatchGeograph,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(topoGeotree,betaPatchGeotree,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)

plot(topoLattice,betaNeutralLattice,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(topoGeograph,betaNeutralGeograph,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(topoGeotree,betaNeutralGeotree,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)

plot(topoLattice,betaSSLattice,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(topoGeograph,betaSSGeograph,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)
plot(topoGeotree,betaSSGeotree,pch = 19,labels = F,xlab = "",ylab = "",cex = 0.8)

dev.copy2pdf(file = "BetaTopoDist.pdf")


