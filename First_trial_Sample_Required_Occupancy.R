
### simulations for sites and number of replicates for simple occupancy
library(wiqid)

# Biological real values
psi <- 0.4 # real occupancy of the study area
p <- 0.1 # real detection probability 

# Sample size and replicates
N <- 100 # sites visits
k <- 20 # no of times each site is visited

# creating pockets for Detection history
DH <- matrix(nrow = N, ncol=k)

# Generating random number of occupied sites
(occ <- rbinom(N,1, psi))

iter <- 1000 # no. of simulations we want to generate
# creating pockets to keep our estimates of occupancy and detection probability
estimate.OCC <- numeric(iter)
estimate.DETC <- numeric(iter)

# Creating detection/non-detection in occupied sites with the iterations = 1000
for(l in 1:iter){
(occ <- rbinom(N,1, psi))
for(i in 1:N){
if(occ[i] == 0){
DH[i,] <- 0
}
else{
for(j in 1:k){
DH[i,j] <-rbinom(1,1, p)
}}}
y <- rowSums(DH, na.rm=TRUE)  # detection per site
n <- rowSums(!is.na(DH))      # no of replicates per site
estimate <- occSS0(y,n)
estimate.OCC[l] <- estimate$real[1,1]
estimate.DETC[l] <- estimate$real[2,1]
}

#  Checking the mean occupancy and detection probability from the simulations
mean(estimate.OCC)
mean(estimate.DETC)

# proportion of estimates that differ the true value (occupancy by 0.1 and detection by 0.05)
mean(abs(estimate.OCC - psi) > 0.1)
mean(abs(estimate.DETC - p) > 0.05)

# plotting the estimates
plotPost(estimate.OCC)
plotPost(estimate.OCC, showCurve=TRUE)
plotPost(estimate.DETC, showCurve=TRUE)


#looking at the beeswarm

library(beeswarm)

beeswarm(estimate.OCC)
beeswarm(estimate.DETC)


# pretty beeswarm

beeswarm(estimate.OCC, method='hex', cex=0.5, col='blue', las=1,
  xlab="Sample size", ylab="Estimated occupancy")

# Add a horizontal line with the true value:
abline(h = psi, col='red', lwd=2)


beeswarm(estimate.DETC, method='hex', cex=0.5, col='blue', las=1,
  xlab="Sample size", ylab="Estimated detection probabilities")

# Add a horizontal line with the true value:
abline(h = p, col='red', lwd=2)
