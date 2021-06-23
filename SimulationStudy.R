# This R script is written to replicate the simulation study included in 
# the manuscript associated with this repository. It can be modified to 
# test additional parameterizations of localization categories or 
# data.
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Start timer 
now <- Sys.time()

# Simulation specifications
nsims <- 2000                   # Number of simulation iterations
n.loc.conds <- 20               # Number of possible categories
n <- c(5, 10, 15, 20, 50, 100)  # Repetitions per target angle
nsubs <- c(5, 10, 20, 50, 100)  # Number of simulated subjects

# Initialize data frames for output
v <- vector(length = (length(n) * length(nsubs)))
sum.df <- data.frame(n = v,
                     nsubs = v,
                     purity = v,
                     nmi = v,
                     rand.index = v,
                     alg = v)
km.df <- data.frame(n = v,
                     nsubs = v,
                     purity = v,
                     nmi = v,
                     rand.index = v,
                     alg = v)

# Initialize loop index
m <- 0  
for (i in 1:nsims){
  for (j in 1:length(n)){
    for (k in 1:length(nsubs)){
      # Increase loop index
      m <- m + 1
    
      # Simulate data from all possible categories
      clus.mat <- simLocData(n[j], nsubs[k])
      # Create vector of group numbers based on shuffling
      gindex <- sort( rep( 1 : n.loc.conds, nrow(clus.mat) / n.loc.conds))
    
      # Randomly shuffle rows in data frame
      shuffi <- sample( nrow( clus.mat))
      clus.mat <- clus.mat[shuffi,]
      # Update group numbers
      gindex <- gindex[shuffi]
    
      # Add new data to summary data frame
      sum.df[m,1] <- n[j]
      sum.df[m,2] <- nsubs[k]
      sum.df[m,3:6] <- externalEval(clus.mat,
              n.loc.conds,
              gindex,
              cmtype=2,
              alg='PAM')
      
      # Add new data to comparison data frame using alternative, k-means algorithm
      km.df[m,1] <- n[j]
      km.df[m,2] <- nsubs[k]
      km.df[m,3:6] <- externalEval(clus.mat,
              n.loc.conds,
              gindex,
              cmtype=2,
              alg='kmeans')
    }
  }
}

# Take mean over simulations
sum.mean <- aggregate(. ~ n + nsubs + alg, FUN = mean, data = sum.df)
# Rename columns to reflect mean
names(sum.mean) <- c('n','nsubs','alg','purity.mean','nmi.mean','rand.index.mean')
# Take SD over simulations
sum.sd <- aggregate(. ~ n + nsubs + alg, FUN = sd, data = sum.df)
# Rename columns to reflect mean
names(sum.sd) <- c('n','nsubs','alg','purity.sd','nmi.sd','rand.index.sd')

# Same steps for alternative k-means algorithm
km.mean <- aggregate(. ~ n + nsubs + alg, FUN = mean, data = km.df)
names(km.mean) <- c('n','nsubs','alg','purity.mean','nmi.mean','rand.index.mean')
km.sd <- aggregate(. ~ n + nsubs + alg, FUN = sd, data = km.df)
names(km.sd) <- c('n','nsubs','alg','purity.sd','nmi.sd','rand.index.sd')

# Assemble mean and SD in same data frame
sum.out <- merge(sum.mean,sum.sd,by=c('n','nsubs','alg'))
km.out <- merge(km.mean,km.sd,by=c('n','nsubs','alg'))
out <- rbind(sum.out,km.out)

# Write out summary CSV file
write.csv(out, file = 'simulationStudyCIs.csv', 
          col.names = T, row.names = F)

# Print runtime for code chunk
difftime(Sys.time(),now)
