# This R script is written to complete a simulation study using simulations 
# of localization functions output and saved as a .RData file from the 
# 'LocSimulations.R' script in this repository.
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Clear existing workspace
rm(list=ls())
# Add desired filename of data simulated LocSimulations.R
load('LocSimulationData_10per.RData') 
# Assign random number table in R
set.seed(185)

# Functions

purity <- function(y,yprime){
  require(caret)
  
  # Generate confusion matrix
  cm <- confusionMatrix(as.factor(yprime),
                        as.factor(y))
  
  # Find mode of each cluster
  maxs <- apply(cm$table,MARGIN=2,FUN=max)
  
  # Compute cluster similarity statistics
  out <- sum(maxs/(length(y)))
  return(out)
}

externalEval <- function(clus.mat,k,gindex,cmtype=2,shuffle=T){
  require(caret)
  require(cluster)
  require(aricode)
  require(clusteval)
  
  # Shuffle data
  if (shuffle==1){
    lsiz <- nrow(clus.mat)/k
    shuffi <- sample(nrow(clus.mat))
    gindex <- gindex[shuffi]
    clus.mat <- clus.mat[shuffi,]
  }
  
  # Perform PAM algorithm
  pm <- pam(clus.mat,k)
  
  # Get confusion matrix object
  cm <- confusionMatrix(as.factor(pm$clustering),as.factor(gindex))
  
  # Find mode of each cluster
  maxs <- apply(cm$table,MARGIN=2,FUN=max)
  
  # Compute cluster similarity statistics
  pure <- purity(gindex,pm$clustering)
  cat(paste0('Clustering purity:\n',pure,'\n\n'))
  simil <- cluster_similarity(gindex,pm$clustering,similarity='rand')
  cat(paste0('Clustering similarity:\n',simil,'\n\n'))
  nmi <- NMI(gindex,pm$clustering,variant='sqrt')
  cat(paste0('Normalized mutual information:\n',nmi,'\n\n'))
  randi <- RI(gindex,pm$clustering)
  cat(paste0('Rand Index:\n',randi,'\n\n'))
  
  # Print confusion matrix
  if (cmtype == 1){ # Preserve rows
    row.index <- apply(cm$table,MARGIN=2,FUN=which.max)
    print(cm$table[,as.numeric(names(sort(row.index)))])
  } else if (cmtype == 2){ # Preserve columns
    col.index <- apply(cm$table,MARGIN=1,FUN=which.max)
    print(cm$table[as.numeric(names(sort(col.index))),])
  }
  
  out<- data.frame(purity=pure,
                   similarity=simil,
                   nmi=nmi,
                   rand.index=randi)
  return(out)
}

# Clustering
n.loc.conds <- 20
gindex <- sort(rep(1:n.loc.conds,nrow(n5)/n.loc.conds))

externalEval(n5, # 5 observations per response angle
            n.loc.conds, # Number of groups
            gindex, # True groups
            cmtype=2) # Reorganize confusion matrix by rows
externalEval(n10, # 10 observations per response angle
            n.loc.conds, # Number of groups
            gindex, # True groups
            cmtype=2) # Reorganize confusion matrix by rows
externalEval(n25, # 5 observations per response angle
            n.loc.conds, # Number of groups
            gindex, # True groups
            cmtype=2) # Reorganize confusion matrix by rows

# --------------------------------------------------------------------
rm(list=ls())
load('LocSimulationData_100per.RData')
#load('GapStats_10per.RData')
set.seed(185)

library(cluster)

# Functions

purity <- function(y,yprime){
  require(caret)
  
  # Generate confusion matrix
  cm <- confusionMatrix(as.factor(yprime),
                        as.factor(y))
  
  # Find mode of each cluster
  maxs <- apply(cm$table,MARGIN=2,FUN=max)
  
  # Compute cluster similarity statistics
  out <- sum(maxs/(length(y)))
  return(out)
}

externalEval <- function(clus.mat,k,gindex,cmtype=2,shuffle=T){
  require(caret)
  require(cluster)
  require(aricode)
  require(clusteval)
  
  # Shuffle data
  if (shuffle==1){
    lsiz <- nrow(clus.mat)/k
    shuffi <- sample(nrow(clus.mat))
    gindex <- gindex[shuffi]
    clus.mat <- clus.mat[shuffi,]
  }
  
  # Perform PAM algorithm
  pm <- pam(clus.mat,k)
  
  # Get confusion matrix object
  cm <- confusionMatrix(as.factor(pm$clustering),as.factor(gindex))
  
  # Find mode of each cluster
  maxs <- apply(cm$table,MARGIN=2,FUN=max)
  
  # Compute cluster similarity statistics
  pure <- purity(gindex,pm$clustering)
  cat(paste0('Clustering purity:\n',pure,'\n\n'))
  simil <- cluster_similarity(gindex,pm$clustering,similarity='rand')
  cat(paste0('Clustering similarity:\n',simil,'\n\n'))
  nmi <- NMI(gindex,pm$clustering,variant='sqrt')
  cat(paste0('Normalized mutual information:\n',nmi,'\n\n'))
  randi <- RI(gindex,pm$clustering)
  cat(paste0('Rand Index:\n',randi,'\n\n'))
  
  # Print confusion matrix
  if (cmtype == 1){ # Preserve rows
    row.index <- apply(cm$table,MARGIN=2,FUN=which.max)
    print(cm$table[,as.numeric(names(sort(row.index)))])
  } else if (cmtype == 2){ # Preserve columns
    col.index <- apply(cm$table,MARGIN=1,FUN=which.max)
    print(cm$table[as.numeric(names(sort(col.index))),])
  }
  
  out<- data.frame(purity=pure,
                   similarity=simil,
                   nmi=nmi,
                   rand.index=randi)
  return(out)
}

# Clustering
n.loc.conds <- 20
gindex <- sort(rep(1:n.loc.conds,nrow(n5)/n.loc.conds))

externalEval(n5, # 5 observations per response angle
            n.loc.conds, # Number of groups
            gindex, # True groups
            cmtype=2) # Reorganize confusion matrix by rows
externalEval(n10, # 10 observations per response angle
            n.loc.conds, # Number of groups
            gindex, # True groups
            cmtype=2) # Reorganize confusion matrix by rows
externalEval(n25, # 5 observations per response angle
            n.loc.conds, # Number of groups
            gindex, # True groups
            cmtype=2) # Reorganize confusion matrix by rows


# Check for optimal number of gaps
# Function to compute gap statistic iteratively
gap.pam = function(x, k){list(cluster = pam(x,k, cluster.only=TRUE))}

# Compute gaps and bootstrap with 1,000 samples
par(mfrow = c(1,3))
gaps.n5 <- clusGap(n5,
                FUN=gap.pam,
                K.max=25,
                B=1000)
plot(gaps.n5,
     xlab = 'Number of Groups',
     main = 'n = 5, Goodness of Cluster')
abline(v = 20, col = 'red')

gaps.n10 <- clusGap(n10,
                FUN=gap.pam,
                K.max=25,
                B=1000)
plot(gaps.n10,
     xlab = 'Number of Groups',
     main = 'n = 10, Goodness of Cluster')
abline(v = 20, col = 'red')

gaps.n25 <- clusGap(n25,
                FUN=gap.pam,
                K.max=25,
                B=1000)
plot(gaps.n25,
     xlab = 'Number of Groups',
     main = 'n = 25, Goodness of Cluster')
abline(v = 20, col = 'red')


save(gaps.n5,gaps.n10,gaps.n25,file = 'GapStats_10per.RData')
