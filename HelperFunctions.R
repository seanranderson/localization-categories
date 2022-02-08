# This R script provides functions used within each of the scripts in 
# this repository. In order to use the other scripts, these functions 
# must be added to the workspace in R.
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Function to simulate localization data from predefined function
simLoc <- function(angle, rmeans, rsds, n, lsiz){
  library(truncnorm)
  # Input:
  # angle - vector of target angles
  # rmeans - vector of response means at each target angle
  # rsds - vector of response SDs at each target angle
  # n - number of responses per target speaker
  # lsiz - number of simulated subjects per category
  # also indicates size as output list
  
  # Output: data frame of means and SDs at each target angle
  
  # Generate data from truncated normal distribution
  x <- as.data.frame( mapply( rtruncnorm,
                              mean = rmeans, 
                              sd = rsds,
                              n = n*lsiz,
                              a = -90, b = 90))
  
  # Name each column according to mean
  names(x) <- as.character(angle)
  
  # Split data into lists
  f <- sort( rep( seq(1,lsiz), n))
  l <- split(x,f)
  return(l)
}

# Second lapply function
  # Allows for lapply over lapply
  # Built for and tested with data frames
dubApply <- function(dat,fu, ...){
  # Input: 
  # dat - list
  # fu - function
  
  # Output: data object with function fu applied over list within list
  
  FUN <- match.fun(fu)
  if (!is.vector(dat) || is.object(dat)) 
    dat <- al.list(dat)
  .Internal( lapply( dat, fu))
}

# Take mean and SD of each column
colMeanSd <- function(df){
  # Input: df - data frame
  
  # Output: data frame of means and SDs from each column of input data frame
  
  # Calculate means and SDs from each column
  m <- apply(df, MARGIN = 2, FUN = mean)
  s <- apply(df, MARGIN = 2, FUN = sd)
  
  # Concatenate columns
  out <- as.data.frame( c(m,s))
  return(out)
}

# Bind all columns of data frames in a list
combListDf <- function(l){
  # Input: l - list of data frames
  
  # Output: large data frame concatenated from list
  
  out <- do.call('cbind', l)
  return(out)
}

# Returns matrix that can be input to clustering algorithms
clusFormat <- function(dat){
  # Input: dat - raw responses from participant/simulation
  
  # Output: matrix where rows are observations/subjects, 
  # cols are features
  
  # Take means and SDs of each column
  l <- lapply(dat, FUN = dubApply, fu=colMeanSd)
  
  # Combine means and SDs from each sample
  l <- lapply(l, FUN = combListDf)
  
  # Transpose matrices
  out <- lapply(l, FUN = t)
  return(out)
}

# Calculate purity of cluster
purity <- function(y, yprime){
  # Input:
  # y - true clustering
  # yprime - algorithmic clustering
    
  # Output: purity bounded [0,1]

  # Load required library
  library(caret)
  
  # Generate confusion matrix
  cm <- confusionMatrix( as.factor( yprime),
                         as.factor( y ))
  
  # Find mode of each cluster
  maxs <- apply(cm$table, MARGIN = 2, FUN = max)
  
  # Compute cluster similarity statistics
  out <- sum(maxs / (length(y)))
  return(out)
}

# Computes different external evaluation metrics.
# Presents results in a summary table. 
externalEval <- function(
  clus.mat, k, gindex, cmtype = 2, print.out = F, alg = 'PAM'){
  # Input:
  # clus.mat - clustering matrix
    # rows are subjects
    # columns are features (means and SDs)
  # k - number of clusters
  # gindex - true clustering vector
  # cmtype - type of confusion matrix to be used
  # print.out - logical - whether to print summary table
  # alg - clustering algorithm to be used
    # current options are 'PAM' or 'kmeans'
    
  # Output: data frame of external evaluation criteria
  
  # Load dependencies
  library(caret)
  library(aricode)

  # Perform clustering
    # Other unsupervised machine learning algorithms 
    # could be implemented here by user
  if (alg == 'PAM'){ # Default: PAM
    pm <- pam(clus.mat, k)
    # vector indicating to which cluster each subject belongs
    clustering <- pm$clustering
  } else if (alg == 'kmeans'){ # Alternative: k-means
    km <- kmeans(clus.mat, k)
    # vector indicating to which cluster each subject belongs
    clustering <- km$cluster
  }
  
  # Get confusion matrix object
  cm <- confusionMatrix( as.factor( clustering),
                         as.factor( gindex))
  
  # Compute cluster similarity statistics
  pure <- purity(gindex, clustering)
  nmi <- NMI(gindex, clustering, variant = 'sqrt')
  randi <- RI(gindex, clustering)
  
  if (print.out){
    cat( paste0( 'Clustering purity:\n', 
                 pure, '\n\n'))
    cat( paste0( 'Normalized mutual information:\n', 
                 nmi, '\n\n'))
    cat( paste0( 'Rand Index:\n', 
                 randi, '\n\n'))
  }
  
  # Print confusion matrix
  if (cmtype == 1 && print.out == T){ # Preserve rows
    row.index <- apply(cm$table, MARGIN = 2, FUN = which.max)
    print(cm$table[ ,as.numeric( names( sort( row.index)))])
  } else if (cmtype == 2 && print.out == T){ # Preserve columns
    col.index <- apply(cm$table, MARGIN = 1, FUN = which.max)
    print(cm$table[as.numeric( names( sort( col.index))),])
  }
  
  # Create data frame of external evaluation criteria
    # stringsAsFactors must be set to FALSE to prevent coercing
    # alg to a numeric format
  out <- data.frame(purity = pure,
                   nmi = nmi,
                   rand.index = randi,
                   alg = alg,
                   stringsAsFactors = F)
  return(out)
}
