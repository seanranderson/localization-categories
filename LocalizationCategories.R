# This R script is written to replicate the analysis included in the 
# manuscript associated with this repository. The code can be modified 
# to test different parameterizations of categories if desired. For 
# an example dataset, see [file to be uploaded following IRB approval].
# This script requires functions defined in 'HelperFunctions.R' to be 
# in the workspace.
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Start timer
now <- Sys.time()

# Number of simulated datasets compared against each 
# subject in experimental data
nrep <- 50

# Subject IDs in dataset
SubIDs <- unique( raw.dat$subject)

# Assigns categories from Fig. S.2, defined in simLocData, 
# to subjects based on their response means and SDs at 
# each target angle. Category assignment is based on the 
# mode of simulated subjects' categories that are 
# classified into the same cluster. Process is completed 
# iteratively for each subject and repeated nrep times 
# with newly simulated data.
classAssignment <- function(dat, nrep = 30, n.sim.cases){
  # Input:
  # dat - clutering data frame 
  # rows are subjects 
  # columns are 'features'
  # i.e., mean/SD response angles for each target angle
  # nrep - number of iterations for which classification 
  # is performed for each subject.
  # Each iteration is completed using newly simulated data 
  # from each category.
  # n.sim.cases - number of simulated subjects per category 
  # based on categories in Fig. S2
  
  # Output: list containing the following elements:
  # class.dist - data frame containing: 
  # mode of clustering for each subject 
  # proportion of time that mode occurred
  # prop = # mode / nrep
  # class.df - list whose length = nrep 
  # made of data frames 
  # each row corresponds to a different subject
  # Variable 'class' in each data frame indicates 
  # the category to which that subject was assigned 
  # during the jth iteration.
  
  # Initialize list
  class.df <- list(length = nrep)
  for (j in 1:nrep){
    # Initialize df in jth element of list
    class.df[[j]] <- data.frame(class = vector(length = nrow(dat)),
                                purity = vector(length = nrow(dat)),
                                n = vector(length = nrow(dat)))
    # Simulate data from all possible categories
    # First argument = repetitions per target speaker
    sim.dat <- simLocData(15, n.sim.cases)
    
    for (i in 1:nrow(dat)){
      # Bind data frames from list into large data frame
      clus.mat <- rbind(sim.dat, dat[i,])
      # Create vector of group numbers based on shuffling
      gindex <- c( sort( rep( 1:20, nrow(sim.dat) / 20)), NA)
      # Randomly shuffle rows in data frame
      shuffi <- sample( nrow( clus.mat))
      clus.mat <- clus.mat[shuffi,]
      # Update group numbers
      gindex <- gindex[shuffi]
      # Find case where gindex == NA
      # This indicates data from experiment
      item.index <- match(NA, gindex)
      
      # Perform PAM
      # k = 20 since there are 20 simulated categories
      pm <- pam(clus.mat,20)
      # Store the category from each observation
      # Takes the mode of the cluster to which 
      # the ith subject was assigned.
      # 'table' calculates number of elements with 
      # a particular value.
      # Values input to table = category to which 
      # simulated data in same cluster are assigned.
      # Ordered from greatest to least
      # First element reflects greatest number of obs
      class.df[[j]]$class[i] <- as.numeric( names( sort(
        # Only data from experiment
        # item.index is logical indicating observation was 
        # from experiment.
        table( gindex[pm$clustering == pm$clustering[item.index]]),
        decreasing = T))[1])
      
      # Find the size of each cluster
      class.df[[j]]$n[i] <- length( gindex[
        pm$clustering == pm$clustering[item.index]])
      # Compute the purity based on simulated data
      class.df[[j]]$purity[i] <- max( table( gindex[
        pm$clustering == pm$clustering[item.index]])) / (class.df[[j]]$n[i] - 1)
    }
  }
  
  # Initialize data frame
  class.dist <- data.frame(class = vector(length = nrow(dat)),
                           prop = vector(length = nrow(dat)))
  
  # Extract category assignments for each subject during each iteration in nrep
  mat <- matrix( unlist( lapply( class.df,
                                 FUN = function(x){ x$class })), nrow = nrow(dat), ncol = nrep)
  # Take mode for each subject over nrep iterations
  class.dist$class <- as.numeric( apply(mat, MARGIN = 1,
                                        FUN=function(x){ names( sort( table(x), decreasing = T)[1])}))
  # Calculate proportion of time subject assigned to mode
  class.dist$prop <- apply(mat, MARGIN = 1,
                           FUN=function(x){ max( table(x)) / length(x)})
  
  # Calculate Krippendorf's alpha
  # Measure of reliability of classification
  library(irr)
  print( kripp.alpha( t(mat), method = 'nominal'))
  
  # Return list 
  return(list(class.dist, class.df))
}

ls <- classAssignment(dat, nrep, 50)

# Show example rows from output summary
head(ls[[1]])

# Write summary CSV file
write.csv( t( setNames( as.data.frame( matrix( unlist( lapply(
  ls[[2]], function(x) {x$class})), 48, 50),
    # Subject
    row.names = as.character( SubIDs)), 
  # Rep number
  as.character(1:nrep))),
  file = 'confusionsBySub.csv', col.names = T, row.names = T)

# Print runtime for code chunk
difftime(Sys.time(),now)
