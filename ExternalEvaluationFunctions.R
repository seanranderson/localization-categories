# This R script provides functions to perform external evaluation of 
# clustering performance in the simulation study completed within 
# 'SimClustering.R'.
# 
# Sean R. Anderson - sean.hearing@gmail.com

randSimilarity <- function(y,yprime,vectorized=T){
  # y is defined groups
  # yprime is algorithmic clustering
  
  k <- length(unique(y))
  if (vectorized == F){
    for (i in 1:k){
      for (j in 1:k){
        n <- matrix(nrow=k,ncol=k)
        n[i,j] <- sum(y[yprime==j]==i)
      }
    }
  } else {
    s <- split(y,yprime)
    n <- matrix(unlist(lapply(s,
                              FUN=function(x,k){
                                apply(matrix(sort(rep(1:k,length(x))),
                                             nrow=length(x),
                                             ncol=k)==x,
                                      MARGIN=2,
                                      FUN=sum)},
                              k=20),use.names=F),
                nrow=k,
                ncol=k)
  }
  out <- (ncol(combn(length(y),2)) - 
            (0.5*(sum(apply(n,MARGIN=2,FUN=sum)^2) + 
                    sum(apply(n,MARGIN=1,FUN=sum)^2)) -
               sum(n^2)))/ncol(combn(length(y),2))
  return(out)
}

normalizedMutualInf <- function(y,yprime){
  require(caret)
  # Same as sqrt variant of NMI function
    # can quickly confirm using entropy package - entropy and mi.plugin function
  
  pk <- tabulate(yprime)/length(yprime)
  pclass <- tabulate(y)/length(y)
  cm <- confusionMatrix(as.factor(yprime),as.factor(y))
  pintersection <- cm$table/length(y)
  
  n <- pintersection * log(pintersection / 
                             (t(matrix(pk,
                                       nrow=length(pk),
                                       ncol=length(pk),
                                       byrow=T)) * 
                                matrix(pclass,
                                       nrow=length(pclass),
                                       ncol=length(pclass),
                                       byrow=T)))
  n[is.na(n)] <- 0
  d <- ((-sum(pk * log(pk))) + (-sum(pclass * log(pclass))))/2
  
  out <- sum(n)/d
  return(out)
}

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
