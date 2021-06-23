rm(list=ls())
library(truncnorm)

# <<<<<<<<<<<<<<<<<<<<<<<<<<< Functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

simLoc <- function(angle,rmeans,rsds,n,lsiz){
  require(truncnorm)
  # Generate data
  x <- as.data.frame(mapply(rtruncnorm,
              mean=rmeans,sd=rsds,n=n*lsiz,
              a = -90, b = 90))
  
  # Name each column by mean
  names(x) <- as.character(angle)
  
  # Split data into lists for samples
  f <- sort(rep(seq(1,lsiz),n))
  l <- split(x,f)
  return(l)
  
}

dubApply <- function(dat,fu, ...){
  # Second lapply function
    # Allows for lapply over lapply
  
  FUN <- match.fun(fu)
  if (!is.vector(dat) || is.object(dat)) 
    dat <- al.list(dat)
  .Internal(lapply(dat, fu))

}

colMeanSd <- function(df){
  
  # Take mean and SD of each column
  m <- apply(df, MARGIN = 2, FUN = mean)
  s <- apply(df, MARGIN = 2, FUN = sd)
  
  # Concatenate columns
  out <- as.data.frame(c(m,s))
  return(out)

}

combListDf <- function(l){
  
  # Bind all columns of data frames in a list
  out <- do.call('cbind',l)
  return(out)
  
}

clusFormat <- function(dat){
  # Returns matrix that can be input to clustering algorithms
  # Format: each row is unique observation, each column is variable
  
  # Take means and SDs of each column
  l <- lapply(dat, FUN=dubApply, fu=colMeanSd)
  
  # Combine means and SDs from each sample
  l <- lapply(l, FUN=combListDf)
  
  # Transpose matrices
  out <- lapply(l, FUN=t)
  return(out)
  
}

locPlot <- function(x,dat,main = ''){
  
  xl <- 'Angle'
  yl <- 'Response'
  
  plot(sort(rep(x,nrow(dat))),dat[,1:length(x)],
       xlab = xl, ylab = yl, main = main,
       xlim = c(-90,90), ylim = c(-90,90))
  
}

plotLoc <- function(x,y,SDs = 0,tstr = ''){
  plot(x,y,
       xlim = c(-90,90),
       ylim = c(-90,90),
       xlab = 'Target Angle',
       ylab = 'Response Angle',
       main = tstr)
  do.call(arrows,
          c(list(x, y + SDs, x, y - SDs,
                 code = 0)))
  abline(a = 0, b = 1, col = 'red')
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<< Simulations >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

set.seed(427)
n <- c(5,10,25) # samples per angle
lsiz <- 100 # samples per performance type

x <- seq(-90,90,10)
SDs.eq <- rep(10, length(x))
SDs.un <- c(30,30,30,20,20,20,10,10,10,
            5,10,10,10,20,20,20,30,30,30)
SDs.al <- c(50,50,50,40,40,40,30,30,30,
            20,10,10,10,20,20,20,30,30,30)

# ------------ Generate Data ------------ 

# Ideal performance
y <- SSfpl(x, -180, 180, 0, 85)
dat.ideal.eq <- lapply(n,FUN=simLoc,angle=x,
          rmeans=y,rsds=SDs.eq,lsiz=lsiz)
ideal.eq <- clusFormat(dat.ideal.eq)
dat.ideal.un <- lapply(n,FUN=simLoc,angle=x,
          rmeans=y,rsds=SDs.un,lsiz=lsiz)
ideal.un <- clusFormat(dat.ideal.un)
dat.ideal.al <- lapply(n,FUN=simLoc,angle=x,
          rmeans=y,rsds=SDs.al,lsiz=lsiz)
ideal.al <- clusFormat(dat.ideal.al)
dat.ideal.ar <- lapply(n,FUN=simLoc,angle=x,
                        rmeans=y,rsds=rev(SDs.al),lsiz=lsiz)
ideal.ar <- clusFormat(dat.ideal.ar)

# Asymmetric - Left
y <- SSfpl(x, -50, 150, 55, 50)
dat.asyml.eq <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.eq,lsiz=lsiz)
asyml.eq <- clusFormat(dat.asyml.eq)
dat.asyml.un <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.un,lsiz=lsiz)
asyml.un <- clusFormat(dat.asyml.un)
dat.asyml.al <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.al,lsiz=lsiz)
asyml.al <- clusFormat(dat.asyml.al)
dat.asyml.ar <- lapply(n,FUN=simLoc,angle=x,
                         rmeans=y,rsds=rev(SDs.al),lsiz=lsiz)
asyml.ar <- clusFormat(dat.asyml.ar)

# Asymmetric - Right
y <- SSfpl(x, -150, 50, -55, 50)
dat.asymr.eq <- lapply(n,FUN=simLoc,angle=x,
                       rmeans=y,rsds=SDs.eq,lsiz=lsiz)
asymr.eq <- clusFormat(dat.asymr.eq)
dat.asymr.un <- lapply(n,FUN=simLoc,angle=x,
                       rmeans=y,rsds=SDs.un,lsiz=lsiz)
asymr.un <- clusFormat(dat.asymr.un)
dat.asymr.al <- lapply(n,FUN=simLoc,angle=x,
                        rmeans=y,rsds=SDs.al,lsiz=lsiz)
asymr.al <- clusFormat(dat.asymr.al)
dat.asymr.ar <- lapply(n,FUN=simLoc,angle=x,
                        rmeans=y,rsds=rev(SDs.al),lsiz=lsiz)
asymr.ar <- clusFormat(dat.asymr.ar)

# Compressed
y <- SSfpl(x, -70, 70, 0, 35)
dat.compr.eq <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.eq,lsiz=lsiz)
compr.eq <- clusFormat(dat.compr.eq)
dat.compr.un <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.un,lsiz=lsiz)
compr.un <- clusFormat(dat.compr.un)
dat.compr.al <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.al,lsiz=lsiz)
compr.al <- clusFormat(dat.compr.al)
dat.compr.ar <- lapply(n,FUN=simLoc,angle=x,
                        rmeans=y,rsds=rev(SDs.al),lsiz=lsiz)
compr.ar <- clusFormat(dat.compr.ar)

# Flat
y <- SSfpl(x, -15, 15, 0, 35)
dat.flatt.eq <- lapply(n,FUN=simLoc,angle=x,
                rmeans=y,rsds=SDs.eq,lsiz=lsiz)
flatt.eq <- clusFormat(dat.flatt.eq)
dat.flatt.un <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.un,lsiz=lsiz)
flatt.un <- clusFormat(dat.flatt.un)
dat.flatt.al <- lapply(n,FUN=simLoc,angle=x,
              rmeans=y,rsds=SDs.al,lsiz=lsiz)
flatt.al <- clusFormat(dat.flatt.al)
dat.flatt.ar <- lapply(n,FUN=simLoc,angle=x,
                        rmeans=y,rsds=rev(SDs.al),lsiz=lsiz)
flatt.ar <- clusFormat(dat.flatt.ar)

# ------------ Reformat Data ------------ 

n5 <- do.call('rbind',list(ideal.eq[[1]],ideal.un[[1]],ideal.al[[1]],ideal.ar[[1]],
                     asyml.eq[[1]],asyml.un[[1]],asyml.al[[1]],asyml.ar[[1]],
                     asymr.eq[[1]],asymr.un[[1]],asymr.al[[1]],asymr.ar[[1]],
                     compr.eq[[1]],compr.un[[1]],compr.al[[1]],compr.ar[[1]],
                     flatt.eq[[1]],flatt.un[[1]],flatt.al[[1]],flatt.ar[[1]]))

n10 <- do.call('rbind',list(ideal.eq[[2]],ideal.un[[2]],ideal.al[[2]],ideal.ar[[2]],
                            asyml.eq[[2]],asyml.un[[2]],asyml.al[[2]],asyml.ar[[2]],
                            asymr.eq[[2]],asymr.un[[2]],asymr.al[[2]],asymr.ar[[2]],
                            compr.eq[[2]],compr.un[[2]],compr.al[[2]],compr.ar[[2]],
                            flatt.eq[[2]],flatt.un[[2]],flatt.al[[2]],flatt.ar[[2]]))

n25 <- do.call('rbind',list(ideal.eq[[3]],ideal.un[[3]],ideal.al[[3]],ideal.ar[[3]],
                            asyml.eq[[3]],asyml.un[[3]],asyml.al[[3]],asyml.ar[[3]],
                            asymr.eq[[3]],asymr.un[[3]],asymr.al[[3]],asymr.ar[[3]],
                            compr.eq[[3]],compr.un[[3]],compr.al[[3]],compr.ar[[3]],
                            flatt.eq[[3]],flatt.un[[3]],flatt.al[[3]],flatt.ar[[3]]))

# ------------- Plot Data -------------

par(mfrow=c(5,4))

plotLoc(jitter(sort(rep(x,nrow(ideal.eq[[1]])))),ideal.eq[[1]][,1:19],ideal.eq[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(ideal.un[[1]])))),ideal.un[[1]][,1:19],ideal.un[[1]][,20:38],'n = 5')
plotLoc(jitter(sort(rep(x,nrow(ideal.al[[1]])))),ideal.al[[1]][,1:19],ideal.al[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(ideal.ar[[1]])))),ideal.ar[[1]][,1:19],ideal.ar[[1]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(asyml.eq[[1]])))),asyml.eq[[1]][,1:19],asyml.eq[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.un[[1]])))),asyml.un[[1]][,1:19],asyml.un[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.al[[1]])))),asyml.al[[1]][,1:19],asyml.al[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.ar[[1]])))),asyml.ar[[1]][,1:19],asyml.ar[[1]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(asymr.eq[[1]])))),asymr.eq[[1]][,1:19],asymr.eq[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.un[[1]])))),asymr.un[[1]][,1:19],asymr.un[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.al[[1]])))),asymr.al[[1]][,1:19],asymr.al[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.ar[[1]])))),asymr.ar[[1]][,1:19],asymr.ar[[1]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(compr.eq[[1]])))),compr.eq[[1]][,1:19],compr.eq[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.un[[1]])))),compr.un[[1]][,1:19],compr.un[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.al[[1]])))),compr.al[[1]][,1:19],compr.al[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.ar[[1]])))),compr.ar[[1]][,1:19],compr.ar[[1]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(flatt.eq[[1]])))),flatt.eq[[1]][,1:19],flatt.eq[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.un[[1]])))),flatt.un[[1]][,1:19],flatt.un[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.al[[1]])))),flatt.al[[1]][,1:19],flatt.al[[1]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.ar[[1]])))),flatt.ar[[1]][,1:19],flatt.ar[[1]][,20:38])



plotLoc(jitter(sort(rep(x,nrow(ideal.eq[[2]])))),ideal.eq[[2]][,1:19],ideal.eq[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(ideal.un[[2]])))),ideal.un[[2]][,1:19],ideal.un[[2]][,20:38],'n = 10')
plotLoc(jitter(sort(rep(x,nrow(ideal.al[[2]])))),ideal.al[[2]][,1:19],ideal.al[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(ideal.ar[[2]])))),ideal.ar[[2]][,1:19],ideal.ar[[2]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(asyml.eq[[2]])))),asyml.eq[[2]][,1:19],asyml.eq[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.un[[2]])))),asyml.un[[2]][,1:19],asyml.un[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.al[[2]])))),asyml.al[[2]][,1:19],asyml.al[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.ar[[2]])))),asyml.ar[[2]][,1:19],asyml.ar[[2]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(asymr.eq[[2]])))),asymr.eq[[2]][,1:19],asymr.eq[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.un[[2]])))),asymr.un[[2]][,1:19],asymr.un[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.al[[2]])))),asymr.al[[2]][,1:19],asymr.al[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.ar[[2]])))),asymr.ar[[2]][,1:19],asymr.ar[[2]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(compr.eq[[2]])))),compr.eq[[2]][,1:19],compr.eq[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.un[[2]])))),compr.un[[2]][,1:19],compr.un[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.al[[2]])))),compr.al[[2]][,1:19],compr.al[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.ar[[2]])))),compr.ar[[2]][,1:19],compr.ar[[2]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(flatt.eq[[2]])))),flatt.eq[[2]][,1:19],flatt.eq[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.un[[2]])))),flatt.un[[2]][,1:19],flatt.un[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.al[[2]])))),flatt.al[[2]][,1:19],flatt.al[[2]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.ar[[2]])))),flatt.ar[[2]][,1:19],flatt.ar[[2]][,20:38])




plotLoc(jitter(sort(rep(x,nrow(ideal.eq[[3]])))),ideal.eq[[3]][,1:19],ideal.eq[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(ideal.un[[3]])))),ideal.un[[3]][,1:19],ideal.un[[3]][,20:38],'n = 25')
plotLoc(jitter(sort(rep(x,nrow(ideal.al[[3]])))),ideal.al[[3]][,1:19],ideal.al[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(ideal.ar[[3]])))),ideal.ar[[3]][,1:19],ideal.ar[[3]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(asyml.eq[[3]])))),asyml.eq[[3]][,1:19],asyml.eq[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.un[[3]])))),asyml.un[[3]][,1:19],asyml.un[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.al[[3]])))),asyml.al[[3]][,1:19],asyml.al[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asyml.ar[[3]])))),asyml.ar[[3]][,1:19],asyml.ar[[3]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(asymr.eq[[3]])))),asymr.eq[[3]][,1:19],asymr.eq[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.un[[3]])))),asymr.un[[3]][,1:19],asymr.un[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.al[[3]])))),asymr.al[[3]][,1:19],asymr.al[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(asymr.ar[[3]])))),asymr.ar[[3]][,1:19],asymr.ar[[3]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(compr.eq[[3]])))),compr.eq[[3]][,1:19],compr.eq[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.un[[3]])))),compr.un[[3]][,1:19],compr.un[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.al[[3]])))),compr.al[[3]][,1:19],compr.al[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(compr.ar[[3]])))),compr.ar[[3]][,1:19],compr.ar[[3]][,20:38])

plotLoc(jitter(sort(rep(x,nrow(flatt.eq[[3]])))),flatt.eq[[3]][,1:19],flatt.eq[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.un[[3]])))),flatt.un[[3]][,1:19],flatt.un[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.al[[3]])))),flatt.al[[3]][,1:19],flatt.al[[3]][,20:38])
plotLoc(jitter(sort(rep(x,nrow(flatt.ar[[3]])))),flatt.ar[[3]][,1:19],flatt.ar[[3]][,20:38])
