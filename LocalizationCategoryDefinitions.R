# This R script has functions defining how localization data for 
# categories are defined. Parameters can be modified, and 
# categories can be added or removed by the user if desired.
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Simulated classes are defined and simulations are completed for 
# each category.
simLocData <- function(n, lsiz){
  # Input:
  # n - number of repetitions per target speaker
  # lsiz - number of simulated subjects per category
    # also indicates length of list
  
  # Ouput: data frame of means and SDs from each simulated category
  
  # Target speaker locations
  x <- seq(-90, 90, 10)
  # Sequences of SDs for ideal and larger lateral categories 
  SDs.eq <- rep(10, length(x)) 
  SDs.un <- c(30, 30, 30, 20, 20, 20, 10, 10, 10,
              5,  10, 10, 10, 20, 20, 20, 30, 30, 30)
  SDs.al <- c(50, 50, 50, 40, 40, 40, 30, 30, 30,
              20, 10, 10, 10, 20, 20, 20, 30, 30, 30)

# ------------ Generate Data ------------ 
  # Each sub-section is separated by patterns of means. 
  # New cases can be added or adjusted here by the user.
  
  # Ideal means
  y <- SSfpl(x, -180, 180, 0, 85)
  dat.ideal.eq <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.eq, lsiz = lsiz)
  ideal.eq <- clusFormat(dat.ideal.eq)
  dat.ideal.un <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.un, lsiz = lsiz)
  ideal.un <- clusFormat(dat.ideal.un)
  dat.ideal.al <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.al, lsiz = lsiz)
  ideal.al <- clusFormat(dat.ideal.al)
  dat.ideal.ar <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = rev(SDs.al), lsiz = lsiz)
  ideal.ar <- clusFormat(dat.ideal.ar)

  # Biased left means
  y <- SSfpl(x, -50, 150, 55, 50)
  dat.asyml.eq <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.eq, lsiz = lsiz)
  asyml.eq <- clusFormat(dat.asyml.eq)
  dat.asyml.un <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.un, lsiz = lsiz)
  asyml.un <- clusFormat(dat.asyml.un)
  dat.asyml.al <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.al, lsiz = lsiz)
  asyml.al <- clusFormat(dat.asyml.al)
  dat.asyml.ar <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = rev(SDs.al), lsiz = lsiz)
  asyml.ar <- clusFormat(dat.asyml.ar)

  # Biased right means
  y <- SSfpl(x, -150, 50, -55, 50)
  dat.asymr.eq <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.eq, lsiz = lsiz)
  asymr.eq <- clusFormat(dat.asymr.eq)
  dat.asymr.un <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.un, lsiz = lsiz)
  asymr.un <- clusFormat(dat.asymr.un)
  dat.asymr.al <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.al, lsiz = lsiz)
  asymr.al <- clusFormat(dat.asymr.al)
  dat.asymr.ar <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = rev(SDs.al), lsiz = lsiz)
  asymr.ar <- clusFormat(dat.asymr.ar)

  # Compressed means
  y <- SSfpl(x, -70, 70, 0, 35)
  dat.compr.eq <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.eq, lsiz = lsiz)
  compr.eq <- clusFormat(dat.compr.eq)
  dat.compr.un <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.un, lsiz = lsiz)
  compr.un <- clusFormat(dat.compr.un)
  dat.compr.al <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.al, lsiz = lsiz)
  compr.al <- clusFormat(dat.compr.al)
  dat.compr.ar <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = rev(SDs.al), lsiz = lsiz)
  compr.ar <- clusFormat(dat.compr.ar)

  # Flat means
  y <- SSfpl(x, -15, 15, 0, 35)
  dat.flatt.eq <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.eq, lsiz = lsiz)
  flatt.eq <- clusFormat(dat.flatt.eq)
  dat.flatt.un <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.un, lsiz = lsiz)
  flatt.un <- clusFormat(dat.flatt.un)
  dat.flatt.al <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = SDs.al, lsiz = lsiz)
  flatt.al <- clusFormat(dat.flatt.al)
  dat.flatt.ar <- lapply(n, FUN = simLoc, angle = x,
            rmeans = y, rsds = rev(SDs.al), lsiz = lsiz)
  flatt.ar <- clusFormat(dat.flatt.ar)

# ------------ Reformat Data ------------ 
  # Takes data frames from each simulated class
  # and combines into one larger data frame. 
  # New classes also need to be added here if 
  # they are introduced by the user.
  
  out <- do.call('rbind',list(
    ideal.eq[[1]], ideal.un[[1]], ideal.al[[1]], ideal.ar[[1]],
    asyml.eq[[1]], asyml.un[[1]], asyml.al[[1]], asyml.ar[[1]],
    asymr.eq[[1]], asymr.un[[1]], asymr.al[[1]], asymr.ar[[1]],
    compr.eq[[1]], compr.un[[1]], compr.al[[1]], compr.ar[[1]],
    flatt.eq[[1]], flatt.un[[1]], flatt.al[[1]], flatt.ar[[1]]))
  return(out)
}
