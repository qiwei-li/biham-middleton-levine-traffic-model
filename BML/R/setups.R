# setup 
setX11 = function(r, c){
  ratio = r/c
  screen = 8/12
  if(ratio >= screen)
    x11(width=8*(c/r), height=8)
  else
    x11(width=12, height=12*(r/c)) 
}

createBMLGrid = function(r, c, ncars=c(red=0, blue=0), density=0){
  if(r<=0 | c<=0)
    stop("Error: grid dimension must be positive")
  if(density > 1 | density < 0) 
    stop("Error: density should be between 0 and 1")
  if(density){
    nred = nblue = floor((r*c*density)/2)
    totalcar = nred + nblue
  }  
  else{
    nred = ncars['red']
    nblue = ncars['blue']
    totalcar = nred + nblue
    if(totalcar > r*c | totalcar < 0) 
      stop("Error: Number of cars must be between 0 and grid size")
  }
  colors = c(rep(0, r*c-totalcar), rep(1, nred), rep(2, nblue))
  structure(matrix(sample(colors, r*c), nrow = r), class=c("BML", "matrix"))
}

runBMLGrid = function(grid, numSteps, ifPlot = FALSE, ifVelocity = FALSE, method){
  if(method == "fast"){
    moveBlue = moveBlue.fast
    moveRed = moveRed.fast
  }
  else if(method == "simultaneous"){
    moveBlue = moveBlue.simul
    moveRed = moveRed.simul
  }
  else if(method == "slow"){
    moveBlue = moveBlue.slow
    moveRed = moveRed.slow
  }
  else
    stop("Error: Invalid method")
  
  n = floor(numSteps/2)
  velocity = rep(NA, n)
  r = nrow(grid)
  c = ncol(grid)
  
  if(ifPlot){
    setX11(r, c)
    par(mar=rep(0,4))
    plot.BML(grid, ifadd=FALSE)
    for(i in 1:n){
      mb = moveBlue(grid, r, c)
      grid = mb$grid
      plot.BML(grid)
      mr = moveRed(grid, r, c)
      grid = mr$grid
      plot.BML(grid)
      velocity[i] = mb$velocity + mr$velocity
    }
    if(numSteps%%2==1){
      mb = moveBlue(grid, r, c)
      grid = mb$grid
      plot.BML(grid)
    }
    dev.off()
    if(ifVelocity){
      par(mar=rep(5,4))
      plotVelocity(grid, velocity)
    }
    grid
  }
  else{
    for(i in 1:n){
      mb = moveBlue(grid, r, c)
      grid = mb$grid
      mr = moveRed(grid, r, c)
      grid = mr$grid
      velocity[i] = mb$velocity + mr$velocity
    }
    if(numSteps%%2==1){
      mb = moveBlue(grid, r, c)
      grid = mb$grid
    }
    if(ifVelocity){
      par(mar=rep(5,4))
      plotVelocity(grid, velocity)
    }
    grid
  }  
}

plotVelocity = function(grid, velocity){
  plot(velocity, main="Number of cars moved in each step", type='l', xlab="steps", lwd=3, ylim=c(0, table(grid!=0)[2]*1.2))
  total = table(grid!=0)[2]
  abline(h = total)
  text(length(velocity)*0.8, total*1.1, "total cars") 
}