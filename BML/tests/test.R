g = matrix(c(0,1,0,0,0,2,0,1,0,0,2,0,2,0,1,0,1,2,0,0), nrow=4, ncol=5)

fast.test = runBMLGrid(grid = g,numSteps = 2,ifPlot = FALSE, method = "fast")
slow.test = runBMLGrid(grid = g,numSteps = 2,ifPlot = FALSE, method = "slow")
simul.test = runBMLGrid(grid = g,numSteps = 2,ifPlot = FALSE, method = "simultaneous")

ans = matrix(c(1,0,0,0,2,1,0,0,0,2,0,1,0,0,0,2,0,2,1,0), nrow=4, ncol=5)

if(sum(fast.test != ans))
  stop("method fast is wrong")

if(sum(slow.test != ans))
  stop("method slow is wrong")

if(sum(simul.test != ans))
  stop("method simultaneous is wrong")

