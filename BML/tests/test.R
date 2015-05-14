library(BML)
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

grid = createBMLGrid(r = 100, c = 100, density = 0.5)
a = runBMLGrid(grid = grid,numSteps = 100,ifPlot = FALSE,method = "fast")
b = crunBMLGrid(grid=grid, numSteps = 100)

if(sum(a!=b)!=0)
  stop("c routine is wrong")
