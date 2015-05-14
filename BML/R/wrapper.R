crunBMLGrid = function(grid, numSteps){
  r = nrow(grid)
  c = ncol(grid)
  a = .C("crun1", as.integer(grid), as.integer(r), as.integer(c), as.integer(numSteps))
  matrix(a[[1]], nrow=nrow(grid))
}

