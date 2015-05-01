plot.BML = function(grid, ifadd=TRUE){
  image(t(grid)[,nrow(grid):1], axes = FALSE, add=ifadd, col = c("white", "red", "blue"))
}

summary.BML = function(grid){
  r = nrow(grid)
  c = ncol(grid)
  size = r*c
  nRed = sum(grid==2)
  nBlue = sum(grid==1)
  d = as.data.frame(matrix(c(round(nBlue/size, digits=2), round(nRed/size, digits=2), nBlue,  nRed), nrow=2, ncol=2))
  colnames(d) = c("Density", "CarCount")
  rownames(d) = c("Blue", "Red")
  cat(paste0("This BML grid has ", r, " rows and ", c, " columns.\n"))
  cat(paste0("The size of the grid is ", size, ".\n"))
  cat("\n")
  print(d)
}
